package Graph::AdjacencyMap::Heavy;

# THIS IS INTERNAL IMPLEMENTATION ONLY, NOT TO BE USED DIRECTLY.
# THE INTERFACE IS HARD TO USE AND GOING TO STAY THAT WAY AND
# ALMOST GUARANTEED TO CHANGE OR GO AWAY IN FUTURE RELEASES.

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

use Graph::AdjacencyMap qw(:flags :fields);
use base 'Graph::AdjacencyMap';

require overload; # for de-overloading

sub stringify {
    my $m = shift;
    my @rows;
    my $a = $m->[ _arity ];
    my $s = $m->[ _s ];
    my $f = $m->[ _f ];
    my $hyper = $f & _HYPER;
    my $multi = $f & _MULTI;
    my @p = map $_->[0], sort _s_sort map [$_,"@$_"], $m->paths; # use the Schwartz
    if ($a == 2) {
	my (%p, %s);
	for my $t (@p) {
	    my ($u, $v) = @$t;
	    $p{$u} = $s{$v} = 1;
	}
	my @s = sort keys %s;
	@rows = [ 'to:', @s ];
	for my $u (sort keys %p) {
	    my @r = $u;
	    for my $v (@s) {
		my $v = $s->{$u}{$v};
		push @r, $m->_dumper(ref $v ? $v->[-1] : defined $v ? 1 : '');
	    }
	    push @rows, \@r;
	}
    } elsif ($a == 1) {
	for my $v (@p) {
	    my @r = $v->[0];
	    my ($text) = $m->get_ids_by_paths([ $v ]);
	    my $attrs = $multi
		? (( $m->__get_path_node( @$v ) )[0] || [])->[-1]
		: $m->_get_path_attrs(@$v);
	    $text .= ",".$m->_dumper($attrs) if defined $attrs;
	    push @r, $text;
	    push @rows, \@r;
	}
    }
    $m->SUPER::stringify . join '',
	map "$_\n",
	map join(' ', map sprintf('%4s', $_), @$_),
	@rows;
}

# because in BLOCK mode, $a is 1 while $b is right - probable perl bug
sub _s_sort { $a->[1] cmp $b->[1] }

sub __set_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    my $id = pop if $f & _MULTI;
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->SUPER::stringify,
	scalar @_ - 1, $m->[ _arity ]) if @_ - 1 != $m->[ _arity ] && !($f & _HYPER);
    my $p;
    $p = ($f & _HYPER) ?
	(( $m->[ _s ] ||= [ ] )->[ @_-1 ] ||= { }) :
	(  $m->[ _s ]                     ||= { });
    my @p = $p;
    my @k;
    my @a = ($f & _UNORD) ? sort @_[1..$#_] : @_[1..$#_];
    push @_, $id if $f & _MULTI;
    while (@a) {
	my $k = shift @a;
	my $q = ref $k && ($f & _REF) && overload::Method($k, '""') ? overload::StrVal($k) : $k;
	if (@a) {
	    $p = $p->{ $q } ||= {};
	    push @p, $p;
	}
	push @k, $q;
    }
    return (\@p, \@k);
}

sub __set_path_node {
    my ($m, $p, $l, @args) = @_;
    my $f = $m->[ _f ] ;
    my $id = pop @args if ($f & _MULTI);
    unless (exists $p->[-1]->{ $l }) {
	my $i = $m->_new_node( \$p->[-1]->{ $l }, $id );
	die "undefined index" if !defined $i;
	$m->[ _i ]->[ $i ] = \@args;
	return defined $id ? ($id eq _GEN_ID ? $$id : $id) : $i;
    } else {
	return $m->_inc_node( \$p->[-1]->{ $l }, $id );
    }
}

sub __has_path {
    my ($m) = @_;
    my $f = $m->[ _f ];
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->SUPER::stringify,
	@_ - 1, $m->[ _arity ]) if @_ != $m->[ _arity ] + 1 && !($f & _HYPER);
    if (@_ > 2 && ($f & _UNORDUNIQ)) {
	if (($f & _UNORDUNIQ) == _UNORD && @_ > 2) { @_ = ($_[0], sort @_[1..$#_]) }
	else { &Graph::AdjacencyMap::__arg }
    }
    return if !defined(my $p = $m->[ _s ]);
    return if ($f & _HYPER) and !defined($p = $p->[ @_ - 1 ]);
    my @p = $p;
    my @k;
    my @a = @_[1..$#_];
    @a = map ref() && overload::Method($_, '""') ? overload::StrVal($_) : $_, @a if $f & _REF;
    while (@a) {
	my $k = shift @a;
	if (@a) {
	    return unless defined($p = $p->{ $k });
	    push @p, $p;
	}
	push @k, $k;
    }
    return (\@p, \@k);
}

sub _get_path_node {
    my $m = $_[0];
    my $f = $m->[ _f ];
    goto &{ $m->can('__get_path_node') } # Slow path
	if !($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ)));
    @_ = ($m, sort @_[1..$#_]) if $f & _UNORD;
    return unless exists $m->[ _s ]->{ $_[1] };
    my $p = [ $m->[ _s ], $m->[ _s ]->{ $_[1] } ];
    my $l = $_[2];
    exists $p->[-1]->{ $l } ? ( $p->[-1]->{ $l }, $p, [ @_[1,2] ], $l ) : ();
}

sub get_ids_by_paths {
    my ($m, $list) = @_;
    my ($n, $f, $a, $i, $s) = @$m;
    my $unord = $a > 1 && ($f & _UNORD);
    return map { # Fast path
	my @p = @$_;
	@p = sort @p if $unord;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	defined $this_s ? $this_s : ();
    } @$list if $a == 2 && !($f & (_HYPER|_REF|_UNIQ));
    my @n;
    map !(@n = $m->_get_path_node(@$_)) ? () : ref $n[0] ? $n[0]->[ _ni ] : $n[0], @$list;
}

sub _get_path_count {
    my $m = $_[0];
    return undef unless my ($n) = &_get_path_node;
    my $f = $m->[ _f ];
    return
	($f & _COUNT) ? $n->[ _nc ] :
	($f & _MULTI) ? scalar keys %{ $n->[ _nm ] } : 1;
}

sub get_paths_by_ids {
    my ($m, $list) = @_;
    my $i = $m->[ _i ];
    map [ map {
	my $p = defined $_ ? $i->[ $_ ] : undef;
	my @v = defined $p ? @$p : ( );
	@v == 1 ? $v[0] : \@v
    } @$_ ], @$list;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] > 1; # arity > 1, all integers, no names
    my ($n, $f, $a, $i, $s, $p) = @$m;
    return 0 unless exists $s->{ $from };
    $i->[ $s->{ $from }[0] ][0] = $to;
    $s->{ $to } = delete $s->{ $from };
    return 1;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

1;
__END__
