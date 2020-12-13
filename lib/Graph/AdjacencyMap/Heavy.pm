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
    my @p = map ref()?$_:[$_], $m->paths; # normalise to all array-refs
    @p = map $_->[0], sort _s_sort map [$_,"@$_"], @p; # use the Schwartz
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
	    my @r = $hyper ? '['.join(',', @$v).']' : $v->[0];
	    my ($text) = $m->get_ids_by_paths([ $v ]);
	    my $attrs = $multi
		? (( $m->__get_path_node( @$v ) )[1] || [])->[-1]
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
	    return unless $p;
	    push @p, $p;
	}
	push @k, $q;
    }
    return (\@p, \@k);
}

sub __set_path_node {
    my ($m, $p, $l) = splice @_, 0, 3;
    my $f = $m->[ _f ] ;
    my $id = pop if ($f & _MULTI);
    unless (exists $p->[-1]->{ $l }) {
	my $i = $m->_new_node( \$p->[-1]->{ $l }, $id );
	die "undefined index" if !defined $i;
	$m->[ _i ]->[ $i ] = [ @_ ];
	return defined $id ? ($id eq _GEN_ID ? $$id : $id) : $i;
    } else {
	return $m->_inc_node( \$p->[-1]->{ $l }, $id );
    }
}

sub set_path {
    my ($m) = @_;
    my $f = $m->[ _f ];
    return if @_ == 1 && !($f & _HYPER);
    if (@_ > 2 && ($f & _UNORDUNIQ)) {
	if (($f & _UNORDUNIQ) == _UNORD && @_ == 3) { @_ = ($_[0], sort @_[1..$#_]) }
	else { &Graph::AdjacencyMap::__arg }
    }
    my ($p, $k) = &__set_path;
    return unless defined $p && defined $k;
    my $l = defined $k->[-1] ? $k->[-1] : "";
    return $m->__set_path_node( $p, $l, @_[1..$#_] );
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
    my $p = $m->[ _s ];
    return unless defined $p;
    $p = $p->[ @_ - 1 ] if ($f & _HYPER);
    return unless defined $p;
    my @p = $p;
    my @k;
    my @a = @_[1..$#_];
    while (@a) {
	my $k = shift @a;
	my $q = ref $k && ($f & _REF) && overload::Method($k, '""') ? overload::StrVal($k) : $k;
	if (@a) {
	    $p = $p->{ $q };
	    return unless defined $p;
	    push @p, $p;
	}
	push @k, $q;
    }
    return (\@p, \@k);
}

sub has_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    my ($p, $k) = &__has_path;
    return unless defined $p && defined $k;
    return exists $p->[-1]->{ defined $k->[-1] ? $k->[-1] : "" };
}

sub _get_path_node {
    my $m = $_[0];
    my $f = $m->[ _f ];
    if ($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ))) { # Fast path.
	@_ = ($m, sort @_[1..$#_]) if $f & _UNORD;
	return unless exists $m->[ _s ]->{ $_[1] };
	my $p = [ $m->[ _s ], $m->[ _s ]->{ $_[1] } ];
	my $k = [ $_[1], $_[2] ];
	my $l = $_[2];
	return ( exists $p->[-1]->{ $l }, $p->[-1]->{ $l }, $p, $k, $l );
    }
    goto &{ $m->can('__get_path_node') };
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
    map {
	my ($e, $n) = $m->_get_path_node(@$_);
	!$e ? () : ref $n ? $n->[ _ni ] : $n;
    } @$list;
}

sub _get_path_count {
    my $m = $_[0];
    my ($e, $n) = &_get_path_node;
    return undef unless $e && defined $n;
    my $f = $m->[ _f ];
    return
	($f & _COUNT) ? $n->[ _nc ] :
	($f & _MULTI) ? scalar keys %{ $n->[ _nm ] } : 1;
}

sub __attr {
    my ($m) = @_;
    return if @_ < 3;
    my $f = $m->[ _f ];
    return if !($f & _UNORDUNIQ);
    goto &Graph::AdjacencyMap::__arg if ($f & _UNORDUNIQ) != _UNORD;
    @_ = ($_[0], sort @_[1..$#_]);
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

sub del_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    my ($e, $n, $p, $k, $l) = &{ $m->can('__get_path_node') };
    return unless $e;
    my $c = ($f & _COUNT) ? --$n->[ _nc ] : 0;
    if ($c == 0) {
	delete $m->[ _i ]->[ ref $n ? $n->[ _ni ] : $n ];
	delete $p->[-1]->{ $l };
	while (@$p && @$k && keys %{ $p->[-1]->{ $k->[-1] } } == 0) {
	    delete $p->[-1]->{ $k->[-1] };
	    pop @$p;
	    pop @$k;
	}
    }
    return 1;
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
    my $m = shift;
    return grep defined, @{ $m->[ _i ] } if defined $m->[ _i ];
    wantarray ? ( ) : 0;
}

1;
__END__
