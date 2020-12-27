package Graph::AdjacencyMap;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

my (@FLAGS, %FLAG_COMBOS, %FLAG2I);
BEGIN {
    @FLAGS = qw(_COUNT _MULTI _HYPER _UNORD _UNIQ _REF _UNIONFIND _LIGHT _STR);
    %FLAG_COMBOS = (
	_COUNTMULTI => [qw(_COUNT _MULTI)],
	_UNORDUNIQ => [qw(_UNORD _UNIQ)],
	_REFSTR => [qw(_REF _STR)],
    );
    for my $i (0..$#FLAGS) {
	my $n = $FLAGS[$i];
	my $f = 1 << $i;
	$FLAG2I{$n} = $f;
	no strict 'refs';
	*$n = sub () { $f };
	*{"_is$n"} = sub { $_[0]->[ 1 ] & $f }; # 1 = _f
    }
    for my $k (keys %FLAG_COMBOS) {
	my $f = 0;
	$f |= $_ for map $FLAG2I{$_}, @{ $FLAG_COMBOS{$k} };
	no strict 'refs';
	*$k = sub () { return $f }; # return to dodge pointless 5.22 stricture
	*{"_is$k"} = sub { $_[0]->[ 1 ] & $f }; # 1 = _f
    }
}

require Exporter;
use vars qw(@ISA @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);
%EXPORT_TAGS =
    (flags =>  [@FLAGS, keys %FLAG_COMBOS, qw(_GEN_ID)],
     fields => [qw(_n _f _arity _i _s _attr _u _ni _nc _na _nm)]);
@EXPORT_OK = map @$_, values %EXPORT_TAGS;

my $_GEN_ID = 0;

sub _GEN_ID () { \$_GEN_ID }

sub _ni () { 0 } # Node index.
sub _nc () { 1 } # Node count.
sub _na () { 2 } # Node attributes.
sub _nm () { 3 } # Node map.

sub _n () { 0 } # Next id.
sub _f () { 1 } # Flags.
sub _arity () { 2 } # Arity.
sub _i () { 3 } # Index to path.
sub _s () { 4 } # Successors / Path to Index.
sub _attr () { 5 } # attributes (AdjacencyMap::Light)

sub stringify {
    my $m = shift;
    my @rows;
    my $a = $m->[ _arity ];
    my $s = $m->[ _s ];
    my $f = $m->[ _f ];
    my $hyper = $f & _HYPER;
    my $multi = $f & _MULTI;
    my @p = map $_->[0], sort _s_sort map [$_,"@$_"], $m->paths; # use the Schwartz
    if ($a == 2 and !$hyper) {
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
		my $text = $m->has_path($u, $v) ? 1 : '';
		my $attrs = $multi
		    ? (( $m->__get_path_node( $u, $v ) )[0] || [])->[-1]
		    : $m->_get_path_attrs( $u, $v )
		    if $text;
		$text = $m->_dumper($attrs) if defined $attrs;
		push @r, $text;
	    }
	    push @rows, \@r;
	}
    } else {
	for my $v (@p) {
	    my @r = $a == 1 ? $v->[0] : '[' . join(' ', @$v) . ']';
	    my ($text) = $m->get_ids_by_paths([ $v ]);
	    my $attrs = $multi
		? (( $m->__get_path_node( @$v ) )[0] || [])->[-1]
		: $m->_get_path_attrs(@$v);
	    $text .= ",".$m->_dumper($attrs) if defined $attrs;
	    push @r, $text;
	    push @rows, \@r;
	}
    }
    join '',
	map "$_\n",
	"@{[ref $m]} arity=@{[$m->[ _arity ]]} flags: @{[_stringify_fields($m->[ _f ])]}",
	map join(' ', map sprintf('%4s', $_), @$_),
	@rows;
}

# because in BLOCK mode, $a is 1 while $b is right - probable perl bug
sub _s_sort { $a->[1] cmp $b->[1] }

sub _stringify_fields {
    return '0' if !$_[0];
    join '|', grep $_[0] & $FLAG2I{$_}, @FLAGS;
}

sub _dumper {
    my ($m, $got) = @_;
    return $got if defined $got and !ref $got;
    require Data::Dumper;
    my $dumper = Data::Dumper->new([$got]);
    $dumper->Indent(0)->Terse(1);
    $dumper->Sortkeys(1) if $dumper->can("Sortkeys");
    $dumper->Dump;
}

sub _new {
    my ($class, $flags, $arity, @extra) = @_;
    bless [ 0, $flags, $arity, [], ($flags & _HYPER ? [] : {}), @extra ], $class;
}

sub _ids {
    my $m = shift;
    return $m->[ _i ];
}

sub has_paths {
    @{ $_[0]->[ _i ] || [] };
}

sub __has_path {
    &__arg;
    my ($f, $s, @k) = (@{ $_[0] }[ _f, _s ], @_[1..$#_]);
    @k = map ref() ? __strval($_, $f) : $_, @k if $f & _REF;
    return if !defined($s = ($f & _HYPER) ? $s->[ @k ] : $s);
    ([$s, map defined($s = $s->{$_}) ? $s : return, @k[0..$#k-1]], [@k?@k:'']);
}

sub set_path {
    &__arg;
    return if @_ < 2 && !($_[0]->[ _f ] & _HYPER);
    goto &set_path_by_multi_id;
}

sub __set_path {
    my $inc_if_exists = pop;
    my $id = pop if my $is_multi = (my $f = $_[0]->[ _f ]) & _MULTI;
    &__arg;
    my ($m, @a) = @_;
    push @_, $id if $is_multi;
    my $is_countmulti = $f & _COUNTMULTI;
    my @path = @a;
    @a = map ref() ? __strval($_, $f) : $_, @a if $f & _REF;
    my $p = (($f & _HYPER) ? $m->[ _s ]->[ @a ] : $m->[ _s ]) ||= { };
    $p = $p->{ $_ } ||= {} for @a[0..$#a-1];
    my $l = ( @a ? @a : '' )[-1];
    if (exists $p->{ $l }) {
	return ($p, $l) if !($inc_if_exists and $is_countmulti);
	my $nm = (my $n = $p->{ $l })->[ _nm ];
	$n->[ _nc ]++, return ($p, $l) if !$is_multi;
	if ($id eq _GEN_ID) {
	    $n->[ _nc ]++ while exists $nm->{ $n->[ _nc ] };
	    $id = $n->[ _nc ];
	}
	$nm->{ $id } = { };
	return ($p, $l, $id);
    }
    $m->[ _i ][ my $i = $m->[ _n ]++ ] = \@path;
    return ($p, $l, $p->{ $l } = $i) if !$is_countmulti;
    $p->{ $l } = $is_multi
	? [$i, 0, undef, { ($id = ($id eq _GEN_ID) ? 0 : $id) => {} }]
	: [$i, 1];
    ($p, $l, $is_multi ? $id : $i);
}

sub _set_path_attr_common {
    my $f = $_[0]->[ _f ];
    my $id   = pop if ($f & _MULTI);
    &__arg;
    push @_, $id if ($f & _MULTI);
    push @_, 0;
    my ($p, $l) = &__set_path;
    return \$p->{ $l }->[ _nm ]->{ $id } if ($f & _MULTI);
    $p->{ $l } = [$p->{ $l }, 1] if !ref $p->{ $l }; # Extend if simple id node
    return \$p->{ $l }->[ _na ];
}

sub set_path_by_multi_id {
    push @_, 1;
    (&__set_path)[2];
}

sub paths_non_existing {
    my ($m, $list) = @_;
    grep !$m->has_path(@$_), @$list;
}

sub has_path {
    my $m = $_[0];
    return unless my ($p, $k) = &__has_path;
    return exists $p->[-1]->{ $k->[-1] };
}

sub __get_path_node {
    my ($p, $k);
    &__arg;
    my $f = $_[0]->[ _f ];
    my ($m) = @_;
    if ($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ|_MULTI))) { # Fast path.
	my $s = $m->[ _s ];
	return unless exists $s->{ $_[1] };
	$p = [ $s, $s->{ $_[1] } ];
	$k = [ $_[1], $_[2] ];
    } else {
	return unless ($p, $k) = &__has_path;
    }
    exists $p->[-1]->{ $k->[-1] } ? ( $p->[-1]->{ $k->[-1] }, $p, $k ) : ();
}

sub has_path_by_multi_id {
    my $m = $_[0];
    my $id = pop;
    return undef unless my ($n) = &__get_path_node;
    return exists $n->[ _nm ]->{ $id };
}

sub del_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    return unless my ($n, $p, $k) = &__get_path_node;
    return 1 if 0 != (($f & _COUNT) ? --$n->[ _nc ] : 0);
    delete $m->[ _i ]->[ ref $n ? $n->[ _ni ] : $n ];
    delete $p->[-1]->{ $k->[-1] };
    while (@$p && @$k && keys %{ $p->[-1]->{ $k->[-1] } } == 0) {
	delete $p->[-1]->{ $k->[-1] };
	pop @$p;
	pop @$k;
    }
    return 1;
}

sub del_path_by_multi_id {
    my $m = $_[0];
    my $id = pop;
    return unless my ($n, $p, $k) = &__get_path_node;
    delete $n->[ _nm ]->{ $id };
    return 1 if keys %{ $n->[ _nm ] };
    delete $m->[ _i ]->[ $n->[ _ni ] ];
    delete $p->[-1]->{ $k->[-1] };
    while (@$p && @$k && keys %{ $p->[-1]->{ $k->[-1] } } == 0) {
	delete $p->[-1]->{ $k->[-1] };
	pop @$p;
	pop @$k;
    }
    return 1;
}

sub get_multi_ids {
    my $f = $_[0]->[ _f ];
    return unless ($f & _MULTI);
    return unless my ($n) = &__get_path_node;
    keys %{ $n->[ _nm ] };
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] > 1; # arity > 1, all integers, no names
    return unless my ($n, $p, $k) = $m->__get_path_node( $from );
    $m->[ _i ][ ref $n ? $n->[ _ni ] : $n ] = [ $to ];
    $p->[ -1 ]{ $to } = delete $p->[-1]{ $k->[-1] };
    return 1;
}

sub _get_path_attrs {
    my $f = $_[0]->[ _f ];
    my $id = pop if ($f & _MULTI);
    &__arg;
    my ($m) = @_;
    if (($f & _MULTI)) {
	return unless my ($p, $k) = &__has_path;
	push @_, $id;
	$p->[-1]->{ $k->[-1] }->[ _nm ]->{ $id };
    } else {
	return unless my ($n) = &__get_path_node;
	return $n->[ _na ] if ref $n && $#$n == _na;
	return;
    }
}

sub _get_path_node {
    my $m = $_[0];
    my $f = $m->[ _f ];
    goto &__get_path_node # Slow path
	if !($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ)));
    &__arg;
    return unless exists $m->[ _s ]->{ $_[1] };
    my $p = [ $m->[ _s ], $m->[ _s ]->{ $_[1] } ];
    my $l = $_[2];
    exists $p->[-1]->{ $l } ? ( $p->[-1]->{ $l }, $p, [ @_[1,2] ] ) : ();
}

sub _del_path_attrs {
    my $f = $_[0]->[ _f ];
    my $id = pop if ($f & _MULTI);
    &__arg;
    my ($m) = @_;
    if ($f & _MULTI) {
	return unless my ($p, $k) = &__has_path;
	push @_, $id;
	$p->[-1]->{ $k->[-1] }->[ _nm ]->{ $id } = undef;
	return 1;
    } else {
	return undef unless my ($n) = &__get_path_node;
	return 0 if !ref $n;
	my $e = _na == $#$n && keys %{ $n->[ _na ] } ? 1 : 0;
	$#$n = _na - 1;
	return $e;
    }
}

sub get_paths_by_ids {
    my ($m, $list) = @_;
    my $i = $m->[ _i ];
    map [ map $i->[ $_ ], @$_ ], @$list;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

sub get_ids_by_paths {
    my ($m, $list) = @_;
    my ($n, $f, $a, $i, $s) = @$m;
    return map { # Fast path
	my @p = @$_;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	defined $this_s ? ref $this_s ? $this_s->[ _ni ] : $this_s : ();
    } @$list if $a == 2 && !($f & (_HYPER|_REF|_UNIQ));
    my @n;
    map !(@n = $m->_get_path_node(@$_)) ? () : ref $n[0] ? $n[0]->[ _ni ] : $n[0], @$list;
}

sub _has_path_attrs {
    return undef unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    keys %$attrs ? 1 : 0;
}

sub _set_path_attrs {
    my $f = $_[0]->[ _f ];
    my $attrs = pop;
    my $handle = &{ $_[0]->can('_set_path_attr_common') };
    $$handle = $attrs;
}

sub _has_path_attr {
    my $attr = pop;
    return undef unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    exists $attrs->{$attr};
}

sub _set_path_attr {
    my $f = $_[0]->[ _f ];
    my $val  = pop;
    my $attr = pop;
    my $handle = &{ $_[0]->can('_set_path_attr_common') };
    return $$handle->{ $attr } = $val;
}

sub __strval {
    my ($k, $f) = @_;
    return $k unless ref $k && ($f & _REF);
    require overload;
    (($f & _STR) xor overload::Method($k, '""')) ? overload::StrVal($k) : $k;
}

sub _get_path_attr {
    my $attr = pop;
    return undef unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    $attrs->{$attr};
}

sub _get_path_attr_names {
    return unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    keys %$attrs;
}

sub _get_path_attr_values {
    return unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    values %$attrs;
}

sub _get_path_count {
    my $m = $_[0];
    return undef unless my ($n) = &_get_path_node;
    my $f = $m->[ _f ];
    return
        ($f & _COUNT) ? $n->[ _nc ] :
        ($f & _MULTI) ? scalar keys %{ $n->[ _nm ] } : 1;
}

sub _del_path_attr {
    my $attr = pop;
    return unless defined(my $attrs = &{ $_[0]->can('_get_path_attrs') });
    return 0 unless exists $attrs->{$attr};
    delete $attrs->{$attr};
    return 1 if keys %$attrs;
    &_del_path_attrs;
    1;
}

sub __arg {
    my ($m, @a) = @_;
    return if @_ < 3; # nothing to do if 1 or 0 passed args
    my $f = $m->[ _f ];
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->stringify,
		  @a, $m->[ _arity ])
        if !($f & _HYPER) and @a != $m->[ _arity ];
    return if !($f & _UNIQ);
    my %u;
    @a = grep !$u{$_}++, @a if $f & _UNIQ;
    @_ = ($_[0], @a);
}

1;
__END__
=pod

=head1 NAME

Graph::AdjacencyMap - map of graph vertices or edges

=head1 SYNOPSIS

    Internal.

=head1 DESCRIPTION

B<This module is meant for internal use by the Graph module.>

=head1 OBJECT METHODS

=head2 del_path(@id)

Delete a Map path by ids.

=head2 del_path_by_multi_id($id)

Delete a Map path by a multi(vertex) id.

=head2 get_multi_ids

Return the multi ids.

=head2 has_path(@id)

Return true if the Map has the path by ids, false if not.

=head2 has_paths

Return true if the Map has any paths, false if not.

=head2 has_path_by_multi_id($id)

Return true if the Map has the path by a multi(vertex) id, false if not.

=head2 paths

Return all the paths of the Map.

=head2 set_path(@id)

Set the path by @ids.

=head2 set_path_by_multi_id

Set the path in the Map by the multi id.

=head2 get_paths_by_ids

Given an array-ref of array-refs of vertex IDs, returns a list of
array-refs of paths.

=head2 paths_non_existing

    @non_existing = $m->paths_non_existing(\@paths);

Given an array-ref of array-refs with paths, returns a list of
non-existing paths.

=head2 get_ids_by_paths

    @ids = $m->get_ids_by_paths(\@paths);

Given an array-ref of array-refs with paths, returns a list of IDs of
existing paths (non-existing ones will not be represented).

=head2 rename_path($from, $to)

Rename the path.

=head2 stringify

Return a string describing the object in a human-friendly(ish) way.

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
