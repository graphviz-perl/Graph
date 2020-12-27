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
     fields => [qw(_n _f _arity _i _s _attr _u _ni _nc _na)]);
@EXPORT_OK = map @$_, values %EXPORT_TAGS;

my $_GEN_ID = 0;

sub _GEN_ID () { \$_GEN_ID }

sub _ni () { 0 } # Node index.
sub _nc () { 1 } # Node count.
sub _na () { 2 } # Node attributes - two-level for MULTI

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
		my $text = $m->has_path([$u, $v]) ? 1 : '';
		my $attrs = $multi
		    ? (( $m->__get_path_node([$u, $v]) )[0] || [])->[-1]
		    : $m->_get_path_attrs([$u, $v])
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
		? (( $m->__get_path_node( $v ) )[0] || [])->[-1]
		: $m->_get_path_attrs($v);
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
    my (undef, $got) = @_;
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
    $_[0]->[ _i ];
}

sub has_paths {
    @{ $_[0]->[ _i ] || [] };
}

sub __has_path {
    &__arg;
    my ($f, $s, @k) = (@{ $_[0] }[ _f, _s ], @{ $_[1] });
    @k = map ref() ? __strval($_, $f) : $_, @k if $f & _REF;
    return if !defined($s = ($f & _HYPER) ? $s->[ @k ] : $s);
    ([$s, map defined($s = $s->{$_}) ? $s : return, @k[0..$#k-1]], [@k?@k:'']);
}

sub set_path {
    &__arg;
    return if !@{ $_[1] } && !($_[0]->[ _f ] & _HYPER);
    goto &set_path_by_multi_id;
}

sub __set_path {
    my $inc_if_exists = pop;
    &__arg;
    my ($f, $map_i, $s, $m, $id, @a) = (@{ $_[0] }[ _f, _i, _s ], @_[0, 2], @{ $_[1] });
    my ($is_multi, $is_countmulti) = ($f & _MULTI, $f & _COUNTMULTI);
    my @path = @a;
    @a = map ref() ? __strval($_, $f) : $_, @a if $f & _REF;
    my $p = (($f & _HYPER) ? $s->[ @a ] : $s) ||= { };
    $p = $p->{ $_ } ||= {} for @a[0..$#a-1];
    my $l = ( @a ? @a : '' )[-1];
    if (exists $p->{ $l }) {
	return ($p, $l) if !($inc_if_exists and $is_countmulti);
	my $na = (my $n = $p->{ $l })->[ _na ];
	$n->[ _nc ]++, return ($p, $l) if !$is_multi;
	if ($id eq _GEN_ID) {
	    $n->[ _nc ]++ while exists $na->{ $n->[ _nc ] };
	    $id = $n->[ _nc ];
	}
	$na->{ $id } = { };
	return ($p, $l, $id);
    }
    $map_i->[ my $i = $m->[ _n ]++ ] = \@path;
    return ($p, $l, $p->{ $l } = $i) if !$is_countmulti;
    $p->{ $l } = $is_multi
	? [$i, 0, { ($id = ($id eq _GEN_ID) ? 0 : $id) => {} }]
	: [$i, 1];
    ($p, $l, $is_multi ? $id : $i);
}

sub _set_path_attr_common {
    &__arg;
    push @_, 0;
    my ($p, $l) = &__set_path;
    return \$p->{ $l }->[ _na ]->{ $_[2] } if ($_[0]->[ _f ] & _MULTI);
    $p->{ $l } = [$p->{ $l }, 1] if !ref $p->{ $l }; # Extend if simple id node
    return \$p->{ $l }->[ _na ];
}

sub set_path_by_multi_id {
    push @_, 1;
    (&__set_path)[2];
}

sub paths_non_existing {
    my ($m, $list) = @_;
    grep !$m->has_path($_), @$list;
}

sub has_path {
    return unless my ($p, $k) = &__has_path;
    return exists $p->[-1]->{ $k->[-1] };
}

sub __get_path_node {
    my ($p, $k);
    &__arg;
    my ($f, @a) = ((my $m = $_[0])->[ _f ], @{ $_[1] });
    if ($m->[ _arity ] == 2 && @a == 2 && !($f & (_HYPER|_REF|_UNIQ|_MULTI))) { # Fast path.
	my $s = $m->[ _s ];
	return unless exists $s->{ $a[0] };
	$p = [ $s, $s->{ $a[0] } ];
	$k = \@a;
    } else {
	return unless ($p, $k) = &__has_path;
    }
    exists $p->[-1]->{ $k->[-1] } ? ( $p->[-1]->{ $k->[-1] }, $p, $k ) : ();
}

sub has_path_by_multi_id {
    return undef unless my ($n) = &__get_path_node;
    return exists $n->[ _na ]->{ $_[2] };
}

sub _sequence_del {
    my ($map_i, $id, $p, $k) = @_;
    delete $map_i->[ $id ];
    delete $p->[-1]->{ $k->[-1] };
    while (@$p && @$k && keys %{ $p->[-1]->{ $k->[-1] } } == 0) {
	delete $p->[-1]->{ $k->[-1] };
	pop @$p;
	pop @$k;
    }
    return 1;
}

sub del_path {
    return unless my ($n, $p, $k) = &__get_path_node;
    return 1 if &_is_COUNT and --$n->[ _nc ] > 0;
    _sequence_del($_[0]->[ _i ], ref $n ? $n->[ _ni ] : $n, $p, $k);
}

sub del_path_by_multi_id {
    return unless my ($n, $p, $k) = &__get_path_node;
    delete $n->[ _na ]->{ $_[2] };
    return 1 if keys %{ $n->[ _na ] };
    _sequence_del($_[0]->[ _i ], $n->[ _ni ], $p, $k);
}

sub get_multi_ids {
    return unless ($_[0]->[ _f ] & _MULTI) and my ($n) = &__get_path_node;
    keys %{ $n->[ _na ] };
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] > 1; # arity > 1, all integers, no names
    return unless my ($n, $p, $k) = $m->__get_path_node([$from]);
    $m->[ _i ][ ref $n ? $n->[ _ni ] : $n ] = [ $to ];
    $p->[ -1 ]{ $to } = delete $p->[-1]{ $k->[-1] };
    return 1;
}

sub _get_path_attrs {
    &__arg;
    return unless my ($n) = &__get_path_node;
    return $n->[ _na ]->{ $_[2] } if ($_[0]->[ _f ] & _MULTI);
    return $n->[ _na ] if ref $n && $#$n == _na;
}

sub _get_path_node {
    my ($f, $a, $s, @a) = (@{ $_[0] }[ _f, _arity, _s ], @{ $_[1] });
    goto &__get_path_node # Slow path
	if !($a == 2 && @a == 2 && !($f & (_HYPER|_REF|_UNIQ)));
    &__arg;
    return unless exists $s->{ $a[0] };
    my $p = [ $s, $s->{ $a[0] } ];
    my $l = $a[1];
    exists $p->[-1]->{ $l } ? ( $p->[-1]->{ $l }, $p, \@a ) : ();
}

sub _del_path_attrs {
    my $id = $_[2] if my $is_multi = ((my $f = $_[0]->[ _f ]) & _MULTI);
    &__arg;
    return unless my ($n) = &__get_path_node;
    return $n->[ _na ]->{ $id } = undef, 1 if $is_multi;
    return 0 if !ref $n;
    my $e = _na == $#$n && keys %{ $n->[ _na ] } ? 1 : 0;
    $#$n = _na - 1;
    return $e;
}

sub get_paths_by_ids {
    my ($i, $m, $list) = ( @{ $_[0] }[ _i ], @_ );
    map [ map $i->[ $_ ], @$_ ], @$list;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

sub get_ids_by_paths {
    my ($f, $a, $s, $m, $list) = ( @{ $_[0] }[ _f, _arity, _s ], @_ );
    return map { # Fast path
	my @p = @$_;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	defined $this_s ? ref $this_s ? $this_s->[ _ni ] : $this_s : ();
    } @$list if $a == 2 && !($f & (_HYPER|_REF|_UNIQ));
    my @n;
    map !(@n = $m->_get_path_node($_)) ? () : ref $n[0] ? $n[0]->[ _ni ] : $n[0], @$list;
}

sub _has_path_attrs {
    keys %{ &{ $_[0]->can('_get_path_attrs') } || return undef } ? 1 : 0;
}

sub _set_path_attrs {
    ${ &{ $_[0]->can('_set_path_attr_common') } } = $_[-1];
}

sub _has_path_attr {
    exists(( &{ $_[0]->can('_get_path_attrs') } || return )->{ $_[-1] });
}

sub _set_path_attr {
    ${ &{ $_[0]->can('_set_path_attr_common') } }->{ $_[-2] } = $_[-1];
}

sub __strval {
    my ($k, $f) = @_;
    return $k unless ref $k && ($f & _REF);
    require overload;
    (($f & _STR) xor overload::Method($k, '""')) ? overload::StrVal($k) : $k;
}

sub _get_path_attr {
    ( &{ $_[0]->can('_get_path_attrs') } || return )->{ $_[-1] };
}

sub _get_path_attr_names {
    keys %{ &{ $_[0]->can('_get_path_attrs') } || return };
}

sub _get_path_attr_values {
    values %{ &{ $_[0]->can('_get_path_attrs') } || return };
}

sub _get_path_count {
    return undef unless my ($n) = &_get_path_node;
    my $f = $_[0]->[ _f ];
    return
        ($f & _COUNT) ? $n->[ _nc ] :
        ($f & _MULTI) ? scalar keys %{ $n->[ _na ] } : 1;
}

sub _del_path_attr {
    return unless my $attrs = &{ $_[0]->can('_get_path_attrs') };
    return 0 unless exists $attrs->{ my $attr = $_[-1] };
    delete $attrs->{$attr};
    return 1 if keys %$attrs;
    &_del_path_attrs;
    1;
}

sub __arg {
    my ($f, @a) = ((my $m = $_[0])->[ _f ], @{ $_[1] });
    return if @a < 2; # nothing to do if 1 or 0-length seq
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->stringify,
		  scalar @a, $m->[ _arity ])
        if !($f & _HYPER) and @a != $m->[ _arity ];
    return if !($f & _UNIQ);
    my %u;
    @a = grep !$u{$_}++, @a;
    splice @_, 1, 1, \@a;
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

=head2 del_path(\@seq)

Delete a Map path.

=head2 del_path_by_multi_id(\@seq, $id)

Delete a Map path by a multi(vertex) id.

=head2 get_multi_ids(\@seq)

Return the multi ids.

=head2 has_path(\@seq)

Return true if the Map has the path, false if not.

=head2 has_paths

Return true if the Map has any paths, false if not.

=head2 has_path_by_multi_id(\@seq, $id)

Return true if the Map has the path by a multi(vertex) id, false if not.

=head2 paths

Return all the paths of the Map.

=head2 set_path(\@seq)

Set the path by @ids.

=head2 set_path_by_multi_id(\@seq, $id)

Set the path in the Map by the multi id.

=head2 get_paths_by_ids([ \@idlist1, \@idlist2... ])

Given an array-ref of array-refs of vertex IDs, returns a list of
array-refs of paths.

=head2 paths_non_existing

    @non_existing = $m->paths_non_existing([ \@seq1, \@seq2... ]);

Given an array-ref of array-refs with paths, returns a list of
non-existing paths.

=head2 get_ids_by_paths

    @ids = $m->get_ids_by_paths([ \@seq1, \@seq2... ]);

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
