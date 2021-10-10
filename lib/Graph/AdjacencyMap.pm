package Graph::AdjacencyMap;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

my $empty = {};
sub _empty () { $empty }

my (@FLAGS, %FLAG_COMBOS, %FLAG2I, @FIELDS);
BEGIN {
    @FLAGS = qw(_COUNT _MULTI _UNORD _REF _UNIONFIND _LIGHT _STR);
    %FLAG_COMBOS = (
	_COUNTMULTI => [qw(_COUNT _MULTI)],
	_REFSTR => [qw(_REF _STR)],
    );
    # Next id, Flags, Arity, Index to path, path to index,
    # successors, predecessors: 2-level hashes to array-ref of path IDs
    # attributes - two-level for MULTI, node/multi count
    @FIELDS = qw(_n _f _arity _i _pi _s _p _attr _count);
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
    for my $i (0..$#FIELDS) {
	no strict 'refs';
	*{ $FIELDS[$i] }= sub () { $i };
    }
}

sub _new {
    my ($class, $flags, $arity) = @_;
    my $hyper = !$arity;
    my $need_s = $arity != 1;
    my $need_p = $need_s && !($flags & _UNORD);
    bless [
	0, $flags, $arity, [], {},
	($need_s ? {} : undef), ($need_p ? {} : undef),
	[], [],
    ], $class;
}

require Exporter;
use vars qw(@ISA @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);
%EXPORT_TAGS =
    (flags =>  [@FLAGS, keys %FLAG_COMBOS, qw(_GEN_ID)],
     fields => \@FIELDS);
@EXPORT_OK = map @$_, values %EXPORT_TAGS;

my $_GEN_ID = 0;

sub _GEN_ID () { \$_GEN_ID }

sub stringify {
    my ($f, $arity, $m) = (@{ $_[0] }[ _f, _arity ], $_[0]);
    my ($multi, @rows) = $f & _MULTI;
    my @p = $m->paths;
    @p = $arity == 1 ? sort @p :
	map $_->[0], sort { $a->[1] cmp $b->[1] }
	    ($arity == 0 && !($f & _UNORD))
		? map [$_, join '|', map "@$_", @$_], @p
		: map [$_,"@$_"], @p; # use the Schwartz
    if ($arity == 2) {
	require Set::Object;
	my ($pre, $suc, @s) = (Set::Object->new(map $_->[0], @p), Set::Object->new(map $_->[1], @p));
	@rows = ([ 'to:', @s = sort $suc->members ], map {
	    my $p = $_;
	    [ $p, map {
		my $text = defined(my $id = $m->has_path([$p, $_])) ? 1 : '';
		my $attrs = !$text ? undef :
		    $multi ? $m->[ _attr ][$id] : $m->_get_path_attrs([$p, $_]);
		defined $attrs ? $m->_dumper($attrs) : $text;
	    } @s ];
	} sort $pre->members);
    } else {
	@rows = map {
	    my $attrs = $multi
		? $m->[ _attr ][ $m->has_path($_) ] : $m->_get_path_attrs($_);
	    [ $m->_dumper($_),
		($m->get_ids_by_paths([ $_ ], 0))[0].
		    (!defined $attrs ? '' : ",".$m->_dumper($attrs)) ];
	} @p;
    }
    join '',
	map "$_\n",
	"@{[ref $m]} arity=$arity flags: @{[_stringify_fields($m->[ _f ])]}",
	map join(' ', map sprintf('%4s', $_), @$_),
	@rows;
}

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

sub has_any_paths {
    scalar keys %{ $_[0]->[ _pi ] };
}

sub _set_path_attr_common {
    push @_, 0;
    my ($i) = &__set_path;
    my $attr = (my $m = $_[0])->[ _attr ];
    ($m->[ _f ] & _MULTI) ? \$attr->[ $i ]{ $_[2] } : \$attr->[ $i ];
}

sub _set_path_attrs {
    ${ &{ $_[0]->can('_set_path_attr_common') } } = $_[-1];
}

sub _set_path_attr {
    ${ &{ $_[0]->can('_set_path_attr_common') } }->{ $_[-2] } = $_[-1];
}

sub set_paths {
    map +($_[0]->__set_path($_, 1))[0], @_[1..$#_];
}

sub set_path_by_multi_id {
    push @_, 1;
    goto &__set_path;
}

sub __set_path {
    my $inc_if_exists = pop;
    &__arg;
    my ($f, $a, $map_i, $pi, $map_s, $map_p, $m, $k, $id) = (@{ $_[0] }[ _f, _arity, _i, _pi, _s, _p ], @_);
    my $is_multi = $f & _MULTI;
    my $k_orig = $k;
    $k = __strval($k, $f) if $a == 1 && ($f & _REF) && ref($k);
    my $l = ($a == 0 && !($f & _UNORD)) ? join '|', map join(' ', sort @$_), @$k : $a == 1 ? "$k" : "@$k";
    if (exists $pi->{ $l }) {
	return ($pi->{ $l }) if !($inc_if_exists and ($f & _COUNTMULTI));
	my $nc = \$m->[ _count ][ my $i = $pi->{ $l } ];
	$$nc++, return ($i) if !$is_multi;
	my $na = $m->[ _attr ][ $i ];
	if ($id eq _GEN_ID) {
	    $$nc++ while exists $na->{ $$nc };
	    $id = $$nc;
	}
	$na->{ $id } = { };
	return ($i, $id);
    }
    $map_i->[ $pi->{ $l } = my $i = $m->[ _n ]++ ] = $k_orig;
    $m->[ _attr ][ $i ] = { ($id = ($id eq _GEN_ID) ? 0 : $id) => {} } if $is_multi;
    $m->[ _count ][ $i ] = $is_multi ? 0 : 1 if ($f & _COUNTMULTI);
    _successors_add($f, $a, $map_s, $map_p, $i, $k) if $map_s; # dereffed
    ($i, $id);
}

sub _successors_add {
    my ($f, $a, $map_s, $map_p, $id, $path) = @_;
    my $pairs = _successors_cartesian(($f & _UNORD), $a == 0, $path);
    push @{ $map_s->{ $_->[0] }{ $_->[1] } }, $id for @$pairs;
    return if !$map_p;
    push @{ $map_p->{ $_->[1] }{ $_->[0] } }, $id for @$pairs;
}

sub _successors_del {
    my ($f, $a, $map_s, $map_p, $id, $path) = @_;
    my $pairs = _successors_cartesian(($f & _UNORD), $a == 0, $path);
    for (@$pairs) {
	my ($p, $s) = @$_;
	my @new = grep $_ != $id, @{ $map_s->{ $p }{ $s } };
	if (@new) {
	    $map_s->{ $p }{ $s } = \@new;
	    $map_p->{ $s }{ $p } = \@new if $map_p;
	    next;
	}
	delete $map_s->{ $p }{ $s };
	delete $map_s->{ $p } if !keys %{ $map_s->{ $p } };
	next if !$map_p;
	delete $map_p->{ $s }{ $p };
	delete $map_p->{ $s } if !keys %{ $map_p->{ $s } };
    }
}

sub _successors_cartesian {
    my ($unord, $hyper, $seq) = @_;
    return [ $seq ] if !$unord and !$hyper;
    return [] if $unord and $hyper and !@$seq;
    my ($allow_self, $p_s, $s_s, @pairs);
    if ($unord) {
	require Set::Object;
	my @a = Set::Object->new(@$seq)->members;
	($allow_self, $p_s, $s_s) = (@a < 2, \@a, \@a);
    } else {
	($allow_self, $p_s, $s_s) = (1, @$seq);
    }
    for my $p (@$p_s) {
	push @pairs, map [$p, $_], $allow_self ? @$s_s : grep $p != $_, @$s_s;
    }
    \@pairs;
}

sub _get_path_count {
    return 0 unless my ($i) = &__has_path;
    my $f = (my $m = $_[0])->[ _f ];
    return
        ($f & _COUNT) ? $m->[ _count ][ $i ] :
        ($f & _MULTI) ? scalar keys %{ $m->[ _attr ][ $i ] } : 1;
}

sub has_path {
    ( &__has_path )[0];
}

sub has_path_by_multi_id {
    return undef unless my ($i) = &__has_path;
    return exists $_[0]->[ _attr ][ $i ]{ $_[2] };
}

sub del_path {
    return unless my ($i, $l) = &__has_path;
    return 1 if &_is_COUNT and --$_[0][ _count ][ $i ] > 0;
    $_[0]->_sequence_del($i, $l);
    1;
}

sub del_path_by_multi_id {
    return unless my ($i, $l) = &__has_path;
    delete((my $attrs = (my $m = $_[0])->[ _attr ][ $i ])->{ $_[2] });
    return 1 if keys %$attrs;
    $m->_sequence_del($i, $l);
    1;
}

sub get_multi_ids {
    return unless ((my $m = $_[0])->[ _f ] & _MULTI) and my ($i) = &__has_path;
    keys %{ $m->[ _attr ][ $i ] };
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] != 1; # all integers, no names
    return unless my ($i, $l) = $m->__has_path($from);
    $m->[ _i ][ $i ] = $to;
    $to = __strval($to, $m->[ _f ]) if ref($to) and ($m->[ _f ] & _REF);
    $m->[ _pi ]{ $to } = delete $m->[ _pi ]{ $l };
    return 1;
}

sub _del_path_attrs {
    return unless my ($i) = &__has_path;
    my $attr = (my $m = $_[0])->[ _attr ];
    return $attr->[ $i ]{ $_[2] } = undef, 1 if ($m->[ _f ] & _MULTI);
    delete $attr->[ $i ];
}

sub __has_path {
    &__arg;
    my ($f, $a, $pi, $k) = (@{ $_[0] }[ _f, _arity, _pi ], $_[1]);
    $k = __strval($k, $f) if $a == 1 && ($f & _REF) && ref($k);
    my $l = ($a == 0 && !($f & _UNORD)) ? join '|', map join(' ', sort @$_), @$k : $a == 1 ? "$k" : "@$k";
    my $id = $pi->{ $l };
    (defined $id ? $id : return, $l);
}

sub _get_path_attrs {
    return unless my ($i) = &__has_path;
    my $attrs = (my $m = $_[0])->[ _attr ][ $i ];
    ($m->[ _f ] & _MULTI) ? $attrs->{ $_[2] } : $attrs;
}

sub _has_path_attrs {
    keys %{ &{ $_[0]->can('_get_path_attrs') } || return undef } ? 1 : 0;
}

sub _has_path_attr {
    exists(( &{ $_[0]->can('_get_path_attrs') } || return )->{ $_[-1] });
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

sub _del_path_attr {
    return unless my $attrs = &{ $_[0]->can('_get_path_attrs') };
    return 0 unless exists $attrs->{ my $attr = $_[-1] };
    delete $attrs->{$attr};
    return 1 if keys %$attrs;
    &{ $_[0]->can('_del_path_attrs') };
    1;
}

sub _sequence_del {
    my ($m, $id, $l) = @_;
    my ($f, $a, $map_i, $pi, $map_s, $map_p) = @$m[ _f, _arity, _i, _pi, _s, _p ];
    delete $pi->{ $l };
    delete $m->[ $_ ][ $id ] for _count, _attr;
    my $path = delete $map_i->[ $id ];
    _successors_del($f, $a, $map_s, $map_p, $id, $path) if $map_s;
    return 1;
}

sub get_paths_by_ids {
    my ($i, undef, $list, $deep) = ( @{ $_[0] }[ _i ], @_ );
    $deep ? map [ map [ @$i[ @$_ ] ], @$_ ], @$list : map [ @$i[ @$_ ] ], @$list;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || Graph::_empty_array() };
}

sub get_ids_by_paths {
    my ($f, $a, $pi, $m, $list, $ensure, $deep) = ( @{ $_[0] }[ _f, _arity, _pi ], @_ );
    $deep ||= 0;
    my ($is_multi, $is_ref, $is_unord) = (map $f & $_, _MULTI, _REF, _UNORD);
    return map { # Fast path
	my @ret = map {
	    my $id = $pi->{ $a != 1 ? "@$_" : $_ };
	    defined $id ? $id :
		!$ensure ? return :
		($is_multi ? $m->set_path_by_multi_id($_, _GEN_ID) : $m->set_paths($_))[0];
	} $deep ? @$_ : $_;
	$deep ? \@ret : @ret;
    } @$list if $a and !$is_ref and $deep < 2;
    map {
	my @ret = map {
	    my @ret2 = map {
		my $k = $_;
		$k = __strval($k, $f) if $a == 1 && $is_ref && ref($k);
		my $l = ($a == 0 && !$is_unord) ? join '|', map join(' ', sort @$_), @$k : $a == 1 ? "$k" : "@$k";
		my $id = $pi->{ $l };
		defined $id ? $id :
		    !$ensure ? return :
		    ($is_multi ? $m->set_path_by_multi_id($_, _GEN_ID) : $m->set_paths($_))[0];
	    } $deep > 1 ? @$_ : $_;
	    $deep > 1 ? \@ret2 : @ret2;
	} $deep ? @$_ : $_;
	$deep ? \@ret : @ret;
    } @$list;
}

sub _paths_fromto {
    my $offset = pop;
    my ($i, $map_x, @v) = ( @{ $_[0] }[ _i, $offset ], @_[1..$#_] );
    Graph::__carp_confess("undefined vertex") if grep !defined, @v;
    require Set::Object;
    map $i->[ $_ ], Set::Object->new(map @$_, map values %{ $map_x->{ $_ } || _empty }, @v)->members;
}
sub paths_from { push @_, _s; goto &_paths_fromto }
sub paths_to { push @_, _p; goto &_paths_fromto }

sub _cessors {
    my $offset = pop;
    my ($map_x, @v) = ( @{ $_[0] }[ $offset ], @_[1..$#_] );
    Graph::__carp_confess("undefined vertex") if grep !defined, @v;
    require Set::Object;
    Set::Object->new(map keys %{ $map_x->{ $_ } || _empty }, @v)->members;
}
sub successors { push @_, _s; goto &_cessors }
sub predecessors { push @_, _p; goto &_cessors }

sub has_successor {
    my ($map_s, $u, $v) = ( @{ $_[0] }[ _s ], @_[1, 2] );
    Graph::__carp_confess("undefined vertex") if grep !defined, $u, $v;
    exists ${ $map_s->{ $u } || _empty }{ $v };
}

sub __strval {
    my ($k, $f) = @_;
    return $k unless ref $k && ($f & _REF);
    return "$k" if ($f & _STR);
    require Scalar::Util;
    Scalar::Util::refaddr($k);
}

sub __arg {
    my ($f, $a, $m, $k) = (@{ $_[0] }[ _f, _arity ], @_[0, 1]);
    Graph::__carp_confess(sprintf "arguments %d (%s) expected %d for\n".$m->stringify,
	scalar @$k, "@$k", $a)
	if $a > 1 and @$k != $a;
}

sub reindex {
    my ($f, $a, $i2p, $m) = (@{ $_[0] }[ _f, _arity, _i ], $_[0]);
    my $is_ref = $a == 1 && ($f & _REF);
    my $pi = $m->[ _pi ] = {};
    for my $i ( 0..$#{ $i2p } ) {
        next if !defined(my $k = $i2p->[ $i ]); # deleted
        $k = __strval($k, $f) if $is_ref && ref($k);
        $pi->{ $k } = $i;
    }
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

Returns the integer ID of the path, or undef if Map doesn't have it.

=head2 has_any_paths

Return true if the Map has any paths, false if not.

=head2 has_path_by_multi_id(\@seq, $id)

Return true if the Map has the path by a multi(vertex) id, false if not.

=head2 paths

Return all the paths of the Map.

=head2 set_paths(\@seq1, \@seq2, ...)

    @ids = set_paths($seq1, $seq2, ...)

Create/identify the path of C<$seq*>. Returns the integer ID of each path.
For arity other than 1, the sequence items must be integers.
For arity 1, do not wrap the item in an array.
For C<_UNORD>, you must give the sequence already sorted.

=head2 set_path_by_multi_id(\@seq, $id)

    ($integer_ID, $multi_ID) = $m->set_path_by_multi_id(\@seq, $id)

Set the path in the Map by the multi id.

=head2 get_paths_by_ids([ \@idlist1, \@idlist2... ], $deep)

Given an array-ref of array-refs of vertex IDs, returns a list of
array-refs of vertex-names.
This is to look up vertex paths for use in edges. Only useful for arity 1.
The C<$deep> option is useful with directed hyperedges.

=head2 get_ids_by_paths

    @ids = $m->get_ids_by_paths([ \@seq1, \@seq2... ], $ensure, 0);
    @id_lists = $m->get_ids_by_paths([ \@seq1, \@seq2... ], $ensure, 1);

This is to look up vertex IDs for use in edges. Only useful for arity 1.
Given an array-ref of array-refs with paths, returns a list of IDs of
existing paths.

If C<$ensure> is true, will first create paths that do not already exist.
If it is not, any non-existing paths will cause an empty list to be returned.

If $deep is true, each sequence will be treated as a list of paths,
and IDs filled in for the return values. This can have a value up to 2.

=head2 rename_path($from, $to)

Rename the path.

=head2 stringify

Return a string describing the object in a human-friendly(ish) way.

=head2 successors

    @successors = $m->successors(@v)

Only valid for a map of arity other than 1.

=head2 predecessors

    @predecessors = $m->predecessors($v)

Only valid for a non-C<_UNORD> map of arity other than 1.

=head2 paths_from

    @paths = $m->paths_from(@v)

Only valid for a map of arity other than 1.

=head2 paths_to

    @paths = $m->paths_to($v)

Only valid for a non-C<_UNORD> map of arity other than 1.

=head2 has_successor

    $bool = $m->has_successor($u, $v)

Only valid for a map of arity other than 1.

=head2 reindex

Will recreate the mapping from paths to indexes. Intended for use after
a deep copy.

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
