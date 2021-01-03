package Graph::AdjacencyMap;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

my (@FLAGS, %FLAG_COMBOS, %FLAG2I);
BEGIN {
    @FLAGS = qw(_COUNT _MULTI _UNORD _REF _UNIONFIND _LIGHT _STR);
    %FLAG_COMBOS = (
	_COUNTMULTI => [qw(_COUNT _MULTI)],
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
     fields => [qw(_n _f _arity _i _s _attr _count)]);
@EXPORT_OK = map @$_, values %EXPORT_TAGS;

my $_GEN_ID = 0;

sub _GEN_ID () { \$_GEN_ID }

sub _n () { 0 } # Next id.
sub _f () { 1 } # Flags.
sub _arity () { 2 } # Arity.
sub _i () { 3 } # Index to path.
sub _s () { 4 } # Successors / Path to Index.
sub _attr () { 5 } # attributes - two-level for MULTI
sub _count () { 6 }

sub stringify {
    my ($f, $a, $s, $m) = (@{ $_[0] }[ _f, _arity, _s ], $_[0]);
    my ($multi, @rows) = $f & _MULTI;
    my @p = map $_->[0], sort _s_sort map [$_,"@$_"], $m->paths; # use the Schwartz
    if (defined $a and $a == 2) {
	my (%pre, %suc, @s);
	$pre{$_->[0]} = $suc{$_->[1]} = 1 for @p;
	@rows = ([ 'to:', @s = sort keys %suc ], map {
	    my $p = $_;
	    [ $p, map {
		my $text = defined(my $id = $m->has_path([$p, $_])) ? 1 : '';
		my $attrs = !$text ? undef :
		    $multi ? $m->[ _attr ][$id] : $m->_get_path_attrs([$p, $_]);
		defined $attrs ? $m->_dumper($attrs) : $text;
	    } @s ];
	} sort keys %pre);
    } else {
	@rows = map {
	    my $attrs = $multi
		? $m->[ _attr ][ $m->has_path($_) ] : $m->_get_path_attrs($_);
	    [ (defined $a and $a == 1) ? $_->[0] : '[' . join(' ', @$_) . ']',
		($m->get_ids_by_paths([ $_ ], 0))[0].
		    (!defined $attrs ? '' : ",".$m->_dumper($attrs)) ];
	} @p;
    }
    join '',
	map "$_\n",
	"@{[ref $m]} arity=@{[$m->_dumper($a)]} flags: @{[_stringify_fields($m->[ _f ])]}",
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
    my ($class, $flags, $arity) = @_;
    bless [ 0, $flags, $arity, [], (defined $arity ? {} : []), [], [] ], $class;
}

sub _ids {
    $_[0]->[ _i ];
}

sub has_any_paths {
    defined $_[0]->[ _arity ]
	? keys %{ $_[0]->[ _s ] }
	: grep defined, @{ $_[0]->[ _i ] };
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

sub set_path {
    push @_, 1;
    (&__set_path)[0];
}

sub set_path_by_multi_id {
    push @_, 1;
    (&__set_path)[1];
}

sub __set_path {
    my $inc_if_exists = pop;
    &__arg;
    my ($f, $a, $map_i, $s, $m, $id, @a) = (@{ $_[0] }[ _f, _arity, _i, _s ], @_[0, 2], @{ $_[1] });
    my $is_multi = $f & _MULTI;
    my @path = @a;
    @a = map ref() ? __strval($_, $f) : $_, @a if $f & _REF;
    my $p = (defined($a) ? $s : $s->[ @a ]) ||= { };
    $p = $p->{ $_ } ||= {} for @a[0..$#a-1];
    my $l = ( @a ? @a : '' )[-1];
    if (exists $p->{ $l }) {
	return ($p->{ $l }) if !($inc_if_exists and ($f & _COUNTMULTI));
	my $nc = \$m->[ _count ][ my $i = $p->{ $l } ];
	$$nc++, return ($i) if !$is_multi;
	my $na = $m->[ _attr ][ $i ];
	if ($id eq _GEN_ID) {
	    $$nc++ while exists $na->{ $$nc };
	    $id = $$nc;
	}
	$na->{ $id } = { };
	return ($i, $id);
    }
    $map_i->[ $p->{ $l } = my $i = $m->[ _n ]++ ] = \@path;
    $m->[ _attr ][ $i ] = { ($id = ($id eq _GEN_ID) ? 0 : $id) => {} } if $is_multi;
    $m->[ _count ][ $i ] = $is_multi ? 0 : 1 if ($f & _COUNTMULTI);
    ($i, $id);
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
    return exists $_[0]->[ _attr ][ $i ]->{ $_[2] };
}

sub del_path {
    return unless my ($i, $p, $k) = &__has_path;
    return 1 if &_is_COUNT and --$_[0]->[ _count ][ $i ] > 0;
    _sequence_del((my $m = $_[0])->[ _i ], $i, $p, $k);
    delete $m->[ $_ ]->[ $i ] for _count, _attr;
    1;
}

sub del_path_by_multi_id {
    return unless my ($i, $p, $k) = &__has_path;
    delete((my $attrs = (my $m = $_[0])->[ _attr ][ $i ])->{ $_[2] });
    return 1 if keys %$attrs;
    _sequence_del($m->[ _i ], $i, $p, $k);
    delete $m->[ $_ ]->[ $i ] for _count, _attr;
    1;
}

sub get_multi_ids {
    return unless ((my $m = $_[0])->[ _f ] & _MULTI) and my ($i) = &__has_path;
    keys %{ $m->[ _attr ][ $i ] };
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] > 1; # arity > 1, all integers, no names
    return unless my ($i, $p, $k) = $m->__has_path([$from]);
    $p->[ -1 ]{ $to } = delete $p->[-1]{ $k->[-1] };
    $m->[ _i ][ $i ] = [ $to ];
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
    my ($f, $a, $s, @k) = (@{ $_[0] }[ _f, _arity, _s ], @{ $_[1] });
    @k = map ref() ? __strval($_, $f) : $_, @k if $f & _REF;
    return if !defined(my $orig_s = $s = defined($a) ? $s : $s->[ @k ]);
    my @p = ($orig_s, map defined($s = $s->{$_}) ? $s : return, @k[0..$#k-1]);
    @k = '' if !@k;
    (exists $s->{$k[-1]} ? $s->{$k[-1]} : return, \@p, \@k);
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

sub get_paths_by_ids {
    my ($i, undef, $list) = ( @{ $_[0] }[ _i ], @_ );
    map [ map @{ $i->[ $_ ] }, @$_ ], @$list;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

sub get_ids_by_paths {
    my ($f, $a, $s, $m, $list, $ensure) = ( @{ $_[0] }[ _f, _arity, _s ], @_ );
    my ($is_multi, $is_hyper) = (($f & _MULTI), !defined $a);
    my (@id, @empty_indices, @non_exist);
    if (!($is_hyper or $f & (_REF))) { # Fast path
	for (@$list) {
	    my ($this_s, @p) = ($s, @$_);
	    $this_s = $this_s->{ shift @p } while defined $this_s and @p;
	    push @id,
		defined $this_s ? $this_s :
		!$ensure ? return :
		(push(@empty_indices, 0+@id), push(@non_exist, $_), undef)[2];
	}
	$id[$empty_indices[$_]] = $is_multi
	    ? $m->set_path_by_multi_id($non_exist[$_], _GEN_ID)
	    : $m->set_path($non_exist[$_]) for 0..$#empty_indices;
	return @id;
    }
    my ($is_ref, @dereffed) = (($f & _REF), ($f & _REF) ? map [ map ref() ? __strval($_, $f) : $_, @$_ ], @$list : ());
    for (0..$#$list) {
	my ($this_s, @p) = ($is_hyper ? $s->[ @{ $list->[$_] } ] : $s,
	    $is_ref ? @{ $dereffed[$_] } : @{ $list->[$_] });
	if (@p) {
	    $this_s = $this_s->{ shift @p } while @p and defined $this_s;
	} else {
	    $this_s = $this_s->{''};
	}
	push @id,
	    defined $this_s ? $this_s :
	    !$ensure ? return :
	    (push(@empty_indices, 0+@id), push(@non_exist, $list->[$_]), undef)[2];
    }
    $id[$empty_indices[$_]] = $is_multi
	? $m->set_path_by_multi_id($non_exist[$_], _GEN_ID)
	: $m->set_path($non_exist[$_]) for 0..$#empty_indices;
    @id;
}

sub __strval {
    my ($k, $f) = @_;
    return $k unless ref $k && ($f & _REF);
    require overload;
    (($f & _STR) xor overload::Method($k, '""')) ? overload::StrVal($k) : $k;
}

sub __arg {
    my ($f, $a, $m, @a) = (@{ $_[0] }[ _f, _arity ], $_[0], @{ $_[1] });
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->stringify,
		  scalar @a, $a)
        if defined($a) and @a != $a;
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

=head2 set_path(\@seq)

Set the path by @ids. Returns the integer ID of the path.

=head2 set_path_by_multi_id(\@seq, $id)

Set the path in the Map by the multi id.

=head2 get_paths_by_ids([ \@idlist1, \@idlist2... ])

Given an array-ref of array-refs of vertex IDs, returns a list of
array-refs of vertex-names.

=head2 get_ids_by_paths

    @ids = $m->get_ids_by_paths([ \@seq1, \@seq2... ], $ensure);

Given an array-ref of array-refs with paths, returns a list of IDs of
existing paths (non-existing ones will not be represented).

If C<$ensure> is true, will first create paths that do not already exist.

=head2 rename_path($from, $to)

Rename the path.

=head2 stringify

Return a string describing the object in a human-friendly(ish) way.

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
