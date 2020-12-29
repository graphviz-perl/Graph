package Graph::AdjacencyMap;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

my (@FLAGS, %FLAG_COMBOS, %FLAG2I);
BEGIN {
    @FLAGS = qw(_COUNT _MULTI _UNORD _UNIQ _REF _UNIONFIND _LIGHT _STR);
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
    my $m = shift;
    my @rows;
    my $a = $m->[ _arity ];
    my $s = $m->[ _s ];
    my $f = $m->[ _f ];
    my $multi = $f & _MULTI;
    my @p = map $_->[0], sort _s_sort map [$_,"@$_"], $m->paths; # use the Schwartz
    if (defined $a and $a == 2) {
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
		    ? $m->[ _attr ][ ( $m->__has_path( [$u, $v] ) )[0] ]
		    : $m->_get_path_attrs([$u, $v])
		    if $text;
		$text = $m->_dumper($attrs) if defined $attrs;
		push @r, $text;
	    }
	    push @rows, \@r;
	}
    } else {
	for my $v (@p) {
	    my @r = (defined $a and $a == 1) ? $v->[0] : '[' . join(' ', @$v) . ']';
	    my ($text) = $m->get_ids_by_paths([ $v ]);
	    my $attrs = $multi
		? $m->[ _attr ][ ( $m->__has_path( $v ) )[0] ]
		: $m->_get_path_attrs($v);
	    $text .= ",".$m->_dumper($attrs) if defined $attrs;
	    push @r, $text;
	    push @rows, \@r;
	}
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

sub has_paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

sub __has_path {
    &__arg;
    my ($f, $a, $s, @k) = (@{ $_[0] }[ _f, _arity, _s ], @{ $_[1] });
    @k = map ref() ? __strval($_, $f) : $_, @k if $f & _REF;
    return if !defined($s = defined($a) ? $s : $s->[ @k ]);
    my @p = ($s, map defined($s = $s->{$_}) ? $s : return, @k[0..$#k-1]);
    @k = '' if !@k;
    (exists $s->{$k[-1]} ? $s->{$k[-1]} : return, \@p, \@k);
}

sub set_path {
    push @_, 1;
    (&__set_path)[0];
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

sub _set_path_attr_common {
    push @_, 0;
    my ($i) = &__set_path;
    my $attr = (my $m = $_[0])->[ _attr ];
    ($m->[ _f ] & _MULTI) ? \$attr->[ $i ]{ $_[2] } : \$attr->[ $i ];
}

sub set_path_by_multi_id {
    push @_, 1;
    (&__set_path)[1];
}

sub paths_non_existing {
    my ($m, $list) = @_;
    grep !$m->has_path($_), @$list;
}

sub has_path {
    defined( ( &__has_path )[0] );
}

sub has_path_by_multi_id {
    return undef unless my ($i) = &__has_path;
    return exists $_[0]->[ _attr ][ $i ]->{ $_[2] };
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

sub _get_path_attrs {
    return unless my ($i) = &__has_path;
    my $attrs = (my $m = $_[0])->[ _attr ][ $i ];
    ($m->[ _f ] & _MULTI) ? $attrs->{ $_[2] } : $attrs;
}

sub _del_path_attrs {
    return unless my ($i) = &__has_path;
    my $attr = (my $m = $_[0])->[ _attr ];
    return $attr->[ $i ]{ $_[2] } = undef, 1 if ($m->[ _f ] & _MULTI);
    delete $attr->[ $i ];
}

sub get_paths_by_ids {
    my ($i, undef, $list) = ( @{ $_[0] }[ _i ], @_ );
    map [ map $i->[ $_ ], @$_ ], @$list;
}

sub paths {
    grep defined, @{ $_[0]->[ _i ] || [] };
}

sub get_ids_by_paths {
    my ($f, $a, $s, $m, $list) = ( @{ $_[0] }[ _f, _arity, _s ], @_ );
    my $is_hyper = !defined $a;
    return map { # Fast path
	my @p = @$_;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	defined $this_s ? $this_s : ();
    } @$list if !($is_hyper or $f & (_REF|_UNIQ));
    map {
	my @p = @$_;
	my $this_s = $is_hyper ? $s->[ @p ] : $s;
	if (@p) {
	    $this_s = $this_s->{ shift @p } while @p and defined $this_s;
	} else {
	    $this_s = $this_s->{''};
	}
	defined $this_s ? $this_s : ();
    } ($f & _REF) ? map [ map ref() ? __strval($_, $f) : $_, @$_ ], @$list : @$list;
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
    return undef unless my ($i) = &__has_path;
    my $f = (my $m = $_[0])->[ _f ];
    return
        ($f & _COUNT) ? $m->[ _count ][ $i ] :
        ($f & _MULTI) ? scalar keys %{ $m->[ _attr ][ $i ] } : 1;
}

sub _del_path_attr {
    return unless my $attrs = &{ $_[0]->can('_get_path_attrs') };
    return 0 unless exists $attrs->{ my $attr = $_[-1] };
    delete $attrs->{$attr};
    return 1 if keys %$attrs;
    &{ $_[0]->can('_del_path_attrs') };
    1;
}

sub __arg {
    my ($f, $a, $m, @a) = (@{ $_[0] }[ _f, _arity ], $_[0], @{ $_[1] });
    return if @a < 2; # nothing to do if 1 or 0-length seq
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->stringify,
		  scalar @a, $a)
        if defined($a) and @a != $a;
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
