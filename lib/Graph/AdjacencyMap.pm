package Graph::AdjacencyMap;

use strict;
use warnings;

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
     fields => [qw(_n _f _arity _i _s _p _g _u _ni _nc _na _nm)]);
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
sub _p () { 5 } # Predecessors.
sub _g () { 6 } # Graph (AdjacencyMap::Light)

sub _V () { 2 }  # Graph::_V()

sub stringify {
    my $m = shift;
    <<EOF;
@{[ref $m]} arity=@{[$m->[ _arity ]]} flags: @{[_stringify_fields($m->[ _f ])]}
EOF
}

sub _stringify_fields {
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
    my $class = shift;
    bless [ 0, @_ ], $class;
}

sub _ids {
    my $m = shift;
    return $m->[ _i ];
}

sub has_paths {
    my $m = shift;
    return defined $m->[ _i ] && @{ $m->[ _i ] };
}

sub _new_node {
    my ($m, $n, $id) = @_;
    my $f = $m->[ _f ];
    my $i = $m->[ _n ]++;
    if (($f & _MULTI)) {
	$id = 0 if $id eq _GEN_ID;
	$$n = [ $i, 0, undef, { $id => { } } ];
    } elsif (($f & _COUNT)) {
	$$n = [ $i, 1 ];
    } else {
	$$n = $i;
    }
    return $i;
}

sub _inc_node {
    my ($m, $n, $id) = @_;
    my $f = $m->[ _f ];
    if (($f & _MULTI)) {
	if ($id eq _GEN_ID) {
	    $$n->[ _nc ]++
		while exists $$n->[ _nm ]->{ $$n->[ _nc ] };
	    $id = $$n->[ _nc ];
	}
	$$n->[ _nm ]->{ $id } = { };
    } elsif (($f & _COUNT)) {
	$$n->[ _nc ]++;
    }
    return $id;
}

sub __get_path_node {
    my ($p, $k);
    my $f = $_[0]->[ _f ];
    if (@_ > 2 && ($f & _UNORDUNIQ)) {
        if (($f & _UNORDUNIQ) == _UNORD && @_ > 2) { @_ = ($_[0], sort @_[1..$#_]) }
        else { &__arg }
    }
    my ($m) = @_;
    if ($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ|_MULTI))) { # Fast path.
	my $s = $m->[ _s ];
	return unless exists $s->{ $_[1] };
	$p = [ $s, $s->{ $_[1] } ];
	$k = [ $_[1], $_[2] ];
    } else {
	return unless ($p, $k) = &{ $m->can('__has_path') };
    }
    my $l = defined $k->[-1] ? $k->[-1] : "";
    return ( exists $p->[-1]->{ $l }, $p->[-1]->{ $l }, $p, $k, $l );
}

sub set_path_by_multi_id {
    my $m = $_[0];
    my ($p, $k) = &{ $m->can('__set_path') };
    return unless defined $p && defined $k;
    my $l = defined $k->[-1] ? $k->[-1] : "";
    return $m->__set_path_node( $p, $l, @_[1..$#_] );
}

sub paths_non_existing {
    my ($m, $list) = @_;
    grep !$m->has_path(@$_), @$list;
}

sub has_path_by_multi_id {
    my $m = $_[0];
    my $id = pop;
    my ($e, $n) = &{ $m->can('__get_path_node') };
    return undef unless $e;
    return exists $n->[ _nm ]->{ $id };
}

sub del_path_by_multi_id {
    my $m = $_[0];
    my $f = $m->[ _f ];
    my $id = pop;
    my ($e, $n, $p, $k, $l) = &{ $m->can('__get_path_node') };
    return unless $e;
    delete $n->[ _nm ]->{ $id };
    unless (keys %{ $n->[ _nm ] }) {
	delete $m->[ _i ]->[ $n->[ _ni ] ];
	delete $p->[-1]->{ $l };
	while (@$p && @$k && keys %{ $p->[-1]->{ $k->[-1] } } == 0) {
	    delete $p->[-1]->{ $k->[-1] };
	    pop @$p;
	    pop @$k;
	}
    }
    return 1;
}

sub get_multi_ids {
    my $f = $_[0]->[ _f ];
    return () unless ($f & _MULTI);
    my ($e, $n) = &__get_path_node;
    return $e ? keys %{ $n->[ _nm ] } : ();
}

sub _has_path_attrs {
    return undef unless defined(my $attrs = &_get_path_attrs);
    keys %$attrs ? 1 : 0;
}

sub _set_path_attrs {
    my $f = $_[0]->[ _f ];
    my $attrs = pop;
    my $id   = pop if ($f & _MULTI);
    &{ $_[0]->can('__attr') };
    my ($m) = @_;
    push @_, $id if ($f & _MULTI);
    my ($p, $k) = &{ $m->can('__set_path') };
    return unless defined $p && defined $k;
    my $l = defined $k->[-1] ? $k->[-1] : "";
    $m->__set_path_node( $p, $l, @_[1..$#_] ) unless exists $p->[-1]->{ $l };
    if (($f & _MULTI)) {
	$p->[-1]->{ $l }->[ _nm ]->{ $id } = $attrs;
    } else {
	# Extend the node if it is a simple id node.
	$p->[-1]->{ $l } = [ $p->[-1]->{ $l }, 1 ] unless ref $p->[-1]->{ $l };
	$p->[-1]->{ $l }->[ _na ] = $attrs;
    }
}

sub _has_path_attr {
    my $attr = pop;
    return undef unless defined(my $attrs = &_get_path_attrs);
    exists $attrs->{$attr};
}

sub _set_path_attr {
    my $f = $_[0]->[ _f ];
    my $val  = pop;
    my $attr = pop;
    my $id   = pop if ($f & _MULTI);
    &{ $_[0]->can('__attr') }; # _LIGHT maps need this to get upgraded when needed, also sorts for _UNORD
    my ($m) = @_;
    push @_, $id if ($f & _MULTI);
    my ($p, $k) = &{ $m->can('__set_path') };
    return unless defined $p && defined $k;
    my $l = defined $k->[-1] ? $k->[-1] : "";
    $m->__set_path_node( $p, $l, @_[1..$#_] ) unless exists $p->[-1]->{ $l };
    if (($f & _MULTI)) {
	$p->[-1]->{ $l }->[ _nm ]->{ $id }->{ $attr } = $val;
    } else {
	# Extend the node if it is a simple id node.
	$p->[-1]->{ $l } = [ $p->[-1]->{ $l }, 1 ] unless ref $p->[-1]->{ $l };
	$p->[-1]->{ $l }->[ _na ]->{ $attr } = $val;
    }
    return $val;
}

sub _get_path_attrs {
    my $f = $_[0]->[ _f ];
    my $id = pop if ($f & _MULTI);
    &{ $_[0]->can('__attr') };
    my ($m) = @_;
    if (($f & _MULTI)) {
	return unless my ($p, $k) = &{ $m->can('__has_path') };
	push @_, $id;
	my $l = defined $k->[-1] ? $k->[-1] : "";
	$p->[-1]->{ $l }->[ _nm ]->{ $id };
    } else {
	my ($e, $n) = &{ $m->can('__get_path_node') };
	return unless $e;
	return $n->[ _na ] if ref $n && $#$n == _na;
	return;
    }
}

sub _get_path_attr {
    my $attr = pop;
    return undef unless defined(my $attrs = &_get_path_attrs);
    $attrs->{$attr};
}

sub _get_path_attr_names {
    return unless defined(my $attrs = &_get_path_attrs);
    keys %$attrs;
}

sub _get_path_attr_values {
    return unless defined(my $attrs = &_get_path_attrs);
    values %$attrs;
}

sub _del_path_attrs {
    my $f = $_[0]->[ _f ];
    my $id = pop if ($f & _MULTI);
    &{ $_[0]->can('__attr') };
    my ($m) = @_;
    if ($f & _MULTI) {
	return unless my ($p, $k) = &{ $m->can('__has_path') };
	push @_, $id;
	my $l = defined $k->[-1] ? $k->[-1] : "";
	delete $p->[-1]->{ $l }->[ _nm ]->{ $id };
	delete $p->[-1]->{ $l }
	    unless keys %{ $p->[-1]->{ $l }->[ _nm ] } ||
		   (defined $p->[-1]->{ $l }->[ _na ] &&
		    keys %{ $p->[-1]->{ $l }->[ _na ] });
    } else {
	my ($e, $n) = &{ $m->can('__get_path_node') };
	return undef unless $e;
	return 0 if !ref $n;
	$e = _na == $#$n && keys %{ $n->[ _na ] } ? 1 : 0;
	$#$n = _na - 1;
	return $e;
    }
}

sub _del_path_attr {
    my $attr = pop;
    return unless defined(my $attrs = &_get_path_attrs);
    return 0 unless exists $attrs->{$attr};
    delete $attrs->{$attr};
    return 1 if keys %$attrs;
    &_del_path_attrs;
    1;
}

sub __arg {
    my ($m) = @_;
    return if @_ < 3; # nothing to do if 1 or 0 passed args
    my $f = $m->[ _f ];
    my @a = @_[1..$#_];
    my %u;
    @a = grep !$u{$_}++, @a if $f & _UNIQ;
    # Alphabetic or numeric sort, does not matter as long as it unifies.
    @_ = ($_[0], $f & _UNORD ? sort @a : @a);
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
