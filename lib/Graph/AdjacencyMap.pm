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
    my @rows;
    my $a = $m->[ _arity ];
    my $s = $m->[ _s ];
    my $f = $m->[ _f ];
    my $hyper = $f & _HYPER;
    my $multi = $f & _MULTI;
    my $light = $f & _LIGHT;
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
		my $text = $m->has_path($u, $v) ? 1 : '';
		my $attrs = $multi
		    ? (( $m->__get_path_node( $u, $v ) )[0] || [])->[-1]
		    : $m->_get_path_attrs( $u, $v )
		    if $text and !$light;
		$text = $m->_dumper($attrs) if defined $attrs;
		push @r, $text;
	    }
	    push @rows, \@r;
	}
    } elsif ($a == 1) {
	for my $v (@p) {
	    my @r = $v->[0];
	    my ($text) = $m->get_ids_by_paths([ $v ]);
	    my $attrs = $multi
		? (( $m->__get_path_node( @$v ) )[0] || [])->[-1]
		: $m->_get_path_attrs(@$v) if !$light;
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
    @{ $_[0]->[ _i ] || [] };
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
    &__arg;
    my $f = $_[0]->[ _f ];
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
    exists $p->[-1]->{ $l } ? ( $p->[-1]->{ $l }, $p, $k, $l ) : ();
}

sub __set_path_node {
    my ($m, $p, $l, @args) = @_;
    my $f = $m->[ _f ];
    my $id = pop @args if $f & _MULTI;
    my $arity = $m->[ _arity ];
    return $m->_inc_node( \$p->[-1]->{ $l }, $id ) if exists $p->[-1]->{ $l };
    my $i = $m->_new_node( \$p->[-1]->{ $l }, $id );
    die "undefined index" if !defined $i;
    $m->[ _i ][ $i ] = \@args;
    defined $id ? ($id eq _GEN_ID ? $$id : $id) : $i;
}

sub __has_path {
    my ($m) = @_;
    my $f = $m->[ _f ];
    &Graph::AdjacencyMap::__arg;
    return if !defined(my $p = $m->[ _s ]);
    return if ($f & _HYPER) and !defined($p = $p->[ @_ - 1 ]);
    my @p = $p;
    my @k;
    my @a = @_[1..$#_];
    @a = map ref() ? __strval($_, $f) : $_, @a if $f & _REF;
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

sub set_path {
    my ($m) = @_;
    my $f = $m->[ _f ];
    return if @_ == 1 && !($f & _HYPER);
    &__arg;
    my ($p, $k) = &{ $m->can('__set_path') };
    $m->__set_path_node( $p, defined $k->[-1] ? $k->[-1] : "", @_[1..$#_] );
}

sub set_path_by_multi_id {
    my $m = $_[0];
    my ($p, $k) = &{ $m->can('__set_path') };
    $m->__set_path_node( $p, $k->[-1], @_[1..$#_] );
}

sub paths_non_existing {
    my ($m, $list) = @_;
    grep !$m->has_path(@$_), @$list;
}

sub has_path {
    my $m = $_[0];
    return unless my ($p, $k) = &{ $m->can('__has_path') };
    return exists $p->[-1]->{ $k->[-1] };
}

sub has_path_by_multi_id {
    my $m = $_[0];
    my $id = pop;
    return undef unless my ($n) = &{ $m->can('__get_path_node') };
    return exists $n->[ _nm ]->{ $id };
}

sub del_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    return unless my ($n, $p, $k, $l) = &{ $m->can('__get_path_node') };
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

sub del_path_by_multi_id {
    my $m = $_[0];
    my $id = pop;
    return unless my ($n, $p, $k, $l) = &{ $m->can('__get_path_node') };
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
    return unless ($f & _MULTI);
    return unless my ($n) = &__get_path_node;
    keys %{ $n->[ _nm ] };
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
    my $unord = $a > 1 && ($f & _UNORD);
    return map { # Fast path
	my @p = @$_;
	@p = sort @p if $unord;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	defined $this_s ? ref $this_s ? $this_s->[ _ni ] : $this_s : ();
    } @$list if $a == 2 && !($f & (_HYPER|_REF|_UNIQ));
    my @n;
    map !(@n = $m->_get_path_node(@$_)) ? () : ref $n[0] ? $n[0]->[ _ni ] : $n[0], @$list;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return 1 if $m->[ _arity ] > 1; # arity > 1, all integers, no names
    return unless my ($n, $p, $k, $l) = $m->__get_path_node( $from );
    $m->[ _i ][ ref $n ? $n->[ _ni ] : $n ] = [ $to ];
    $p->[ -1 ]{ $to } = delete $p->[ -1 ]{ $l };
    return 1;
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
    my $l = $k->[-1];
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
    my $l = $k->[-1];
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

sub __strval {
    my ($k, $f) = @_;
    return $k unless ref $k && ($f & _REF);
    require overload;
    (($f & _STR) xor overload::Method($k, '""')) ? overload::StrVal($k) : $k;
}

sub __set_path {
    my $m = $_[0];
    my $f = $m->[ _f ];
    my $id = pop if my $is_multi = $f & _MULTI;
    &Graph::AdjacencyMap::__arg;
    my @p = my $p = ($f & _HYPER) ?
	(( $m->[ _s ] ||= [ ] )->[ @_-1 ] ||= { }) :
	(  $m->[ _s ]                     ||= { });
    my @k;
    my @a = @_[1..$#_];
    push @_, $id if $is_multi;
    while (@a) {
	push @k, my $q = __strval(my $k = shift @a, $f);
	push @p, $p = $p->{ $q } ||= {} if @a;
    }
    return (\@p, \@k);
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
	return unless my ($n) = &{ $m->can('__get_path_node') };
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

sub _get_path_node {
    my $m = $_[0];
    my $f = $m->[ _f ];
    goto &{ $m->can('__get_path_node') } # Slow path
	if !($m->[ _arity ] == 2 && @_ == 3 && !($f & (_HYPER|_REF|_UNIQ)));
    &Graph::AdjacencyMap::__arg;
    return unless exists $m->[ _s ]->{ $_[1] };
    my $p = [ $m->[ _s ], $m->[ _s ]->{ $_[1] } ];
    my $l = $_[2];
    exists $p->[-1]->{ $l } ? ( $p->[-1]->{ $l }, $p, [ @_[1,2] ], $l ) : ();
}

sub _get_path_count {
    my $m = $_[0];
    return undef unless my ($n) = &{ $m->can('_get_path_node') };
    my $f = $m->[ _f ];
    return
        ($f & _COUNT) ? $n->[ _nc ] :
        ($f & _MULTI) ? scalar keys %{ $n->[ _nm ] } : 1;
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
	return undef unless my ($n) = &{ $m->can('__get_path_node') };
	return 0 if !ref $n;
	my $e = _na == $#$n && keys %{ $n->[ _na ] } ? 1 : 0;
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
    Graph::__carp_confess(sprintf "arguments %d expected %d for\n".$m->stringify,
		  @_ - 1, $m->[ _arity ])
        if !($f & _HYPER) and @_ - 1 != $m->[ _arity ];
    return if !($f & _UNORDUNIQ);
    my @a = @_[1..$#_];
    my %u;
    @a = grep !$u{$_}++, @a if $f & _UNIQ;
    # Alphabetic or numeric sort, does not matter as long as it unifies.
    @_ = ($_[0], $f & _UNORD ? sort @a : @a);
}
*__attr = \&__arg;

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
