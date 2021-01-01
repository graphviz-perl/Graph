package Graph;

use strict;
use warnings;

sub __carp_confess { require Carp; Carp::confess(@_) }
BEGIN {
    if (0) { # SET THIS TO ZERO FOR TESTING AND RELEASES!
	$SIG{__DIE__ } = \&__carp_confess;
	$SIG{__WARN__} = \&__carp_confess;
    }
}

use Graph::AdjacencyMap qw(:flags :fields);

our $VERSION = '0.9716';

require 5.006; # Weak references are absolutely required.

my $can_deep_copy_Storable;
sub _can_deep_copy_Storable () {
    return $can_deep_copy_Storable if defined $can_deep_copy_Storable;
    return $can_deep_copy_Storable = 0 if $] < 5.010; # no :load tag Safe 5.8
    eval {
        require Storable;
        require B::Deparse;
        Storable->VERSION(2.05);
        B::Deparse->VERSION(0.61);
    };
    $can_deep_copy_Storable = !$@;
}

sub _F () { 0 } # Flags.
sub _G () { 1 } # Generation.
sub _V () { 2 } # Vertices.
sub _E () { 3 } # Edges.
sub _A () { 4 } # Attributes.
sub _U () { 5 } # Union-Find.
sub _S () { 6 } # Successors cache, [1] is the graph generation
sub _P () { 7 } # Predecessors cache, not made if undirected

my $Inf;

BEGIN {
  if ($] >= 5.022) {
    $Inf = eval '+"Inf"'; # uncoverable statement
  } else {
    local $SIG{FPE}; # uncoverable statement
    eval { $Inf = exp(999) } || # uncoverable statement
	eval { $Inf = 9**9**9 } || # uncoverable statement
	    eval { $Inf = 1e+999 } || # uncoverable statement
		{ $Inf = 1e+99 }; # uncoverable statement
                # Close enough for most practical purposes.
  }
}

sub Infinity () { $Inf }

# Graphs are blessed array references.
# - The first element contains the flags.
# - The second element is the vertices.
# - The third element is the edges.
# - The fourth element is the attributes of the whole graph.
# The defined flags for Graph are:
# - unionfind
# The vertices are contained in a "simplemap"
# (if no attributes) or in a "map".
# The edges are always in a "map".
# The defined flags for maps are:
# - _COUNT for countedness: more than one instance
#   expects one for vertices and two for edges
# - _UNORD for unordered coordinates (a set): if _UNORD is not set
#   the coordinates are assumed to be meaningfully ordered
# - _UNIQ for unique coordinates: if set duplicates are removed,
#   if not, duplicates are assumed to meaningful
# - _UNORDUNIQ: just a union of _UNORD and UNIQ
# Vertices and edges assume none of these flags.

use Graph::Attribute array => _A, map => 'graph';

sub stringify {
    my $u = &is_undirected;
    my $e = $u ? '=' : '-';
    my @s = sort map join($e, $u ? sort { "$a" cmp "$b" } @$_ : @$_), &_edges05;
    push @s, sort { "$a" cmp "$b" } &isolated_vertices;
    join(",", @s);
}

sub eq {
    "$_[0]" eq "$_[1]"
}

sub boolify {
  1;  # Important for empty graphs: they stringify to "", which is false.
}

sub ne {
    "$_[0]" ne "$_[1]"
}

use overload
    '""' => \&stringify,
    'bool' => \&boolify,
    'eq' => \&eq,
    'ne' => \&ne;

sub _opt {
    my ($opt, $flags, %flags) = @_;
    while (my ($flag, $FLAG) = each %flags) {
	$$flags |= $FLAG if delete $opt->{$flag};
	$$flags &= ~$FLAG if delete $opt->{"non$flag"};
    }
}

sub has_union_find {
    my ($g) = @_;
    ($g->[ _F ] & _UNIONFIND) && defined $g->[ _U ];
}

sub _get_union_find {
    my ($g) = @_;
    $g->[ _U ];
}

sub _opt_get {
    my ($opt, $key, $var) = @_;
    return if !exists $opt->{$key};
    $$var = delete $opt->{$key};
}

sub _opt_unknown {
    my ($opt) = @_;
    return unless my @opt = keys %$opt;
    __carp_confess sprintf
        "@{[(caller(1))[3]]}: Unknown option%s: @{[map qq['$_'], sort @opt]}",
        @opt > 1 ? 's' : '';
}

sub new {
    my ($class, @args) = @_;
    my $gflags = 0;
    my $vflags = 0;
    my $eflags = 0;
    my %opt = _get_options( \@args );

    if (ref $class && $class->isa('Graph')) {
	my %existing;
	no strict 'refs';
        for my $c (qw(undirected refvertexed
                      countvertexed multivertexed
                      hyperedged countedged multiedged
		      __stringified)) {
	    $existing{$c}++ if $class->$c;
        }
	$existing{unionfind}++ if $class->has_union_find;
	%opt = (%existing, %opt) if %existing; # allow overrides
    }

    $opt{undirected} = !delete $opt{directed} if exists $opt{directed};

    _opt(\%opt, \$vflags,
	 countvertexed	=> _COUNT,
	 multivertexed	=> _MULTI,
	 refvertexed	=> _REF,
	 refvertexed_stringified => _REFSTR ,
	 __stringified => _STR,
	);

    _opt(\%opt, \$eflags,
	 countedged	=> _COUNT,
	 multiedged	=> _MULTI,
	 undirected	=> _UNORD,
	);
    my $is_hyper = delete $opt{hyperedged};

    _opt(\%opt, \$gflags,
	 unionfind     => _UNIONFIND,
	);

    my @V;
    if ($opt{vertices}) {
	__carp_confess "Graph: vertices should be an array ref"
	    if ref $opt{vertices} ne 'ARRAY';
	@V = @{ delete $opt{vertices} };
    }

    my @E;
    if ($opt{edges}) {
        __carp_confess "Graph: edges should be an array ref of array refs"
	    if ref $opt{edges} ne 'ARRAY';
	@E = @{ delete $opt{edges} };
    }

    _opt_unknown(\%opt);

    __carp_confess "Graph: both countvertexed and multivertexed"
	if ($vflags & _COUNT) && ($vflags & _MULTI);

    __carp_confess "Graph: both countedged and multiedged"
	if ($eflags & _COUNT) && ($eflags & _MULTI);

    my $g = bless [ ], ref $class || $class;

    $g->[ _F ] = $gflags;
    $g->[ _G ] = 0;
    $g->[ _V ] = $vflags ?
	_am_heavy($vflags, 1) :
	    _am_light($vflags, 1);
    $g->[ _E ] = ($is_hyper or $eflags & ~_UNORD) ?
	_am_heavy($eflags, $is_hyper ? undef : 2) :
	    _am_light($eflags, 2);

    $g->add_vertices(@V) if @V;

    for my $e (@E) {
	__carp_confess "Graph: edges should be array refs"
	    if ref $e ne 'ARRAY';
	$g->add_edge(@$e);
    }

    $g->[ _U ] = do { require Graph::UnionFind; Graph::UnionFind->new }
	if $gflags & _UNIONFIND;

    return $g;
}

sub _am_light {
    require Graph::AdjacencyMap::Light;
    Graph::AdjacencyMap::Light->_new(@_);
}

sub _am_heavy {
    Graph::AdjacencyMap->_new(@_);
}

sub countvertexed { $_[0]->[ _V ]->_is_COUNT }
sub multivertexed { $_[0]->[ _V ]->_is_MULTI }
sub refvertexed   { $_[0]->[ _V ]->_is_REF   }
sub refvertexed_stringified { $_[0]->[ _V ]->_is_REFSTR }
sub __stringified { $_[0]->[ _V ]->_is_STR   }

sub countedged    { $_[0]->[ _E ]->_is_COUNT }
sub multiedged    { $_[0]->[ _E ]->_is_MULTI }
sub hyperedged    { !defined $_[0]->[ _E ]->[ _arity ] }
sub undirected    { $_[0]->[ _E ]->_is_UNORD }

sub directed { ! $_[0]->[ _E ]->_is_UNORD }

*is_directed      = \&directed;
*is_undirected    = \&undirected;

*is_countvertexed = \&countvertexed;
*is_multivertexed = \&multivertexed;
*is_refvertexed   = \&refvertexed;
*is_refvertexed_stringified = \&refvertexed_stringified;

*is_countedged    = \&countedged;
*is_multiedged    = \&multiedged;
*is_hyperedged    = \&hyperedged;

sub _union_find_add_vertex {
    $_[0]->[ _U ]->add( &_vertex_ids );
}

sub add_vertex {
    __carp_confess "Graph::add_vertex: use add_vertices for more than one vertex" if @_ != 2;
    __carp_confess "Graph::add_vertex: undef vertex" if grep !defined, @_;
    my $g = $_[0];
    my $V = $g->[ _V ];
    if (&is_multivertexed) {
	push @_, _GEN_ID;
	goto &add_vertex_by_id;
    }
    return $g if !&is_countvertexed and $V->has_path([$_[1]]);
    $V->set_path([@_[1..$#_]]);
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $g;
}

sub has_vertex {
    my $g = $_[0];
    my $V = $g->[ _V ];
    return exists $V->[ _s ]->{ $_[1] } if ($V->[ _f ] & _LIGHT);
    $V->has_path([$_[1]]);
}

sub _vertices05 {
    my $g = $_[0];
    my @v = $g->[ _V ]->paths;
    return scalar @v if !wantarray;
    map @$_, @v;
}

sub vertices {
    my $g = $_[0];
    my @v = &_vertices05;
    return @v if !(&is_multivertexed || &is_countvertexed);
    return map +(($_) x $g->get_vertex_count($_)), @v if wantarray;
    my $V = 0;
    $V += $g->get_vertex_count($_) for @v;
    return $V;
}

*unique_vertices = \&_vertices05;

sub has_vertices {
    my $g = shift;
    scalar $g->[ _V ]->has_paths( @_ );
}

sub _vertex_ids_ensure {
    push @_, 1;
    goto &_vertex_ids_maybe_ensure;
}

sub _vertex_ids_ensure_multi {
    my $id = pop;
    my @i = &_vertex_ids_ensure;
    push @_, $id;
    @i ? (@i, $id) : ();
}

sub _union_find_add_edge {
    my ($g, $u, $v) = @_;
    $g->[ _U ]->union($u, $v);
}

sub add_edge {
    &expect_hyperedged, &expect_undirected if @_ != 3;
    my $g = $_[0];
    if (&is_multiedged) {
	push @_, _GEN_ID;
	goto &add_edge_by_id;
    }
    my @i = &_vertex_ids_ensure;
    @i = sort @i if &is_undirected;
    $g->[ _E ]->set_path( \@i );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @i ) if &has_union_find;
    return $g;
}

sub _vertex_ids_multi {
    my $id = pop;
    my @i = &_vertex_ids;
    push @_, $id;
    @i ? (@i, $id) : ();
}

sub _vertex_ids {
    push @_, 0;
    goto &_vertex_ids_maybe_ensure;
}

sub _vertex_ids_maybe_ensure {
    my $ensure = pop;
    my ($g, @args) = @_;
    my $V = $g->[ _V ];
    if (($V->[ _f ] & _LIGHT)) {
	my $s = $V->[ _s ];
	my @non_exist = grep !exists $s->{ $_ }, @args;
	return if !$ensure and @non_exist;
	$g->add_vertices(@non_exist) if @non_exist;
	return map $s->{ $_ }, @args;
    }
    @args = map [$_], @args;
    my @non_exist = $V->paths_non_existing(\@args);
    return if !$ensure and @non_exist;
    $g->add_vertices(map @$_, @non_exist) if @non_exist;
    $V->get_ids_by_paths(\@args);
}

sub has_edge {
    my $g = $_[0];
    my $E = $g->[ _E ];
    my ($Ef, $Ea) = @$E[ _f, _arity ];
    return 0 if defined($Ea) and @_ != $Ea + 1;
    return 0 if (my @i = &_vertex_ids) != @_ - 1;
    @i = sort @i if &is_undirected;
    return $E->get_ids_by_paths([ \@i ]) if !(@i == 2 && !(!defined($Ea) or $Ef & (_REF|_UNIQ))); # Slow path.
    my $s = $E->[ _s ];
    return exists $s->{ $i[0] } &&
	   exists $s->{ $i[0] }->{ $i[1] } ? 1 : 0;
}

sub any_edge {
    my $g = $_[0];
    my $E = $g->[ _E ];
    my $Ef = $E->[ _f ];
    return 0 if (my @i = &_vertex_ids) != @_ - 1;
    @i = sort @i if &is_undirected;
    &_edge_cache;
    my $N0 = $g->[ _S ][0];
    my $s = $N0->{ $i[0] };
    grep $_ == $i[1], map @$_, @{ $s };
}

sub _edges05 {
    my $g = $_[0];
    my @e = $g->[ _E ]->paths;
    return @e if !wantarray;
    map [ map $_->[0], @$_ ], $g->[ _V ]->get_paths_by_ids(\@e);
}

*unique_edges = \&_edges05;

sub edges {
    my $g = $_[0];
    my @e = &_edges05;
    return @e if !(&is_multiedged || &is_countedged);
    return map +(($_) x $g->get_edge_count(@$_)), @e if wantarray;
    my $E = 0;
    $E += $g->get_edge_count(@$_) for @e;
    return $E;
}

sub has_edges {
    scalar $_[0]->[ _E ]->has_paths;
}

###
# by_id
#

sub add_vertex_by_id {
    &expect_multivertexed;
    my ($g, $v, $id) = @_;
    my $V = $g->[ _V ];
    return $g if $V->has_path_by_multi_id( my @args = ([$v], $id) );
    $V->set_path_by_multi_id( @args );
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $g;
}

sub add_vertex_get_id {
    &expect_multivertexed;
    my ($g, $v) = @_;
    my $id = $g->[ _V ]->set_path_by_multi_id( [$v], _GEN_ID );
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $id;
}

sub has_vertex_by_id {
    &expect_multivertexed;
    my ($g, $v, $id) = @_;
    $g->[ _V ]->has_path_by_multi_id( [$v], $id );
}

sub delete_vertex_by_id {
    &expect_multivertexed;
    &expect_non_unionfind;
    my ($g, $v, $id) = @_;
    return $g unless &has_vertex_by_id;
    # TODO: what to about the edges at this vertex?
    # If the multiness of this vertex goes to zero, delete the edges?
    $g->[ _V ]->del_path_by_multi_id( [$v], $id );
    $g->[ _G ]++;
    return $g;
}

sub get_multivertex_ids {
    &expect_multivertexed;
    my $g = shift;
    $g->[ _V ]->get_multi_ids( \@_ );
}

sub add_edge_by_id {
    &expect_multiedged;
    my $g = $_[0];
    my @i = &_vertex_ids_ensure_multi;
    my $id = pop @i;
    @i = sort @i if &is_undirected;
    $g->[ _E ]->set_path_by_multi_id( \@i, $id );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @i ) if &has_union_find;
    return $g;
}

sub add_edge_get_id {
    &expect_multiedged;
    my $g = $_[0];
    my @i = &_vertex_ids_ensure;
    @i = sort @i if &is_undirected;
    my $id = $g->[ _E ]->set_path_by_multi_id( \@i, _GEN_ID );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @i ) if &has_union_find;
    return $id;
}

sub has_edge_by_id {
    &expect_multiedged;
    my $g = $_[0];
    my @i = &_vertex_ids_multi;
    return 0 if @i < @_ - 2;
    my $id = pop @i;
    @i = sort @i if &is_undirected;
    $g->[ _E ]->has_path_by_multi_id( \@i, $id );
}

sub delete_edge_by_id {
    &expect_multiedged;
    &expect_non_unionfind;
    my $g = $_[0];
    my $E = $g->[ _E ];
    my @i = &_vertex_ids_multi;
    return if @i < @_ - 2;
    my $id = pop @i;
    @i = sort @i if &is_undirected;
    return unless $E->has_path_by_multi_id( my @args = (\@i, $id) );
    $E->del_path_by_multi_id( @args );
    $g->[ _G ]++;
    return $g;
}

sub get_multiedge_ids {
    &expect_multiedged;
    return unless @_-1 == (my @i = &_vertex_ids);
    $_[0]->[ _E ]->get_multi_ids( \@i );
}

###
# Neighbourhood.
#

sub _edges_at {
    goto &_edges_from if &is_undirected;
    require Set::Object;
    Set::Object->new(&_edges_from, &_edges_to)->${ wantarray ? \'members' : \'size' };
}

sub _edge_cache {
    my $g = $_[0];
    return if $g->[ _S ] && $g->[ _S ][1] == $g->[ _G ];
    require Set::Object unless my $directed = &is_directed;
    $g->[ _S ] = [ my $S0 = {}, $g->[ _G ] ];
    $g->[ _P ] = [ my $P0 = {} ]; # only store generation in _S
    my $Ei = $g->[ _E ]->_ids;
    for (my $ei = $#$Ei; $ei >= 0; $ei--) {
	next if !defined(my $ev = $Ei->[$ei]);
	next unless @$ev;
	if ($directed) {
	    my ($f, $t) = @$ev;
	    push @{ $S0->{ $f } }, $ev;
	    push @{ $P0->{ $t } }, $ev;
	} else {
	    my @e = Set::Object->new(@$ev)->members;
	    if (@e == 1) {
		push @{ $S0->{ $e[0] } }, [ @e, @e ];
		next;
	    }
	    for my $i (0..$#e) {
		my ($f, @r) = _list_with_x_first($i, @e);
		push @{ $S0->{ $f } }, [ $f, @r ];
	    }
	}
    }
}

sub _list_with_x_first {
    return if @_ == 1;
    my $i = shift;
    ($_[$i], @_[0..$i-1], @_[$i+1..$#_]);
}

sub _edges {
    &_edge_cache;
    my $n = pop;
    my $N0 = $_[0]->[ $n ][0];
    map @{ $N0->{ $_ } || [] }, &_vertex_ids;
}

sub _edges_from {
    push @_, _S;
    goto &_edges;
}

sub _edges_to {
    goto &_edges_from if &is_undirected;
    push @_, _P;
    goto &_edges;
}

sub edges_at {
    goto &_edges_at if !wantarray;
    map [ map $_->[0], @$_ ], $_[0]->[ _V ]->get_paths_by_ids([ &_edges_at ]);
}

sub edges_from {
    goto &_edges_from if !wantarray;
    map [ map $_->[0], @$_ ], $_[0]->[ _V ]->get_paths_by_ids([ &_edges_from ]);
}

sub edges_to {
    goto &edges_from if &is_undirected;
    goto &_edges_to if !wantarray;
    map [ map $_->[0], @$_ ], $_[0]->[ _V ]->get_paths_by_ids([ &_edges_to ]);
}

sub successors {
    goto &_edges_from if !wantarray;
    my @v = map [ @$_[ 1 .. $#$_ ] ], &_edges_from;
    map @$_, map @$_, $_[0]->[ _V ]->get_paths_by_ids(\@v);
}

sub predecessors {
    goto &_edges_to if !wantarray;
    goto &successors if &is_undirected;
    my @v = map [ @$_[ 0 .. $#$_-1 ] ], &_edges_to;
    map @$_, map @$_, $_[0]->[ _V ]->get_paths_by_ids(\@v);
}

sub _cessors_by_radius {
    my ($radius, $method, $self_only_if_loop) = splice @_, -3, 3;
    my ($g, @v) = @_;
    require Set::Object;
    my ($init, $next) = map Set::Object->new(@v), 1..2;
    my $self = Set::Object->new(grep $g->has_edge($_, $_), @v) if $self_only_if_loop;
    my ($got, $found) = map Set::Object->new, 1..2;
    while (!defined $radius or $radius-- > 0) {
	$found->insert($g->$method($next->members));
	$next = $found->difference($got);
	last if $next->is_null;  # Leave if no new found.
	$got->insert($next->members);
	$found->clear;
    }
    $got->remove($init->difference($self)->members) if $self_only_if_loop;
    $got->members;
}

sub all_successors {
    &expect_directed;
    push @_, undef, 'successors', 0;
    goto &_cessors_by_radius;
}

sub successors_by_radius {
    &expect_directed;
    push @_, 'successors', 0;
    goto &_cessors_by_radius;
}

sub all_predecessors {
    &expect_directed;
    push @_, undef, 'predecessors', 0;
    goto &_cessors_by_radius;
}

sub predecessors_by_radius {
    &expect_directed;
    push @_, 'predecessors', 0;
    goto &_cessors_by_radius;
}

sub neighbours_by_radius {
    push @_, 'neighbours', 1;
    goto &_cessors_by_radius;
}
*neighbors_by_radius = \&neighbours_by_radius;

sub neighbours {
    require Set::Object;
    my $s = Set::Object->new(map @$_[1..$#$_], &_edges_from);
    $s->insert(map @$_[0..$#$_-1], &_edges_to) if &is_directed;
    map @$_, map @$_, $_[0]->[ _V ]->get_paths_by_ids([ map [$_], $s->members ]);
}
*neighbors = \&neighbours;

sub all_neighbours {
    push @_, undef, 'neighbours', 1;
    goto &_cessors_by_radius;
}
*all_neighbors = \&all_neighbours;

sub all_reachable {
    &directed ? goto &all_successors : goto &all_neighbors;
}

sub reachable_by_radius {
    &directed ? goto &successors_by_radius : goto &neighbors_by_radius;
}

sub delete_edge {
    &expect_non_unionfind;
    my $g = $_[0];
    my @i = &_vertex_ids;
    @i = sort @i if &is_undirected;
    return $g unless @i and $g->[ _E ]->del_path( \@i );
    $g->[ _G ]++;
    return $g;
}

sub delete_vertex {
    &expect_non_unionfind;
    my $g = $_[0];
    return $g if @_ != 2;
    my $V = $g->[ _V ];
    return $g unless $V->has_path([$_[1]]);
    # TODO: _edges_at is excruciatingly slow (rt.cpan.org 92427)
    my $E = $g->[ _E ];
    $E->del_path( $_ ) for &_edges_at;
    $V->del_path([$_[1]]);
    $g->[ _G ]++;
    return $g;
}

sub get_vertex_count {
    my $g = shift;
    $g->[ _V ]->_get_path_count( \@_ ) || 0;
}

sub get_edge_count {
    my $g = $_[0];
    my @i = &_vertex_ids;
    return 0 unless @i;
    @i = sort @i if &is_undirected;
    $g->[ _E ]->_get_path_count( \@i ) || 0;
}

sub delete_vertices {
    &expect_non_unionfind;
    my $g = shift;
    while (@_) {
	my $v = shift @_;
	$g->delete_vertex($v);
    }
    return $g;
}

sub delete_edges {
    &expect_non_unionfind;
    my $g = shift;
    while (@_) {
	my ($u, $v) = splice @_, 0, 2;
	$g->delete_edge($u, $v);
    }
    return $g;
}

###
# Degrees.
#

sub in_degree {
    my $g = $_[0];
    return undef unless @_ > 1 && &has_vertex;
    my $in = 0;
    $in += $g->get_edge_count( @$_ ) for &edges_to;
    $in++ if &is_undirected and &is_self_loop_vertex;
    return $in;
}

sub out_degree {
    my $g = $_[0];
    return undef unless @_ > 1 && &has_vertex;
    my $out = 0;
    $out += $g->get_edge_count( @$_ ) for &edges_from;
    $out++ if &is_undirected and &is_self_loop_vertex;
    return $out;
}

sub _total_degree {
    return undef unless @_ > 1 && &has_vertex;
    &is_undirected ? &in_degree : &in_degree - &out_degree;
}

sub degree {
    goto &_total_degree if @_ > 1;
    return 0 if &is_directed;
    my $g = $_[0];
    my $total = 0;
    $total += $g->_total_degree( $_ ) for &_vertices05;
    return $total;
}

*vertex_degree = \&degree;

sub is_sink_vertex {
    return 0 unless @_ > 1;
    &successors == 0 && &predecessors > 0;
}

sub is_source_vertex {
    return 0 unless @_ > 1;
    &predecessors == 0 && &successors > 0;
}

sub is_successorless_vertex {
    return 0 unless @_ > 1;
    &successors == 0;
}

sub is_predecessorless_vertex {
    return 0 unless @_ > 1;
    &predecessors == 0;
}

sub is_successorful_vertex {
    return 0 unless @_ > 1;
    &successors > 0;
}

sub is_predecessorful_vertex {
    return 0 unless @_ > 1;
    &predecessors > 0;
}

sub is_isolated_vertex {
    return 0 unless @_ > 1;
    &predecessors == 0 && &successors == 0;
}

sub is_interior_vertex {
    return 0 unless @_ > 1;
    my $s = &successors;
    $s-- if my $isl = &is_self_loop_vertex;
    return 0 if $s == 0;
    return $s > 0 if &is_undirected;
    my $p = &predecessors;
    $p-- if $isl;
    $p > 0;
}

sub is_exterior_vertex {
    return 0 unless @_ > 1;
    &predecessors == 0 || &successors == 0;
}

sub is_self_loop_vertex {
    return 0 unless @_ > 1;
    return 1 if grep $_ eq $_[1], &successors; # @todo: multiedges
    return 0;
}

for my $p (qw(
    is_sink_vertex
    is_source_vertex
    is_successorless_vertex
    is_predecessorless_vertex
    is_successorful_vertex
    is_predecessorful_vertex
    is_isolated_vertex
    is_interior_vertex
    is_exterior_vertex
    is_self_loop_vertex
)) {
    no strict 'refs';
    (my $m = $p) =~ s/^is_(.*)ex$/${1}ices/;
    *$m = sub { my $g = $_[0]; grep $g->$p($_), &_vertices05 };
}

###
# Paths and cycles.
#

sub add_path {
    my $g = shift;
    my $u = shift;
    while (@_) {
	my $v = shift;
	$g->add_edge($u, $v);
	$u = $v;
    }
    return $g;
}

sub delete_path {
    &expect_non_unionfind;
    my $g = shift;
    my $u = shift;
    while (@_) {
	my $v = shift;
	$g->delete_edge($u, $v);
	$u = $v;
    }
    return $g;
}

sub has_path {
    my $g = shift;
    my $u = shift;
    while (@_) {
	my $v = shift;
	return 0 unless $g->has_edge($u, $v);
	$u = $v;
    }
    return $g;
}

sub add_cycle {
    push @_, $_[1];
    goto &add_path;
}

sub delete_cycle {
    &expect_non_unionfind;
    push @_, $_[1];
    goto &delete_path;
}

sub has_cycle {
    return 0 if @_ == 1;
    push @_, $_[1];
    goto &has_path;
}

*has_this_cycle = \&has_cycle;

sub has_a_cycle {
    my $g = shift;
    require Graph::Traversal::DFS;
    my $t = Graph::Traversal::DFS->new($g, has_a_cycle => 1, @_);
    $t->dfs;
    return $t->get_state('has_a_cycle');
}

sub find_a_cycle {
    require Graph::Traversal::DFS;
    my @r = ( back_edge => \&Graph::Traversal::find_a_cycle);
    push @r,
      down_edge => \&Graph::Traversal::find_a_cycle
	if &is_undirected;
    my $g = shift;
    my $t = Graph::Traversal::DFS->new($g, @r, @_);
    $t->dfs;
    $t->has_state('a_cycle') ? @{ $t->get_state('a_cycle') } : ();
}

###
# Attributes.

my @generic_methods = (
    [ 'set_attribute', \&_set_attribute ],
    [ 'set_attributes', \&_set_attributes ],
    [ 'has_attributes', \&_has_attributes ],
    [ 'has_attribute', \&_has_attribute ],
    [ 'get_attributes', \&_get_attributes ],
    [ 'get_attribute', \&_get_attribute ],
    [ 'get_attribute_names', \&_get_attribute_names ],
    [ 'get_attribute_values', \&_get_attribute_values ],
    [ 'delete_attributes', \&_delete_attributes ],
    [ 'delete_attribute', \&_delete_attribute ],
);
my %entity2offset = (vertex => _V, edge => _E);
my %entity2args = (edge => '_vertex_ids');
for my $entity (qw(vertex edge)) {
    no strict 'refs';
    my $expect_non = \&{ "expect_non_multi${entity}" };
    my $expect_yes = \&{ "expect_multi${entity}" };
    my $args_non = \&{ $entity2args{$entity} } if $entity2args{$entity};
    my $args_yes = \&{ $entity2args{$entity}.'_multi' } if $entity2args{$entity};
    my $offset = $entity2offset{$entity};
    for my $t (@generic_methods) {
	my ($raw, $func) = @$t;
	my ($first, $rest) = ($raw =~ /^(\w+?)_(.+)/);
	my $m = join '_', $first, $entity, $rest;
	*$m = sub {
	    &$expect_non; push @_, 0, $entity, $offset, $args_non; goto &$func;
	};
	*{$m.'_by_id'} = sub {
	    &$expect_yes; push @_, 1, $entity, $offset, $args_yes; goto &$func;
	};
    }
}

sub _munge_args {
    my ($is_multi, $is_undirected, @args) = @_;
    return \@args if !$is_undirected and !$is_multi;
    return [ sort @args ] if !$is_multi;
    my $id = pop @args;
    ($is_undirected ? [ sort @args ] : \@args, $id);
}

sub _set_attribute {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    my $value = pop;
    my $attr = pop;
    no strict 'refs';
    &{ 'add_' . $entity . ($is_multi ? '_by_id' : '') } unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_set_path_attr( @args, $attr, $value );
}

sub _set_attributes {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    my $attr = pop;
    no strict 'refs';
    &{ 'add_' . $entity . ($is_multi ? '_by_id' : '') } unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_set_path_attrs( @args, $attr );
}

sub _has_attributes {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    return 0 unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_has_path_attrs( @args );
}

sub _has_attribute {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    my $attr = pop;
    no strict 'refs';
    return 0 unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_has_path_attr( @args, $attr );
}

sub _get_attributes {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    return undef unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    scalar $_[0]->[ $offset ]->_get_path_attrs( @args );
}

sub _get_attribute {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    my $attr = pop;
    return undef unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    scalar $_[0]->[ $offset ]->_get_path_attr( @args, $attr );
}

sub _get_attribute_names {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    return unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_get_path_attr_names( @args );
}

sub _get_attribute_values {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    return unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_get_path_attr_values( @args );
}

sub _delete_attributes {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    no strict 'refs';
    return undef unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_del_path_attrs( @args );
}

sub _delete_attribute {
    my ($is_multi, $entity, $offset, $args) = splice @_, -4, 4;
    my $attr = pop;
    no strict 'refs';
    return undef unless &{ 'has_' . $entity . ($is_multi ? '_by_id' : '') };
    my @args = ($entity eq 'edge') ? &$args : @_[1..$#_];
    @args = _munge_args($is_multi, &is_undirected, @args);
    $_[0]->[ $offset ]->_del_path_attr( @args, $attr );
}

sub add_vertices {
    my $g = $_[0];
    $g->add_vertex( $_ ) for @_[1..$#_];
    return $g;
}

sub add_edges {
    my $g = shift;
    while (@_) {
	my $u = shift @_;
	if (ref $u eq 'ARRAY') {
	    $g->add_edge( @$u );
	} else {
	    __carp_confess "Graph::add_edges: missing end vertex" if !@_;
	    my $v = shift @_;
	    $g->add_edge( $u, $v );
	}
    }
    return $g;
}

sub rename_vertex {
    my $g = shift;
    $g->[ _V ]->rename_path(@_);
    return $g;
}

sub rename_vertices {
    my ($g, $code) = @_;
    my %seen;
    $g->rename_vertex($_, $code->($_))
	for grep !$seen{$_}++, map @$_, $g->[ _V ]->paths;
    return $g;
}

sub as_hashes {
    my ($g) = @_;
    my (%n, %e, @e);
    my ($is_hyper, $is_directed)= (&is_hyperedged, &is_directed);
    if (&is_multivertexed) {
        for my $v ($g->vertices) {
            $n{$v} = {
                map +($_ => $g->get_vertex_attributes_by_id($v, $_) || {}),
                    $g->get_multivertex_ids($v)
            };
        }
    } else {
        %n = map +($_ => $g->get_vertex_attributes($_) || {}), $g->vertices;
    }
    if (&is_multiedged) {
        for my $e ($g->edges) {
            if ($is_hyper) {
                my %h = (attributes => {
                    map +($_ => $g->get_edge_attributes_by_id(@$e, $_) || {}),
                        $g->get_multiedge_ids(@$e)
                });
                if ($is_directed) {
                } else {
                    $h{vertices} = $e;
                }
                push @e, \%h;
            } else {
                $e{ $e->[0] }{ $e->[1] } = {
                    map +($_ => $g->get_edge_attributes_by_id(@$e, $_) || {}),
                        $g->get_multiedge_ids(@$e)
                };
            }
        }
    } else {
	for ($g->edges) {
	    $e{ $_->[0] }{ $_->[1] } = $g->get_edge_attributes(@$_) || {};
	    $e{ $_->[1] }{ $_->[0] } = $g->get_edge_attributes(@$_) || {}
		if !$is_directed;
	}
    }
    ( \%n, $is_hyper ? \@e : \%e );
}

sub ingest {
    my ($g, $g2) = @_;
    for my $v ($g2->vertices) {
        if (&is_multivertexed) {
            $g->set_vertex_attributes_by_id($v, $_, $g2->get_vertex_attributes_by_id($v, $_))
                for $g2->get_multivertex_ids($v);
        } else {
            $g->set_vertex_attributes($v, $g2->get_vertex_attributes($v));
        }
        if (&is_multiedged) {
            for my $e ($g2->edges_from($v)) {
                $g->set_edge_attributes_by_id(@$e, $_, $g2->get_edge_attributes_by_id(@$e, $_))
                    for $g2->get_multiedge_ids(@$e);
            }
        } else {
            $g->set_edge_attributes(@$_, $g2->get_edge_attributes(@$_))
                for $g2->edges_from($v);
        }
    }
    $g;
}

###
# More constructors.
#

sub copy {
    my ($g, @args) = @_;
    my %opt = _get_options( \@args );
    no strict 'refs';
    my $c =
	(ref $g)->new(map +($_ => &{$_} ? 1 : 0),
		      qw(directed
			 refvertexed
			 countvertexed
			 multivertexed
			 hyperedged
			 countedged
			 multiedged
		         __stringified));
    $c->add_vertex($_) for &isolated_vertices;
    $c->add_edge(@$_) for &_edges05;
    return $c;
}

*copy_graph = \&copy;

sub _deep_copy_Storable {
    my $g = shift;
    require Safe;   # For deep_copy().
    my $safe = Safe->new;
    $safe->permit(qw/:load/);
    local $Storable::Deparse = 1;
    local $Storable::Eval = sub { $safe->reval($_[0]) };
    return Storable::thaw(Storable::freeze($g));
}

sub _deep_copy_DataDumper {
    my $g = shift;
    require Data::Dumper;
    my $d = Data::Dumper->new([$g]);
    use vars qw($VAR1);
    $d->Purity(1)->Terse(1)->Deepcopy(1);
    $d->Deparse(1) if $] >= 5.008;
    eval $d->Dump;
}

sub deep_copy {
    if (_can_deep_copy_Storable()) {
	return _deep_copy_Storable(@_); # uncoverable statement
    } else {
	return _deep_copy_DataDumper(@_); # uncoverable statement
    }
}

*deep_copy_graph = \&deep_copy;

sub transpose_edge {
    my $g = $_[0];
    return $g if !&is_directed;
    return undef unless &has_edge;
    my $c = &get_edge_count;
    my $a = &get_edge_attributes;
    my @e = reverse @_[1..$#_];
    &delete_edge unless $g->has_edge( @e );
    $g->add_edge( @e ) for 1..$c;
    $g->set_edge_attributes(@e, $a) if $a;
    return $g;
}

sub transpose_graph {
    my $t = &copy;
    return $t if !&directed;
    $t->transpose_edge(@$_) for &_edges05;
    return $t;
}

*transpose = \&transpose_graph;

sub complete_graph {
    my $directed = &is_directed;
    my $c = &new;
    my @v = &_vertices05;
    for (my $i = $#v; $i >= 0; $i-- ) {
	for (my $j = $i - 1; $j >= 0; $j-- ) {
	    $c->add_edge($v[$i], $v[$j]);
	    $c->add_edge($v[$j], $v[$i]) if $directed;
	}
    }
    return $c;
}

*complement = \&complement_graph;

sub complement_graph {
    my $c = &complete_graph;
    $c->delete_edge(@$_) for &edges;
    return $c;
}

*complete = \&complete_graph;

sub subgraph {
  my ($g, $src, $dst) = @_;
  __carp_confess "Graph::subgraph: need src and dst array references"
    unless ref $src eq 'ARRAY' && (!defined($dst) or ref $dst eq 'ARRAY');
  require Set::Object;
  my $s = $g->new;
  my @u = grep $g->has_vertex($_), @$src;
  my $v = Set::Object->new($dst ? grep $g->has_vertex($_), @$dst : @u);
  $s->add_vertices(@u, $dst ? $v->members : ());
  $s->add_edges(grep $v->contains($_->[1]), $g->edges_from(@u));
  return $s;
}

###
# Transitivity.
#

sub is_transitive {
    my $g = shift;
    require Graph::TransitiveClosure;
    Graph::TransitiveClosure::is_transitive($g);
}

###
# Weighted vertices.
#

my $defattr = 'weight';

sub _defattr {
    return $defattr;
}

sub add_weighted_vertex {
    &expect_non_multivertexed;
    push @_, $defattr, pop;
    goto &set_vertex_attribute;
}

sub add_weighted_vertices {
    &expect_non_multivertexed;
    my $g = shift;
    while (@_) {
	my ($v, $w) = splice @_, 0, 2;
	$g->add_vertex($v);
	$g->set_vertex_attribute($v, $defattr, $w);
    }
}

sub get_vertex_weight {
    &expect_non_multivertexed;
    push @_, $defattr;
    goto &get_vertex_attribute;
}

sub has_vertex_weight {
    &expect_non_multivertexed;
    push @_, $defattr;
    goto &has_vertex_attribute;
}

sub set_vertex_weight {
    &expect_non_multivertexed;
    push @_, $defattr, pop;
    goto &set_vertex_attribute;
}

sub delete_vertex_weight {
    &expect_non_multivertexed;
    push @_, $defattr;
    goto &delete_vertex_attribute;
}

sub add_weighted_vertex_by_id {
    &expect_multivertexed;
    push @_, $defattr, pop;
    goto &set_vertex_attribute_by_id;
}

sub add_weighted_vertices_by_id {
    &expect_multivertexed;
    my $g = shift;
    my $id = pop;
    while (@_) {
	my ($v, $w) = splice @_, 0, 2;
	$g->add_vertex_by_id($v, $id);
	$g->set_vertex_attribute_by_id($v, $id, $defattr, $w);
    }
}

sub get_vertex_weight_by_id {
    &expect_multivertexed;
    push @_, $defattr;
    goto &get_vertex_attribute_by_id;
}

sub has_vertex_weight_by_id {
    &expect_multivertexed;
    push @_, $defattr;
    goto &has_vertex_attribute_by_id;
}

sub set_vertex_weight_by_id {
    &expect_multivertexed;
    push @_, $defattr, pop;
    goto &set_vertex_attribute_by_id;
}

sub delete_vertex_weight_by_id {
    &expect_multivertexed;
    push @_, $defattr;
    goto &delete_vertex_attribute_by_id;
}

###
# Weighted edges.
#

sub add_weighted_edge {
    &expect_non_multiedged;
    push @_, $defattr, pop;
    goto &set_edge_attribute;
}

sub add_weighted_edges {
    &expect_non_multiedged;
    my $g = shift;
    while (@_) {
	my ($u, $v, $w) = splice @_, 0, 3;
	$g->set_edge_attribute($u, $v, $defattr, $w);
    }
}

sub add_weighted_edges_by_id {
    &expect_multiedged;
    my $g = shift;
    my $id = pop;
    while (@_) {
	my ($u, $v, $w) = splice @_, 0, 3;
	$g->set_edge_attribute_by_id($u, $v, $id, $defattr, $w);
    }
}

sub add_weighted_path {
    &expect_non_multiedged;
    my $g = shift;
    my $u = shift;
    while (@_) {
	my ($w, $v) = splice @_, 0, 2;
	$g->set_edge_attribute($u, $v, $defattr, $w);
	$u = $v;
    }
}

sub get_edge_weight {
    &expect_non_multiedged;
    push @_, $defattr;
    goto &get_edge_attribute;
}

sub has_edge_weight {
    &expect_non_multiedged;
    push @_, $defattr;
    goto &has_edge_attribute;
}

sub set_edge_weight {
    &expect_non_multiedged;
    push @_, $defattr, pop;
    goto &set_edge_attribute;
}

sub delete_edge_weight {
    &expect_non_multiedged;
    push @_, $defattr;
    goto &delete_edge_attribute;
}

sub add_weighted_edge_by_id {
    &expect_multiedged;
    push @_, $defattr, pop;
    goto &set_edge_attribute_by_id;
}

sub add_weighted_path_by_id {
    &expect_multiedged;
    my $g = shift;
    my $id = pop;
    my $u = shift;
    while (@_) {
	my ($w, $v) = splice @_, 0, 2;
	$g->set_edge_attribute_by_id($u, $v, $id, $defattr, $w);
	$u = $v;
    }
}

sub get_edge_weight_by_id {
    &expect_multiedged;
    push @_, $defattr;
    goto &get_edge_attribute_by_id;
}

sub has_edge_weight_by_id {
    &expect_multiedged;
    push @_, $defattr;
    goto &has_edge_attribute_by_id;
}

sub set_edge_weight_by_id {
    &expect_multiedged;
    push @_, $defattr, pop;
    goto &set_edge_attribute_by_id;
}

sub delete_edge_weight_by_id {
    &expect_multiedged;
    push @_, $defattr;
    goto &delete_edge_attribute_by_id;
}

###
# Error helpers.
#

my %expected;
@expected{qw(directed undirected acyclic)} = qw(undirected directed cyclic);

sub _expected {
    my $exp = shift;
    my $got = @_ ? shift : $expected{$exp};
    $got = defined $got ? ", got $got" : "";
    if (my @caller2 = caller(2)) {
	die "$caller2[3]: expected $exp graph$got, at $caller2[1] line $caller2[2].\n";
    } else {
	my @caller1 = caller(1); # uncoverable statement
	die "$caller1[3]: expected $exp graph$got, at $caller1[1] line $caller1[2].\n"; # uncoverable statement
    }
}

sub expect_no_args {
    my $g = shift;
    return unless @_;
    my @caller1 = caller(1); # uncoverable statement
    die "$caller1[3]: expected no arguments, got " . scalar @_ . ", at $caller1[1] line $caller1[2]\n"; # uncoverable statement
}

sub expect_undirected {
    _expected('undirected') unless &is_undirected;
}

sub expect_directed {
    _expected('directed') unless &is_directed;
}

sub expect_acyclic {
    _expected('acyclic') unless &is_acyclic;
}

sub expect_dag {
    my @got;
    push @got, 'undirected' unless &is_directed;
    push @got, 'cyclic'     unless &is_acyclic;
    _expected('directed acyclic', "@got") if @got;
}

sub expect_hyperedged {
    _expected('hyperedged') unless &is_hyperedged;
}

sub expect_multivertexed {
    _expected('multivertexed') unless &is_multivertexed;
}
*expect_multivertex = \&expect_multivertexed;

sub expect_non_multivertexed {
    _expected('non-multivertexed') if &is_multivertexed;
}
*expect_non_multivertex = \&expect_non_multivertexed;

sub expect_non_multiedged {
    _expected('non-multiedged') if &is_multiedged;
}
*expect_non_multiedge = \&expect_non_multiedged;

sub expect_multiedged {
    _expected('multiedged') unless &is_multiedged;
}
*expect_multiedge = \&expect_multiedged;

sub expect_non_unionfind {
    _expected('non-unionfind') if &has_union_find;
}

sub _get_options {
    my @caller = caller(1);
    unless (@_ == 1 && ref $_[0] eq 'ARRAY') {
	die "$caller[3]: internal error: should be called with only one array ref argument, at $caller[1] line $caller[2].\n";
    }
    my @opt = @{ $_[0] };
    unless (@opt  % 2 == 0) {
	die "$caller[3]: expected an options hash, got a non-even number of arguments, at $caller[1] line $caller[2].\n"; # uncoverable statement
    }
    return @opt;
}

###
# Random constructors and accessors.
#

sub __fisher_yates_shuffle (@) {
    # From perlfaq4, but modified to be non-modifying.
    my @a = @_;
    my $i = @a;
    while ($i--) {
	my $j = int rand ($i+1);
	@a[$i,$j] = @a[$j,$i];
    }
    return @a;
}

BEGIN {
    sub _shuffle(@);
    # Workaround for the Perl bug [perl #32383] where -d:Dprof and
    # List::Util::shuffle do not like each other: if any debugging
    # (-d) flags are on, fall back to our own Fisher-Yates shuffle.
    # The bug was fixed by perl changes #26054 and #26062, which
    # went to Perl 5.9.3.  If someone tests this with a pre-5.9.3
    # bleadperl that calls itself 5.9.3 but doesn't yet have the
    # patches, oh, well.
    *_shuffle = $^P && $] < 5.009003 ?
	\&__fisher_yates_shuffle : do { require List::Util; \&List::Util::shuffle };
}

sub random_graph {
    my $class = (@_ % 2) == 0 ? 'Graph' : shift;
    my %opt = _get_options( \@_ );
    __carp_confess "Graph::random_graph: argument 'vertices' missing or undef"
	unless defined $opt{vertices};
    srand delete $opt{random_seed} if exists $opt{random_seed};
    my $random_edge = delete $opt{random_edge} if exists $opt{random_edge};
    my @V;
    if (my $ref = ref $opt{vertices}) {
	__carp_confess "Graph::random_graph: argument 'vertices' illegal"
	    if $ref ne 'ARRAY';
	@V = @{ $opt{vertices} };
    } else {
	@V = 0..($opt{vertices} - 1);
    }
    delete $opt{vertices};
    my $V = @V;
    my $C = $V * ($V - 1) / 2;
    my $E;
    __carp_confess "Graph::random_graph: both arguments 'edges' and 'edges_fill' specified"
	if exists $opt{edges} && exists $opt{edges_fill};
    $E = exists $opt{edges_fill} ? $opt{edges_fill} * $C : $opt{edges};
    delete $opt{edges};
    delete $opt{edges_fill};
    my $g = $class->new(%opt);
    $g->add_vertices(@V);
    return $g if $V < 2;
    $C *= 2 if $g->directed;
    $E = $C / 2 unless defined $E;
    $E = int($E + 0.5);
    my $p = $E / $C;
    $random_edge = sub { $p } unless defined $random_edge;
    # print "V = $V, E = $E, C = $C, p = $p\n";
    __carp_confess "Graph::random_graph: needs to be countedged or multiedged ($E > $C)"
	if $p > 1.0 && !($g->countedged || $g->multiedged);
    my @V1 = @V;
    my @V2 = @V;
    # Shuffle the vertex lists so that the pairs at
    # the beginning of the lists are not more likely.
    @V1 = _shuffle @V1;
    @V2 = _shuffle @V2;
 LOOP:
    while ($E) {
	for my $v1 (@V1) {
	    for my $v2 (@V2) {
		next if $v1 eq $v2; # TODO: allow self-loops?
		my $q = $random_edge->($g, $v1, $v2, $p);
		if ($q && ($q == 1 || rand() <= $q) &&
		    !$g->has_edge($v1, $v2)) {
		    $g->add_edge($v1, $v2);
		    $E--;
		    last LOOP unless $E;
		}
	    }
	}
    }
    return $g;
}

sub random_vertex {
    my @V = &_vertices05;
    @V[rand @V];
}

sub random_edge {
    my @E = &_edges05;
    @E[rand @E];
}

sub random_successor {
    my @S = &successors;
    @S[rand @S];
}

sub random_predecessor {
    my @P = &predecessors;
    @P[rand @P];
}

###
# Algorithms.
#

my $MST_comparator = sub { ($_[0] || 0) <=> ($_[1] || 0) };

sub _MST_attr {
    my $attr = shift;
    my $attribute =
	exists $attr->{attribute}  ?
	    $attr->{attribute}  : $defattr;
    my $comparator =
	exists $attr->{comparator} ?
	    $attr->{comparator} : $MST_comparator;
    return ($attribute, $comparator);
}

sub _MST_edges {
    my ($g, $attr) = @_;
    my ($attribute, $comparator) = _MST_attr($attr);
    map $_->[1],
        sort { $comparator->($a->[0], $b->[0], $a->[1], $b->[1]) }
             map [ $g->get_edge_attribute(@$_, $attribute), $_ ],
                 &_edges05;
}

sub MST_Kruskal {
    &expect_undirected;
    my ($g, %attr) = @_;
    require Graph::UnionFind;

    my $MST = Graph->new(directed => 0);

    my $UF  = Graph::UnionFind->new;
    $UF->add($_) for $g->_vertices05;

    for my $e ($g->_MST_edges(\%attr)) {
	my ($u, $v) = @$e; # TODO: hyperedges
	next if $UF->find( $u ) eq $UF->find( $v );
	$UF->union($u, $v);
	$MST->add_edge($u, $v);
    }

    return $MST;
}

sub _MST_add {
    my ($g, $h, $HF, $r, $attr, $unseen) = @_;
    $HF->add( Graph::MSTHeapElem->new( $r, $_, $g->get_edge_attribute( $r, $_, $attr ) ) )
	for grep exists $unseen->{ $_ }, $g->successors( $r );
}

sub _next_alphabetic { shift; (sort               keys %{ $_[0] })[0] }
sub _next_numeric    { shift; (sort { $a <=> $b } keys %{ $_[0] })[0] }
sub _next_random     { shift; (values %{ $_[0] })[ rand keys %{ $_[0] } ] }

sub _root_opt {
    my ($g, @args) = @_;
    my %opt = @args == 1 ? ( first_root => $args[0] ) : _get_options( \@args );
    my %unseen;
    my @unseen = $g->_vertices05;
    @unseen{ @unseen } = @unseen;
    @unseen = _shuffle @unseen;
    my $r;
    if (exists $opt{ start }) {
	$opt{ first_root } = delete $opt{ start };
	$opt{ next_root  } = undef;
    }
    if (exists $opt{ first_root }) {
	if (ref $opt{ first_root } eq 'CODE') {
	    $r = $opt{ first_root }->( $g, \%unseen );
	} else {
	    $r = $opt{ first_root };
	}
    } else {
	$r = shift @unseen;
    }
    my $next =
	exists $opt{ next_root } ?
	    $opt{ next_root } :
              $opt{ next_alphabetic } ?
                \&_next_alphabetic :
                  $opt{ next_numeric } ?
                    \&_next_numeric :
                      \&_next_random;
    my $code = ref $next eq 'CODE';
    my $attr = exists $opt{ attribute } ? $opt{ attribute } : $defattr;
    return ( \%opt, \%unseen, \@unseen, $r, $next, $code, $attr );
}

sub _heap_walk {
    my ($g, $h, $add, $etc, $opt, $unseenh, $unseena, $r, $next, $code, $attr) = @_;
    require Heap::Fibonacci;
    my $HF = Heap::Fibonacci->new;
    while (defined $r) {
        # print "r = $r\n";
	$add->($g, $h, $HF, $r, $attr, $unseenh, $etc);
	delete $unseenh->{ $r };
	while (defined $HF->top) {
	    my $t = $HF->extract_top;
	    # use Data::Dumper; print "t = ", Dumper($t);
	    if (defined $t) {
		my ($u, $v, $w) = $t->val;
		# print "extracted top: $u $v $w\n";
		if (exists $unseenh->{ $v }) {
		    $h->set_edge_attribute($u, $v, $attr, $w);
		    delete $unseenh->{ $v };
		    $add->($g, $h, $HF, $v, $attr, $unseenh, $etc);
		}
	    }
	}
	return $h unless defined $next;
	$r = $code ? $next->( $g, $unseenh ) : shift @$unseena;
        last unless defined $r;
    }
    return $h;
}

sub MST_Prim {
    &expect_undirected;
    require Graph::MSTHeapElem;
    $_[0]->_heap_walk(Graph->new(directed => 0), \&_MST_add, undef, &_root_opt);
}

*MST_Dijkstra = \&MST_Prim;

*minimum_spanning_tree = \&MST_Prim;

###
# Cycle detection.
#

*is_cyclic = \&has_a_cycle;

sub is_acyclic {
    !&is_cyclic;
}

sub is_dag {
    &is_directed && &is_acyclic ? 1 : 0;
}

*is_directed_acyclic_graph = \&is_dag;

###
# Simple DFS uses.
#

sub topological_sort {
    my $g = shift;
    my %opt = _get_options( \@_ );
    my $eic = delete $opt{ empty_if_cyclic };
    my $hac;
    if ($eic) {
	$hac = $g->has_a_cycle;
    } else {
	$g->expect_dag;
    }
    require Graph::Traversal::DFS;
    my $t = Graph::Traversal::DFS->new($g, %opt);
    my @s = $t->dfs;
    $hac ? () : reverse @s;
}

*toposort = \&topological_sort;

sub _undirected_copy_compute {
  my $c = Graph->new(directed => 0);
  $c->add_vertex($_) for &isolated_vertices; # TODO: if iv ...
  $c->add_edge(@$_) for &_edges05;
  return $c;
}

sub undirected_copy {
    &expect_directed;
    return _check_cache($_[0], 'undirected_copy', [], \&_undirected_copy_compute);
}

*undirected_copy_graph = \&undirected_copy;

sub directed_copy {
    &expect_undirected;
    my $c = Graph::Directed->new;
    $c->add_vertex($_) for &isolated_vertices; # TODO: if iv ...
    for my $e (&_edges05) {
	my @e = @$e;
	$c->add_edge(@e);
	$c->add_edge(reverse @e);
    }
    return $c;
}

*directed_copy_graph = \&directed_copy;

###
# Cache or not.
#

my %_cache_type =
    (
     'connectivity'        => ['_ccc'],
     'strong_connectivity' => ['_scc'],
     'biconnectivity'      => ['_bcc'],
     'SPT_Dijkstra'        => ['_spt_di', 'SPT_Dijkstra_root'],
     'SPT_Bellman_Ford'    => ['_spt_bf', 'SPT_Bellman_Ford_root'],
     'undirected_copy'     => ['_undirected'],
     'transitive_closure_matrix' => ['_tcm'],
    );

for my $t (keys %_cache_type) {
    no strict 'refs';
    my @attr = @{ $_cache_type{$t} };
    *{$t."_clear_cache"} = sub { $_[0]->delete_graph_attribute($_) for @attr };
}

sub _check_cache {
    my ($g, $type, $extra_vals, $code, @args) = @_;
    my $c = $_cache_type{$type};
    __carp_confess "Graph: unknown cache type '$type'" if !defined $c;
    my ($main_key, @extra_keys) = @$c;
    __carp_confess "Graph: wrong number of extra values (@extra_keys) vs (@$extra_vals)" if @extra_keys != @$extra_vals;
    my $a = $g->get_graph_attribute($main_key);
    __carp_confess "$c attribute set to unexpected value $a"
	if defined $a and ref $a ne 'ARRAY';
    unless (defined $a && $a->[ 0 ] == $g->[ _G ]) {
	$g->set_graph_attribute($main_key, $a = [ $g->[ _G ], $code->( $g, @args ) ]);
    }
    my $i = -1;
    my $extra_invalid = grep {
	my $v = $a->[ 1 ]->get_graph_attribute($_);
	$i++; # here so still incremented even if short-cut
	!defined $v or $v ne $extra_vals->[$i];
    } @extra_keys;
    if ($extra_invalid) {
	$g->set_graph_attribute($main_key, $a = [ $g->[ _G ], $code->( $g, @args ) ]);
    }
    return $a->[ 1 ];
}

###
# Connected components.
#

sub _connected_components_compute {
    my $g = $_[0];
    my %v2c;
    my @c;
    return [ [], {} ] unless my @v = $g->unique_vertices;
    if (&has_union_find) {
	my $UF = $g->_get_union_find();
	my $V  = $g->[ _V ];
	my @ids = $V->get_ids_by_paths([ map [$_], @v ]);
	my ($counter, %cc2counter) = 0;
	for (my $i = 0; $i <= $#v; $i++) {
	    my $cc = $UF->find( $ids[$i] );
	    __carp_confess "connected_component union-find did not have vertex '$v[$i]', please report"
		if !defined $cc;
	    $cc2counter{$cc} = $counter++ if !exists $cc2counter{$cc};
	    my $ci = $cc2counter{$cc};
	    $v2c{ $v[$i] } = $ci;
	    push @{ $c[$ci] }, $v[$i];
	}
    } else {
	require Graph::Traversal::DFS;
	my %r; @r{ @v } = @v;
	@c = [];
	my $t = Graph::Traversal::DFS->new(
	    $g,
	    first_root => sub { (each %r)[1] },
	    next_root  => sub { push @c, [] if keys %r; (each %r)[1]; },
	    pre => sub {
		my ($v, $t) = @_;
		$v2c{ $v } = $#c;
		push @{ $c[-1] }, $v;
		delete $r{ $v };
	    },
	    @_[1..$#_]
	);
	$t->dfs;
    }
    return [ \@c, \%v2c ];
}

sub _connected_components {
    my $ccc = _check_cache($_[0], 'connectivity', [],
			   \&_connected_components_compute);
    return @{ $ccc };
}

sub connected_component_by_vertex {
    &expect_undirected;
    (&_connected_components)[1]->{ $_[1] };
}

sub connected_component_by_index {
    &expect_undirected;
    my $value = (&_connected_components)[0]->[$_[1]];
    $value ? @{ $value || [] } : ();
}

sub connected_components {
    &expect_undirected;
    @{ (&_connected_components)[0] };
}

sub same_connected_components {
    &expect_undirected;
    my ($g, @args) = @_;
    my @components;
    if (&has_union_find) {
	my $UF = &_get_union_find;
	my @ids = &_vertex_ids;
	return 0 if @ids != @args;
	@components = map $UF->find( $_ ), @ids;
    } else {
	@components = @{ (&_connected_components)[1] }{ @args };
    }
    return 0 if grep !defined, @components;
    require List::Util;
    List::Util::uniq( @components ) == 1;
}

sub _super_component { join("+", sort @_) }

sub connected_graph {
    &expect_undirected;
    my ($g, %opt) = @_;
    my $cg = Graph->new(undirected => 1);
    if ($g->has_union_find && $g->vertices == 1) {
	# TODO: super_component?
	$cg->add_vertices($g->vertices);
    } else {
	my $sc_cb = $opt{super_component} || \&_super_component;
	$cg->set_vertex_attribute(scalar $sc_cb->(@$_), 'subvertices', $_)
	    for $g->connected_components;
    }
    return $cg;
}

sub is_connected {
    &expect_undirected;
    return @{ (&_connected_components)[0] } == 1;
}

sub is_weakly_connected {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &is_connected;
}

*weakly_connected = \&is_weakly_connected;

sub weakly_connected_components {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &connected_components;
}

sub weakly_connected_component_by_vertex {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &connected_component_by_vertex;
}

sub weakly_connected_component_by_index {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &connected_component_by_index;
}

sub same_weakly_connected_components {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &same_connected_components;
}

sub weakly_connected_graph {
    &expect_directed;
    splice @_, 0, 1, &undirected_copy;
    goto &connected_graph;
}

sub _strongly_connected_components_compute {
    my $g = $_[0];
    require Graph::Traversal::DFS;
    require List::Util;
    my $t = Graph::Traversal::DFS->new($g);
    my @d = reverse $t->dfs;
    my @c;
    my %v2c;
    my $u = Graph::Traversal::DFS->new(
	$g->transpose_graph,
	next_root => sub {
	    my ($t, $u) = @_;
	    return if !defined(my $root = List::Util::first(
		sub { exists $u->{$_} }, @d
	    ));
	    push @c, [];
	    return $root;
	},
	pre => sub {
	    my ($v, $t) = @_;
	    push @{ $c[-1] }, $v;
	    $v2c{$v} = $#c;
	},
	next_alphabetic => 1,
    );
    $u->dfs;
    return [ \@c, \%v2c ];
}

sub _strongly_connected_components_v2c {
    &_strongly_connected_components->[1];
}

sub _strongly_connected_components_arrays {
    @{ &_strongly_connected_components->[0] };
}

sub _strongly_connected_components {
    _check_cache($_[0], 'strong_connectivity', [],
			   \&_strongly_connected_components_compute);
}

sub strongly_connected_components {
    &expect_directed;
    goto &_strongly_connected_components_arrays;
}

sub strongly_connected_component_by_vertex {
    &expect_directed;
    &_strongly_connected_components_v2c->{$_[1]};
}

sub strongly_connected_component_by_index {
    &expect_directed;
    my $i = $_[1];
    return if !defined(my $c = &_strongly_connected_components->[0][ $i ]);
    @$c;
}

sub same_strongly_connected_components {
    &expect_directed;
    my ($g, @args) = @_;
    require Set::Object;
    Set::Object->new(@{ &_strongly_connected_components_v2c }{@args})->size <= 1;
}

sub is_strongly_connected {
    &strongly_connected_components == 1;
}

*strongly_connected = \&is_strongly_connected;

sub strongly_connected_graph {
    &expect_directed;
    my ($g, %attr) = @_;
    my $sc_cb = \&_super_component;
    _opt_get(\%attr, super_component => \$sc_cb);
    _opt_unknown(\%attr);
    my ($c, $v2c) = @{ &_strongly_connected_components };
    my $s = Graph->new;
    my @s = map $sc_cb->(@$_), @$c;
    $s->set_vertex_attribute($s[$_], 'subvertices', $c->[$_]) for 0..$#$c;
    require List::Util;
    $s->add_edge( @s[ @$v2c{ @$_ } ] )
	for grep List::Util::uniq( @$v2c{ @$_ } ) > 1, &_edges05;
    return $s;
}

###
# Biconnectivity.
#

sub _biconnectivity_out {
  my ($state, $u, $v) = @_;
  my @BC;
  while (@{$state->{stack}}) {
    push @BC, my $e = pop @{$state->{stack}};
    last if $e->[0] eq $u && $e->[1] eq $v;
  }
  push @{$state->{BC}}, \@BC if @BC;
}

sub _biconnectivity_dfs {
  my ($g, $u, $state) = @_;
  $state->{low}{$u} = $state->{num}{$u} = $state->{dfs}++;
  for my $v ($g->successors($u)) {
    if (!exists $state->{num}{$v}) {
      push @{$state->{stack}}, [$u, $v];
      $state->{pred}{$v} = $u;
      $state->{succ}{$u}{$v}++;
      _biconnectivity_dfs($g, $v, $state);
      $state->{low}{$u} = List::Util::min(@{ $state->{low} }{$u, $v});
      _biconnectivity_out($state, $u, $v)
	if $state->{low}{$v} >= $state->{num}{$u};
    } elsif (defined $state->{pred}{$u} &&
	     $state->{pred}{$u} ne $v &&
	     $state->{num}{$v} < $state->{num}{$u}) {
      push @{$state->{stack}}, [$u, $v];
      $state->{low}{$u} = List::Util::min($state->{low}{$u}, $state->{num}{$v});
    }
  }
}

sub _biconnectivity_compute {
    require List::Util;
    my ($g) = @_;
    my %state = (BC=>[], dfs=>0);
    my @u = $g->vertices;
    for my $u (@u) {
	next if exists $state{num}->{$u};
	_biconnectivity_dfs($g, $u, \%state);
	push @{$state{BC}}, delete $state{stack} if @{ $state{stack} || [] };
    }

    # Mark the components each vertex belongs to.
    my ($bci, %v2bc, %v2bc_vec, %bc2v) = 0;
    my $Z = "\0" x ((@{$state{BC}} + 7) / 8);
    @v2bc_vec{@u} = ($Z) x @u;
    for my $bc (@{$state{BC}}) {
      vec($v2bc_vec{$_}, $bci, 1) = $v2bc{$_}{$bci} = 1 for map @$_, @$bc;
      $bci++;
    }

    # Any isolated vertices get each their own component.
    $v2bc{$_}{$bci} = vec($v2bc_vec{$_}, $bci++, 1) = 1 for grep !exists $v2bc{$_}, @u;

    for my $v (@u) {
      $bc2v{$_}{$v}{$_} = undef for keys %{$v2bc{$v}};
    }

    # Articulation points / cut vertices are the vertices
    # which belong to more than one component.
    my @ap = grep keys %{$v2bc{$_}} > 1, @u;

    # Bridges / cut edges are the components of two vertices.
    my @br = grep @$_ == 2, map [keys %$_], values %bc2v;

    # Create the subgraph components.
    my @sg = map [ List::Util::uniq( map @$_, @$_ ) ], @{$state{BC}};
    return [ \@ap, \@sg, \@br, \%v2bc, \%v2bc_vec, $Z ];
}

sub biconnectivity {
    &expect_undirected;
    @{ _check_cache($_[0], 'biconnectivity', [],
			   \&_biconnectivity_compute, @_[1..$#_]) || [] };
}

sub is_biconnected {
    &edges >= 2 ? @{ (&biconnectivity)[0] } == 0 : undef ;
}

sub is_edge_connected {
    &edges >= 2 ? @{ (&biconnectivity)[2] } == 0 : undef;
}

sub is_edge_separable {
    &edges >= 2 ? @{ (&biconnectivity)[2] } > 0 : undef;
}

sub articulation_points {
    @{ (&biconnectivity)[0] };
}

*cut_vertices = \&articulation_points;

sub biconnected_components {
    @{ (&biconnectivity)[1] };
}

sub biconnected_component_by_index {
    my ($i) = splice @_, 1, 1;
    (&biconnectivity)[1]->[ $i ];
}

sub biconnected_component_by_vertex {
    my ($v) = splice @_, 1, 1;
    my $v2bc = (&biconnectivity)[3];
    splice @_, 1, 0, $v;
    return defined $v2bc->{ $v } ? keys %{ $v2bc->{ $v } } : ();
}

sub same_biconnected_components {
    my ($v2bc, $Z) =  (&biconnectivity)[4,5];
    return 0 if grep !defined, my @vecs = @$v2bc{ @_[1..$#_] };
    !grep $Z eq ($vecs[0] & $_), @vecs[1..$#vecs];
}

sub biconnected_graph {
    my ($g, %opt) = @_;
    my ($bc, $v2bc) = (&biconnectivity)[1, 3];
    my $bcg = Graph->new(directed => 0);
    my $sc_cb = $opt{super_component} || \&_super_component;
    my @s = map $sc_cb->(@$_), @$bc;
    $bcg->set_vertex_attribute($s[$_], 'subvertices', $bc->[$_]) for 0..$#$bc;
    my %k;
    for my $i (0..$#$bc) {
	my @u = @{ $bc->[ $i ] };
	for my $j (0..$i-1) {
	    my %j; @j{ @{ $bc->[ $j ] } } = ();
	    next if !grep exists $j{ $_ }, @u;
	    next if $k{ $i }{ $j }++;
	    $bcg->add_edge(@s[$i, $j]);
	}
    }
    return $bcg;
}

sub bridges {
    @{ (&biconnectivity)[2] || [] };
}

###
# SPT.
#

sub _SPT_add {
    my ($g, $h, $HF, $r, $attr, $unseen, $etc) = @_;
    my $etc_r = $etc->{ $r } || 0;
    for my $s ( grep exists $unseen->{ $_ }, $g->successors( $r ) ) {
	my $t = $g->get_edge_attribute( $r, $s, $attr );
	$t = 1 unless defined $t;
	__carp_confess "Graph::SPT_Dijkstra: edge $r-$s is negative ($t)"
	    if $t < 0;
	if (!defined($etc->{ $s }) || ($etc_r + $t) < $etc->{ $s }) {
	    my $etc_s = $etc->{ $s } || 0;
	    $etc->{ $s } = $etc_r + $t;
	    # print "$r - $s : setting $s to $etc->{ $s } ($etc_r, $etc_s)\n";
	    $h->set_vertex_attributes($s, { $attr=>$etc->{ $s }, 'p', $r });
	    $HF->add( Graph::SPTHeapElem->new($r, $s, $etc->{ $s }) );
	}
    }
}

sub _SPT_Dijkstra_compute {
    require Graph::SPTHeapElem;
    my $sptg = $_[0]->_heap_walk($_[0]->new, \&_SPT_add, {}, @_[1..$#_]);
    $sptg->set_graph_attribute('SPT_Dijkstra_root', $_[4]);
    $sptg;
}

sub SPT_Dijkstra {
    my $g = $_[0];
    my @args = &_root_opt;
    _check_cache($g, 'SPT_Dijkstra', [$args[3]],
	\&_SPT_Dijkstra_compute, @args);
}

*SSSP_Dijkstra = \&SPT_Dijkstra;

*single_source_shortest_paths = \&SPT_Dijkstra;

sub SP_Dijkstra {
    my ($g, $u, $v) = @_;
    my $sptg = $g->SPT_Dijkstra(first_root => $u);
    my @path = ($v);
    require Set::Object;
    my $seen = Set::Object->new;
    my $V = $g->vertices;
    my $p;
    while (defined($p = $sptg->get_vertex_attribute($v, 'p'))) {
	last if $seen->contains($p);
	push @path, $p;
	$v = $p;
	$seen->insert($p);
	last if $seen->size == $V || $u eq $v;
    }
    return if !@path or $path[-1] ne $u;
    return reverse @path;
}

sub __SPT_Bellman_Ford {
    my ($g, $u, $v, $attr, $d, $p, $c0, $c1) = @_;
    return unless $c0->{ $u };
    my $w = $g->get_edge_attribute($u, $v, $attr);
    $w = 1 unless defined $w;
    if (defined $d->{ $v }) {
	if (defined $d->{ $u }) {
	    if ($d->{ $v } > $d->{ $u } + $w) {
		$d->{ $v } = $d->{ $u } + $w;
		$p->{ $v } = $u;
		$c1->{ $v }++;
	    }
	} # else !defined $d->{ $u } &&  defined $d->{ $v }
    } else {
	if (defined $d->{ $u }) {
	    #  defined $d->{ $u } && !defined $d->{ $v }
	    $d->{ $v } = $d->{ $u } + $w;
	    $p->{ $v } = $u;
	    $c1->{ $v }++;
	} # else !defined $d->{ $u } && !defined $d->{ $v }
    }
}

sub _SPT_Bellman_Ford {
    my ($g, $opt, $unseenh, $unseena, $r, $next, $code, $attr) = @_;
    my %d;
    return unless defined $r;
    $d{ $r } = 0;
    my %p;
    my $V = $g->vertices;
    my %c0; # Changed during the last iteration?
    $c0{ $r }++;
    for (my $i = 0; $i < $V; $i++) {
	my %c1;
	for my $e ($g->edges) {
	    my ($u, $v) = @$e;
	    __SPT_Bellman_Ford($g, $u, $v, $attr, \%d, \%p, \%c0, \%c1);
	    __SPT_Bellman_Ford($g, $v, $u, $attr, \%d, \%p, \%c0, \%c1)
		if $g->undirected;
	}
	%c0 = %c1 unless $i == $V - 1;
    }

    for my $e ($g->edges) {
	my ($u, $v) = @$e;
	if (defined $d{ $u } && defined $d{ $v }) {
	    my $d = $g->get_edge_attribute($u, $v, $attr);
	    __carp_confess "Graph::SPT_Bellman_Ford: negative cycle exists"
		if defined $d && $d{ $v } > $d{ $u } + $d;
	}
    }

    return (\%p, \%d);
}

sub _SPT_Bellman_Ford_compute {
    my ($g, @args) = @_;
    my ($p, $d) = $g->_SPT_Bellman_Ford(@args);
    my $h = $g->new;
    for my $v (keys %$p) {
	my $u = $p->{ $v };
	$h->set_edge_attribute( $u, $v, $args[6],
				$g->get_edge_attribute($u, $v, $args[6]));
	$h->set_vertex_attributes( $v, { $args[6], $d->{ $v }, p => $u } );
    }
    $h->set_graph_attribute('SPT_Bellman_Ford_root', $args[3]);
    $h;
}

sub SPT_Bellman_Ford {
    my @args = &_root_opt;
    _check_cache($_[0], 'SPT_Bellman_Ford', [$args[3]],
	\&_SPT_Bellman_Ford_compute, @args);
}

*SSSP_Bellman_Ford = \&SPT_Bellman_Ford;

sub SP_Bellman_Ford {
    my ($g, $u, $v) = @_;
    my $sptg = $g->SPT_Bellman_Ford(first_root => $u);
    my @path = ($v);
    require Set::Object;
    my $seen = Set::Object->new;
    my $V = $g->vertices;
    my $p;
    while (defined($p = $sptg->get_vertex_attribute($v, 'p'))) {
	last if $seen->contains($p);
	push @path, $p;
	$v = $p;
	$seen->insert($p);
	last if $seen->size == $V;
    }
    # @path = () if @path && "$path[-1]" ne "$u";
    return reverse @path;
}

###
# Transitive Closure.
#

sub TransitiveClosure_Floyd_Warshall {
    my $self = shift;
    require Graph::TransitiveClosure;
    Graph::TransitiveClosure->new($self, @_);
}

*transitive_closure = \&TransitiveClosure_Floyd_Warshall;

sub APSP_Floyd_Warshall {
    my $self = shift;
    require Graph::TransitiveClosure;
    Graph::TransitiveClosure->new($self, path => 1, @_);
}

*all_pairs_shortest_paths = \&APSP_Floyd_Warshall;

sub _transitive_closure_matrix_compute {
    &APSP_Floyd_Warshall->transitive_closure_matrix;
}

sub transitive_closure_matrix {
    _check_cache($_[0], 'transitive_closure_matrix', [],
	\&_transitive_closure_matrix_compute, @_[1..$#_]);
}

sub path_length {
    shift->transitive_closure_matrix->path_length(@_);
}

sub path_successor {
    shift->transitive_closure_matrix->path_successor(@_);
}

sub path_vertices {
    shift->transitive_closure_matrix->path_vertices(@_);
}

sub all_paths {
    shift->transitive_closure_matrix->all_paths(@_);
}

sub is_reachable {
    shift->transitive_closure_matrix->is_reachable(@_);
}

sub for_shortest_paths {
    my $g = shift;
    my $c = shift;
    my $t = $g->transitive_closure_matrix;
    my @v = $g->vertices;
    my $n = 0;
    for my $u (@v) {
	$c->($t, $u, $_, ++$n) for grep $t->is_reachable($u, $_), @v;
    }
    return $n;
}

sub _minmax_path {
    my $g = shift;
    my $min;
    my $max;
    my $minp;
    my $maxp;
    $g->for_shortest_paths(sub {
			       my ($t, $u, $v, $n) = @_;
			       my $l = $t->path_length($u, $v);
			       return unless defined $l;
			       my $p;
			       if ($u ne $v && (!defined $max || $l > $max)) {
				   $max = $l;
				   $maxp = $p = [ $t->path_vertices($u, $v) ];
			       }
			       if ($u ne $v && (!defined $min || $l < $min)) {
				   $min = $l;
				   $minp = $p || [ $t->path_vertices($u, $v) ];
			       }
			   });
    return ($min, $max, $minp, $maxp);
}

sub diameter {
    my $g = shift;
    my ($min, $max, $minp, $maxp) = $g->_minmax_path(@_);
    return defined $maxp ? (wantarray ? @$maxp : $max) : undef;
}

*graph_diameter = \&diameter;

sub longest_path {
    my ($g, $u, $v) = @_;
    my $t = $g->transitive_closure_matrix;
    if (defined $u) {
	return wantarray ? $t->path_vertices($u, $v) : $t->path_length($u, $v)
	    if defined $v;
	my $max;
	my @max;
	for my $v (grep $u ne $_, $g->vertices) {
	    my $l = $t->path_length($u, $v);
	    next if !(defined $l && (!defined $max || $l > $max));
	    $max = $l;
	    @max = $t->path_vertices($u, $v);
	}
	return wantarray ? @max : $max;
    }
    if (defined $v) {
	my $max;
	my @max;
	for my $u (grep $_ ne $v, $g->vertices) {
	    my $l = $t->path_length($u, $v);
	    next if !(defined $l && (!defined $max || $l > $max));
	    $max = $l;
	    @max = $t->path_vertices($u, $v);
	}
	return wantarray ? @max : @max - 1;
    }
    my ($min, $max, $minp, $maxp) = $g->_minmax_path(@_);
    return defined $maxp ? (wantarray ? @$maxp : $max) : undef;
}

sub vertex_eccentricity {
    &expect_undirected;
    my ($g, $u) = @_;
    return Infinity() if !&is_connected;
    my $max;
    for my $v (grep $u ne $_, $g->vertices) {
	my $l = $g->path_length($u, $v);
	next if !(defined $l && (!defined $max || $l > $max));
	$max = $l;
    }
    return defined $max ? $max : Infinity();
}

sub shortest_path {
    &expect_undirected;
    my ($g, $u, $v) = @_;
    my $t = $g->transitive_closure_matrix;
    if (defined $u) {
	return wantarray ? $t->path_vertices($u, $v) : $t->path_length($u, $v)
	    if defined $v;
	my $min;
	my @min;
	for my $v (grep $u ne $_, $g->vertices) {
	    my $l = $t->path_length($u, $v);
	    next if !(defined $l && (!defined $min || $l < $min));
	    $min = $l;
	    @min = $t->path_vertices($u, $v);
	}
	# print "min/1 = @min\n";
	return wantarray ? @min : $min;
    }
    if (defined $v) {
	my $min;
	my @min;
	for my $u (grep $_ ne $v, $g->vertices) {
	    my $l = $t->path_length($u, $v);
	    next if !(defined $l && (!defined $min || $l < $min));
	    $min = $l;
	    @min = $t->path_vertices($u, $v);
	}
	# print "min/2 = @min\n";
	return wantarray ? @min : $min;
    }
    my ($min, $max, $minp, $maxp) = $g->_minmax_path(@_);
    return if !defined $minp;
    wantarray ? @$minp : $min;
}

sub radius {
    &expect_undirected;
    my $g = shift;
    my ($center, $radius) = (undef, Infinity());
    for my $v ($g->vertices) {
	my $x = $g->vertex_eccentricity($v);
	($center, $radius) = ($v, $x) if defined $x && $x < $radius;
    }
    return $radius;
}

sub center_vertices {
    &expect_undirected;
    my ($g, $delta) = @_;
    $delta = 0 unless defined $delta;
    $delta = abs($delta);
    my @c;
    my $Inf = Infinity();
    my $r = $g->radius;
    if (defined $r && $r != $Inf) {
	for my $v ($g->vertices) {
	    my $e = $g->vertex_eccentricity($v);
	    next unless defined $e && $e != $Inf;
	    push @c, $v if abs($e - $r) <= $delta;
	}
    }
    return @c;
}

*centre_vertices = \&center_vertices;

sub average_path_length {
    my $g = shift;
    my @A = @_;
    my $d = 0;
    my $m = 0;
    $g->for_shortest_paths(sub {
        my ($t, $u, $v, $n) = @_;
        return unless my $l = $t->path_length($u, $v);
        return if defined $A[0] && $u ne $A[0];
        return if defined $A[1] && $v ne $A[1];
        $d += $l;
        $m++;
    });
    return $m ? $d / $m : undef;
}

###
# Simple tests.
#

sub is_multi_graph {
    return 0 unless &is_multiedged || &is_countedged;
    my $g = $_[0];
    my $multiedges = 0;
    for my $e (&_edges05) {
	my ($u, @v) = @$e;
	return 0 if grep $u eq $_, @v;
	$multiedges++ if $g->get_edge_count(@$e) > 1;
    }
    return $multiedges;
}

sub is_simple_graph {
    return 1 unless &is_multiedged || &is_countedged;
    my $g = $_[0];
    return 0 if grep $g->get_edge_count(@$_) > 1, &_edges05;
    return 1;
}

sub is_pseudo_graph {
    my $m = &is_countedged || &is_multiedged;
    my $g = $_[0];
    for my $e (&_edges05) {
	my ($u, @v) = @$e;
	return 1 if grep $u eq $_, @v;
	return 1 if $m && $g->get_edge_count($u, @v) > 1;
    }
    return 0;
}

###
# Rough isomorphism guess.
#

my %_factorial = (0 => 1, 1 => 1);

sub __factorial {
    my $n = shift;
    for (my $i = 2; $i <= $n; $i++) {
	next if exists $_factorial{$i};
	$_factorial{$i} = $i * $_factorial{$i - 1};
    }
    $_factorial{$n};
}

sub _factorial {
    my $n = int(shift);
    __carp_confess "factorial of a negative number" if $n < 0;
    __factorial($n) unless exists $_factorial{$n};
    return $_factorial{$n};
}

sub could_be_isomorphic {
    my ($g0, $g1) = @_;
    return 0 unless &vertices == $g1->vertices;
    return 0 unless &_edges05  == $g1->_edges05;
    my %d0;
    $d0{ $g0->in_degree($_) }{ $g0->out_degree($_) }++ for &vertices;
    my %d1;
    $d1{ $g1->in_degree($_) }{ $g1->out_degree($_) }++ for $g1->vertices;
    return 0 unless keys %d0 == keys %d1;
    for my $da (keys %d0) {
	return 0
	    unless exists $d1{$da} &&
		   keys %{ $d0{$da} } == keys %{ $d1{$da} };
	return 0
	    if grep !(exists $d1{$da}{$_} && $d0{$da}{$_} == $d1{$da}{$_}),
	    keys %{ $d0{$da} };
    }
    for my $da (keys %d0) {
	return 0 if grep $d1{$da}{$_} != $d0{$da}{$_}, keys %{ $d0{$da} };
	delete $d1{$da};
    }
    return 0 unless keys %d1 == 0;
    my $f = 1;
    for my $da (keys %d0) {
	$f *= _factorial(abs($d0{$da}{$_})) for keys %{ $d0{$da} };
    }
    return $f;
}

###
# Analysis functions.

sub subgraph_by_radius {
    $_[0]->subgraph([ @_[1..$#_-1], &reachable_by_radius ]);
}

sub clustering_coefficient {
    my ($g) = @_;
    return unless my @v = $g->vertices;
    require Set::Object;
    my %clustering;

    my $gamma = 0;

    for my $n (@v) {
	my $gamma_v = 0;
	my @neigh = $g->successors($n);
	my $c = Set::Object->new;
	for my $u (@neigh) {
	    for my $v (grep +(!$c->contains("$u-$_") && $g->has_edge($u, $_)), @neigh) {
		$gamma_v++;
		$c->insert("$u-$v");
		$c->insert("$v-$u");
	    }
	}
	if (@neigh > 1) {
	    $clustering{$n} = $gamma_v/(@neigh * (@neigh - 1) / 2);
	    $gamma += $gamma_v/(@neigh * (@neigh - 1) / 2);
	} else {
	    $clustering{$n} = 0;
	}
    }

    $gamma /= @v;

    return wantarray ? ($gamma, %clustering) : $gamma;
}

sub betweenness {
    my $g = shift;

    my @V = $g->vertices();

    my %Cb; # C_b{w} = 0

    @Cb{@V} = ();

    for my $s (@V) {
	my @S; # stack (unshift, shift)

	my %P; # P{w} = empty list
	$P{$_} = [] for @V;

	my %sigma; # \sigma{t} = 0
	$sigma{$_} = 0 for @V;
	$sigma{$s} = 1;

	my %d; # d{t} = -1;
	$d{$_} = -1 for @V;
	$d{$s} = 0;

	my @Q; # queue (push, shift)
	push @Q, $s;

	while (@Q) {
	    my $v = shift @Q;
	    unshift @S, $v;
	    for my $w ($g->successors($v)) {
		# w found for first time
		if ($d{$w} < 0) {
		    push @Q, $w;
		    $d{$w} = $d{$v} + 1;
		}
		# Shortest path to w via v
		if ($d{$w} == $d{$v} + 1) {
		    $sigma{$w} += $sigma{$v};
		    push @{ $P{$w} }, $v;
		}
	    }
	}

	my %delta;
	$delta{$_} = 0 for @V;

	while (@S) {
	    my $w = shift @S;
	    $delta{$_} += $sigma{$_}/$sigma{$w} * (1 + $delta{$w})
		for @{ $P{$w} };
	    $Cb{$w} += $delta{$w} if $w ne $s;
	}
    }

    return %Cb;
}

1;
