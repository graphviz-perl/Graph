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

our $VERSION = '0.9712';

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
sub _S () { 6 } # Successors.
sub _P () { 7 } # Predecessors.

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
# - _HYPER for hyperness: a different number of "coordinates" than usual;
#   expects one for vertices and two for edges
# - _UNORD for unordered coordinates (a set): if _UNORD is not set
#   the coordinates are assumed to be meaningfully ordered
# - _UNIQ for unique coordinates: if set duplicates are removed,
#   if not, duplicates are assumed to meaningful
# - _UNORDUNIQ: just a union of _UNORD and UNIQ
# Vertices are assumed to be _UNORDUNIQ; edges assume none of these flags.

use Graph::Attribute array => _A, map => 'graph';

sub stringify {
    my $u = &is_undirected;
    my $g = shift;
    my $e = $u ? '=' : '-';
    my @s = sort map join($e, $u ? sort { "$a" cmp "$b" } @$_ : @$_), $g->_edges05;
    push @s, sort { "$a" cmp "$b" } $g->isolated_vertices;
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
    my $vflags = _UNORDUNIQ;
    my $eflags = 0;
    my %opt = _get_options( \@args );

    if (ref $class && $class->isa('Graph')) {
	my %existing;
	no strict 'refs';
        for my $c (qw(undirected refvertexed
                      countvertexed multivertexed
                      hyperedged countedged multiedged omniedged
		      __stringified)) {
	    $existing{$c}++ if $class->$c;
        }
	$existing{unionfind}++ if $class->has_union_find;
	%opt = (%existing, %opt) if %existing; # allow overrides
    }

    _opt_get(\%opt, undirected   => \$opt{omniedged});
    _opt_get(\%opt, omnidirected => \$opt{omniedged});

    $opt{omniedged} = !delete $opt{directed} if exists $opt{directed};

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
	 hyperedged	=> _HYPER,
	 omniedged	=> _UNORD,
	 uniqedged	=> _UNIQ,
	);

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
    $g->[ _V ] = ($vflags & _MULTI) ?
	_am_heavy($vflags, 1) :
	    (($vflags & ~_UNORDUNIQ) ?
	     _am_vertex($vflags, 1) :
	     _am_light($vflags, 1, $g));
    $g->[ _E ] = ($eflags & ~_UNORD) ?
	_am_heavy($eflags, 2) :
	    _am_light($eflags, 2, $g);

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

sub _am_vertex {
    require Graph::AdjacencyMap::Vertex;
    Graph::AdjacencyMap::Vertex->_new(@_);
}

sub _am_light {
    require Graph::AdjacencyMap::Light;
    Graph::AdjacencyMap::Light->_new(@_);
}

sub _am_heavy {
    require Graph::AdjacencyMap::Heavy;
    Graph::AdjacencyMap::Heavy->_new(@_);
}

sub countvertexed { $_[0]->[ _V ]->_is_COUNT }
sub multivertexed { $_[0]->[ _V ]->_is_MULTI }
sub refvertexed   { $_[0]->[ _V ]->_is_REF   }
sub refvertexed_stringified { $_[0]->[ _V ]->_is_REFSTR }
sub __stringified { $_[0]->[ _V ]->_is_STR   }

sub countedged    { $_[0]->[ _E ]->_is_COUNT }
sub multiedged    { $_[0]->[ _E ]->_is_MULTI }
sub hyperedged    { $_[0]->[ _E ]->_is_HYPER }
sub omniedged     { $_[0]->[ _E ]->_is_UNORD }
sub uniqedged     { $_[0]->[ _E ]->_is_UNIQ  }

*undirected   = \&omniedged;
*omnidirected = \&omniedged;
sub directed { ! $_[0]->[ _E ]->_is_UNORD }

*is_directed      = \&directed;
*is_undirected    = \&undirected;

*is_countvertexed = \&countvertexed;
*is_multivertexed = \&multivertexed;
*is_omnidirected  = \&omnidirected;
*is_refvertexed   = \&refvertexed;
*is_refvertexed_stringified = \&refvertexed_stringified;

*is_countedged    = \&countedged;
*is_multiedged    = \&multiedged;
*is_hyperedged    = \&hyperedged;
*is_omniedged     = \&omniedged;
*is_uniqedged     = \&uniqedged;

sub _union_find_add_vertex {
    my ($g, $v) = @_;
    $g->[ _U ]->add( $g->[ _V ]->_get_path_id( $v ) );
}

sub add_vertex {
    __carp_confess "Graph::add_vertex: use add_vertices for more than one vertex" if @_ != 2;
    __carp_confess "Graph::add_vertex: undef vertex" if grep !defined, @_;
    if (&is_multivertexed) {
	push @_, _GEN_ID;
	goto &add_vertex_by_id;
    }
    my $g = $_[0];
    $g->[ _V ]->set_path( @_[1..$#_] );
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $g;
}

sub has_vertex {
    my $g = $_[0];
    my $V = $g->[ _V ];
    return exists $V->[ _s ]->{ $_[1] } if ($V->[ _f ] & _LIGHT);
    $V->has_path( $_[1] );
}

sub _vertices05 {
    my $g = $_[0];
    my @v = $g->[ _V ]->paths( @_[1..$#_] );
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

sub _add_edge {
    my $g = $_[0];
    my $V = $g->[ _V ];
    if (($V->[ _f ]) & _LIGHT) {
	$g->add_vertex( $_ ) for grep !exists $V->[ _s ]->{ $_ }, @_[1..$#_];
	return map $V->[ _s ]->{ $_ }, @_[1..$#_];
    }
    my @e;
    for my $v ( @_[1..$#_] ) {
	$g->add_vertex( $v ) unless $V->has_path( $v );
	push @e, $V->_get_path_id( $v );
    }
    return @e;
}

sub _union_find_add_edge {
    my ($g, $u, $v) = @_;
    $g->[ _U ]->union($u, $v);
}

sub add_edge {
    &expect_hyperedged if @_ != 3;
    my $g = $_[0];
    if (&is_multiedged) {
	__carp_confess "Graph::add_edge: use add_edges for more than one edge"
	    unless @_ == 3 || &is_hyperedged;
	push @_, _GEN_ID;
	goto &add_edge_by_id;
    }
    my @e = &_add_edge;
    $g->[ _E ]->set_path( @e );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @e ) if &has_union_find;
    return $g;
}

sub _vertex_ids {
    my $g = $_[0];
    my $V = $g->[ _V ];
    if (($V->[ _f ] & _LIGHT)) {
	return if grep !exists $V->[ _s ]->{ $_ }, @_[1..$#_];
	return map $V->[ _s ]->{ $_ }, @_[1..$#_];
    }
    my @e;
    for my $v ( @_[1..$#_] ) {
	return () unless $V->has_path( $v );
	push @e, $V->_get_path_id( $v );
    }
    return @e;
}

sub has_edge {
    my $g = $_[0];
    my $E = $g->[ _E ];
    my $V = $g->[ _V ];
    my @i;
    if (($V->[ _f ] & _LIGHT) && @_ == 3) {
	return 0 unless
	    exists $V->[ _s ]->{ $_[1] } &&
	    exists $V->[ _s ]->{ $_[2] };
	@i = @{ $V->[ _s ] }{ @_[ 1, 2 ] };
    } else {
	@i = &_vertex_ids;
	return 0 if @i == 0 && @_ - 1;
    }
    my $f = $E->[ _f ];
    if (@i == 2 && !($f & (_HYPER|_REF|_UNIQ))) { # Fast path.
	@i = sort @i if ($f & _UNORD);
	return exists $E->[ _s ]->{ $i[0] } &&
	       exists $E->[ _s ]->{ $i[0] }->{ $i[1] } ? 1 : 0;
    } else {
	return defined $E->_get_path_id( @i ) ? 1 : 0;
    }
}

sub _edges05 {
    my $g = shift;
    my $V = $g->[ _V ];
    my @e = $g->[ _E ]->paths( @_ );
    return @e if !wantarray;
    map [ map { my @v = $V->_get_id_path($_);
		@v == 1 ? $v[0] : \@v } @$_ ], @e;
}

*unique_edges = \&_edges05;

sub edges {
    my $g = $_[0];
    my @e = $g->_edges05;
    return @e if !(&is_multiedged || &is_countedged);
    return map +(($_) x $g->get_edge_count(@$_)), @e if wantarray;
    my $E = 0;
    $E += $g->get_edge_count(@$_) for @e;
    return $E;
}

sub has_edges {
    my $g = shift;
    scalar $g->[ _E ]->has_paths( @_ );
}

###
# by_id
#

sub add_vertex_by_id {
    my $g = $_[0];
    &expect_multivertexed;
    $g->[ _V ]->set_path_by_multi_id( @_[1..$#_] );
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $g;
}

sub add_vertex_get_id {
    my $g = $_[0];
    &expect_multivertexed;
    my $id = $g->[ _V ]->set_path_by_multi_id( $_[1], _GEN_ID );
    $g->[ _G ]++;
    &_union_find_add_vertex if &has_union_find;
    return $id;
}

sub has_vertex_by_id {
    &expect_multivertexed;
    $_[0]->[ _V ]->has_path_by_multi_id( @_[1..$#_] );
}

sub delete_vertex_by_id {
    &expect_multivertexed;
    &expect_non_unionfind;
    my $g = shift;
    my $V = $g->[ _V ];
    return unless $V->has_path_by_multi_id( @_ );
    # TODO: what to about the edges at this vertex?
    # If the multiness of this vertex goes to zero, delete the edges?
    $V->del_path_by_multi_id( @_ );
    $g->[ _G ]++;
    return $g;
}

sub get_multivertex_ids {
    &expect_multivertexed;
    my $g = shift;
    $g->[ _V ]->get_multi_ids( @_ );
}

sub add_edge_by_id {
    &expect_multiedged;
    my $g = $_[0];
    my $id = pop;
    my @i = &_add_edge;
    push @_, $id;
    $g->[ _E ]->set_path_by_multi_id( @i, $id );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @i ) if &has_union_find;
    return $g;
}

sub add_edge_get_id {
    my $g = $_[0];
    &expect_multiedged;
    my @i = &_add_edge;
    my $id = $g->[ _E ]->set_path_by_multi_id( @i, _GEN_ID );
    $g->[ _G ]++;
    $g->_union_find_add_edge( @i ) if &has_union_find;
    return $id;
}

sub has_edge_by_id {
    my $g = $_[0];
    &expect_multiedged;
    my $id = pop;
    my @i = &_vertex_ids;
    push @_, $id;
    return 0 if @i == 0 && @_ - 1;
    $g->[ _E ]->has_path_by_multi_id( @i, $id );
}

sub delete_edge_by_id {
    my $g = $_[0];
    &expect_multiedged;
    &expect_non_unionfind;
    my $V = $g->[ _E ];
    my $id = pop;
    my @i = &_vertex_ids;
    return unless $V->has_path_by_multi_id( @i, $id );
    $V->del_path_by_multi_id( @i, $id );
    $g->[ _G ]++;
    return $g;
}

sub get_multiedge_ids {
    my $g = $_[0];
    &expect_multiedged;
    my @i = &_vertex_ids;
    return unless @i;
    $g->[ _E ]->get_multi_ids( @i );
}

###
# Neighbourhood.
#

sub _edges_at {
    my $g = $_[0];
    my $V = $g->[ _V ];
    my $E = $g->[ _E ];
    my @e;
    my $en = 0;
    my %ev;
    for my $v ( @_[1..$#_] ) {
	my $vi = $V->_get_path_id( $v );
	next unless defined $vi;
	my $Ei = $E->_ids;
	for (my $ei = $#$Ei; $ei >= 0; $ei--) {
	    next if !defined(my $ev = $Ei->[$ei]);
	    if (wantarray) {
		push @e, [ $ei, $ev ] for grep $_ == $vi && !$ev{$ei}++, @$ev;
	    } else {
		$en += grep $_ == $vi && !$ev{$ei}++, @$ev;
	    }		    
	}
    }
    return wantarray ? @e : $en;
}

sub _edges {
    my $g = $_[0];
    my $n = pop;
    my $i = $n == _S ? 0 : -1;  # _edges_from() or _edges_to()
    my $V = $g->[ _V ];
    my $E = $g->[ _E ];
    my $N = $g->[ $n ];
    unless (defined $N && $N->[ 0 ] == $g->[ _G ]) {
	$g->[ $n ]->[ 1 ] = { };
	$N = $g->[ $n ];
	my $u = $E->[ _f ] & _UNORD;
	my $Ei = $E->_ids;
	for (my $ei = $#$Ei; $ei >= 0; $ei--) {
	    next if !defined(my $ev = $Ei->[$ei]);
	    next unless @$ev;
	    my $e = [ $ei, $ev ];
	    if ($u) {
		push @{ $N->[ 1 ]->{ $ev->[ 0] } }, $e;
		push @{ $N->[ 1 ]->{ $ev->[-1] } }, $e;
	    } else {
		my $e = [ $ei, $ev ];
		push @{ $N->[ 1 ]->{ $ev->[$i] } }, $e;
	    }
	}
	$N->[ 0 ] = $g->[ _G ];
    }
    my @e;
    my @at = @_[1..$#_];
    my %at; @at{@at} = ();
    for my $v ( @at ) {
	my $vi = $V->_get_path_id( $v );
	next unless defined $vi && exists $N->[ 1 ]->{ $vi };
	push @e, @{ $N->[ 1 ]->{ $vi } };
    }
    if (wantarray && &is_undirected) {
	my @i = map $V->_get_path_id( $_ ), @_[1..$#_];
	$_ = [ $_->[ 0 ], [ reverse @{ $_->[ 1 ] } ] ]
	    for grep $_->[ 1 ]->[ $i ] != $i[ $i ], @e;
    }
    return @e;
}

sub _edges_from {
    push @_, _S;
    goto &_edges;
}

sub _edges_to {
    push @_, _P;
    goto &_edges;
}

sub _edges_id_path {
    my $g = shift;
    my $V  = $g->[ _V ];
    [ map { my @v = $V->_get_id_path($_);
	    @v == 1 ? $v[0] : [ @v ] }
          @{ $_[0]->[1] } ];
}

sub edges_at {
    my $g = $_[0];
    goto &_edges_at if !wantarray;
    map $g->_edges_id_path($_), &_edges_at;
}

sub edges_from {
    my $g = $_[0];
    map $g->_edges_id_path($_), &_edges_from;
}

sub edges_to {
    goto &edges_from if &is_undirected;
    my $g = $_[0];
    map $g->_edges_id_path($_), &_edges_to;
}

sub successors {
    $_[0]->[ _E ]->_successors(@_);
}

sub predecessors {
    $_[0]->[ _E ]->_predecessors(@_);
}

sub _all_cessors {
    my $method = pop;
    my ($g, @init) = @_;
    my %todo;
    @todo{@init} = @init;
    my %seen;
    my %init = %todo;
    my %self;
    while (keys %todo) {
      for my $t (values %todo) {
        $seen{$t} = delete $todo{$t};
        for my $v ($g->$method($t)) {
	  $self{$v} = undef if exists $init{$v};
	  $todo{$v} = $v unless exists $seen{$v};
        }
      }
    }
    delete @seen{ grep !(exists $self{$_} || $g->has_edge($_, $_)), @init };
    return values %seen;
}

sub all_successors {
    &expect_directed;
    push @_, 'successors';
    goto &_all_cessors;
}

sub all_predecessors {
    &expect_directed;
    push @_, 'predecessors';
    goto &_all_cessors;
}

sub neighbours {
    my $g = $_[0];
    my $V  = $g->[ _V ];
    my @s = map { my @v = @{ $_->[ 1 ] }; shift @v; @v } &_edges_from;
    my @p = map { my @v = @{ $_->[ 1 ] }; pop   @v; @v } &_edges_to;
    my %n;
    @n{ @s } = @s;
    @n{ @p } = @p;
    map $V->_get_id_path($_), keys %n;
}

*neighbors = \&neighbours;

sub all_neighbours {
    my $g = shift;
    my @init = @_;
    my @v = @init;
    my (%new, %n);
    while (1) {
      @v = $g->neighbours(@v);
      @new{@v} = @v;
      delete @new{keys %n};
      last if !keys %new;  # Leave if no new found.
      @v = @n{keys %new} = values %new;
    }
    delete @n{ grep !$g->has_edge($_, $_), @init };
    return values %n;
}

*all_neighbors = \&all_neighbours;

sub all_reachable {
    &directed ? goto &all_successors : goto &all_neighbors;
}

sub delete_edge {
    my $g = $_[0];
    &expect_non_unionfind;
    my @i = &_vertex_ids;
    return $g unless @i;
    my $i = $g->[ _E ]->_get_path_id( @i );
    return $g unless defined $i;
    $g->[ _E ]->_del_id( $i );
    $g->[ _G ]++;
    return $g;
}

sub delete_vertex {
    &expect_non_unionfind;
    my $g = $_[0];
    return $g if @_ != 2;
    my $V = $g->[ _V ];
    return $g unless $V->has_path( $_[1] );
    # TODO: _edges_at is excruciatingly slow (rt.cpan.org 92427)
    my $E = $g->[ _E ];
    $E->_del_id( $_->[ 0 ] ) for &_edges_at;
    $V->del_path( $_[1] );
    $g->[ _G ]++;
    return $g;
}

sub get_vertex_count {
    my $g = shift;
    $g->[ _V ]->_get_path_count( @_ ) || 0;
}

sub get_edge_count {
    my $g = $_[0];
    my @i = &_vertex_ids;
    return 0 unless @i;
    $g->[ _E ]->_get_path_count( @i ) || 0;
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
    return $in;
}

sub out_degree {
    my $g = $_[0];
    return undef unless @_ > 1 && &has_vertex;
    my $out = 0;
    $out += $g->get_edge_count( @$_ ) for &edges_from;
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
    my $p = &predecessors;
    my $s = &successors;
    if (&is_self_loop_vertex) {
	$p--;
	$s--;
    }
    $p > 0 && $s > 0;
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

sub sink_vertices {
    my $g = $_[0];
    grep $g->is_sink_vertex($_), $g->_vertices05;
}

sub source_vertices {
    my $g = shift;
    grep $g->is_source_vertex($_), $g->_vertices05;
}

sub successorless_vertices {
    my $g = shift;
    grep $g->is_successorless_vertex($_), $g->_vertices05;
}

sub predecessorless_vertices {
    my $g = shift;
    grep $g->is_predecessorless_vertex($_), $g->_vertices05;
}

sub successorful_vertices {
    my $g = shift;
    grep $g->is_successorful_vertex($_), $g->_vertices05;
}

sub predecessorful_vertices {
    my $g = shift;
    grep $g->is_predecessorful_vertex($_), $g->_vertices05;
}

sub isolated_vertices {
    my $g = shift;
    grep $g->is_isolated_vertex($_), $g->_vertices05;
}

sub interior_vertices {
    my $g = shift;
    grep $g->is_interior_vertex($_), $g->_vertices05;
}

sub exterior_vertices {
    my $g = shift;
    grep $g->is_exterior_vertex($_), $g->_vertices05;
}

sub self_loop_vertices {
    my $g = shift;
    grep $g->is_self_loop_vertex($_), $g->_vertices05;
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

# Vertex attributes.

sub set_vertex_attribute {
    my $g = $_[0];
    &expect_non_multivertexed;
    my $value = pop;
    my $attr  = pop;
    &add_vertex unless &has_vertex;
    $g->[ _V ]->_set_path_attr( $_[1], $attr, $value );
}

sub set_vertex_attribute_by_id {
    &expect_multivertexed;
    my $value = pop;
    my $attr  = pop;
    &add_vertex_by_id unless &has_vertex_by_id;
    $_[0]->[ _V ]->_set_path_attr( @_[1..$#_], $attr, $value );
}

sub set_vertex_attributes {
    &expect_non_multivertexed;
    my $attr = pop;
    &add_vertex unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_set_path_attrs( @_, $attr );
}

sub set_vertex_attributes_by_id {
    &expect_multivertexed;
    my $attr = pop;
    &add_vertex_by_id unless &has_vertex_by_id;
    $_[0]->[ _V ]->_set_path_attrs( @_[1..$#_], $attr );
}

sub has_vertex_attributes {
    &expect_non_multivertexed;
    return 0 unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_has_path_attrs( @_ );
}

sub has_vertex_attributes_by_id {
    &expect_multivertexed;
    return 0 unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_has_path_attrs( @_ );
}

sub has_vertex_attribute {
    &expect_non_multivertexed;
    my $attr = pop;
    return 0 unless &has_vertex;
    $_[0]->[ _V ]->_has_path_attr( @_[1..$#_], $attr );
}

sub has_vertex_attribute_by_id {
    &expect_multivertexed;
    my $attr = pop;
    return 0 unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_has_path_attr( @_, $attr );
}

sub get_vertex_attributes {
    &expect_non_multivertexed;
    return undef unless &has_vertex;
    my $g = shift;
    scalar $g->[ _V ]->_get_path_attrs( @_ );
}

sub get_vertex_attributes_by_id {
    &expect_multivertexed;
    return undef unless &has_vertex_by_id;
    my $g = shift;
    scalar $g->[ _V ]->_get_path_attrs( @_ );
}

sub get_vertex_attribute {
    &expect_non_multivertexed;
    my $attr = pop;
    return unless &has_vertex;
    my $g = shift;
    scalar $g->[ _V ]->_get_path_attr( @_, $attr );
}

sub get_vertex_attribute_by_id {
    &expect_multivertexed;
    my $attr = pop;
    return unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_get_path_attr( @_, $attr );
}

sub get_vertex_attribute_names {
    &expect_non_multivertexed;
    return unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_get_path_attr_names( @_ );
}

sub get_vertex_attribute_names_by_id {
    &expect_multivertexed;
    return unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_get_path_attr_names( @_ );
}

sub get_vertex_attribute_values {
    &expect_non_multivertexed;
    return unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_get_path_attr_values( @_ );
}

sub get_vertex_attribute_values_by_id {
    &expect_multivertexed;
    return unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_get_path_attr_values( @_ );
}

sub delete_vertex_attributes {
    &expect_non_multivertexed;
    return undef unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_del_path_attrs( @_ );
}

sub delete_vertex_attributes_by_id {
    &expect_multivertexed;
    return undef unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_del_path_attrs( @_ );
}

sub delete_vertex_attribute {
    &expect_non_multivertexed;
    my $attr = pop;
    return undef unless &has_vertex;
    my $g = shift;
    $g->[ _V ]->_del_path_attr( @_, $attr );
}

sub delete_vertex_attribute_by_id {
    &expect_multivertexed;
    my $attr = pop;
    return undef unless &has_vertex_by_id;
    my $g = shift;
    $g->[ _V ]->_del_path_attr( @_, $attr );
}

# Edge attributes.

sub set_edge_attribute {
    &expect_non_multiedged;
    my $value = pop;
    my $attr  = pop;
    &add_edge unless &has_edge;
    $_[0]->[ _E ]->_set_path_attr( &_vertex_ids, $attr, $value );
}

sub set_edge_attribute_by_id {
    &expect_multiedged;
    my $value = pop;
    my $attr  = pop;
    &add_edge_by_id unless &has_edge_by_id;
    my $id = pop;
    $_[0]->[ _E ]->_set_path_attr( &_vertex_ids, $id, $attr, $value );
}

sub set_edge_attributes {
    &expect_non_multiedged;
    my $attr = pop;
    &add_edge unless &has_edge;
    $_[0]->[ _E ]->_set_path_attrs( &_vertex_ids, $attr );
}

sub set_edge_attributes_by_id {
    &expect_multiedged;
    my $attr = pop;
    &add_edge_by_id unless &has_edge_by_id;
    my $id = pop;
    $_[0]->[ _E ]->_set_path_attrs( &_vertex_ids, $id, $attr );
}

sub has_edge_attributes {
    &expect_non_multiedged;
    return 0 unless &has_edge;
    $_[0]->[ _E ]->_has_path_attrs( &_vertex_ids );
}

sub has_edge_attributes_by_id {
    my $g = $_[0];
    &expect_multiedged;
    return 0 unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_has_path_attrs( &_vertex_ids, $id );
}

sub has_edge_attribute {
    my $g = $_[0];
    &expect_non_multiedged;
    my $attr = pop;
    return 0 unless &has_edge;
    $g->[ _E ]->_has_path_attr( &_vertex_ids, $attr );
}

sub has_edge_attribute_by_id {
    my $g = $_[0];
    &expect_multiedged;
    my $attr = pop;
    return 0 unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_has_path_attr( &_vertex_ids, $id, $attr );
}

sub get_edge_attributes {
    my $g = $_[0];
    &expect_non_multiedged;
    return undef unless &has_edge;
    scalar $g->[ _E ]->_get_path_attrs( &_vertex_ids );
}

sub get_edge_attributes_by_id {
    my $g = $_[0];
    &expect_multiedged;
    return unless &has_edge_by_id;
    my $id = pop;
    scalar $g->[ _E ]->_get_path_attrs( &_vertex_ids, $id );
}

sub get_edge_attribute {
    my $g = $_[0];
    &expect_non_multiedged;
    my $attr = pop;
    return undef unless &has_edge;
    my @i = &_vertex_ids;
    return undef if @i == 0 && @_ - 1;
    $g->[ _E ]->_get_path_attr( @i, $attr );
}

sub get_edge_attribute_by_id {
    my $g = $_[0];
    &expect_multiedged;
    my $attr = pop;
    return unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_get_path_attr( &_vertex_ids, $id, $attr );
}

sub get_edge_attribute_names {
    my $g = $_[0];
    &expect_non_multiedged;
    return unless &has_edge;
    $g->[ _E ]->_get_path_attr_names( &_vertex_ids );
}

sub get_edge_attribute_names_by_id {
    my $g = $_[0];
    &expect_multiedged;
    return unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_get_path_attr_names( &_vertex_ids, $id );
}

sub get_edge_attribute_values {
    my $g = $_[0];
    &expect_non_multiedged;
    return unless &has_edge;
    $g->[ _E ]->_get_path_attr_values( &_vertex_ids );
}

sub get_edge_attribute_values_by_id {
    my $g = $_[0];
    &expect_multiedged;
    return unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_get_path_attr_values( &_vertex_ids, $id );
}

sub delete_edge_attributes {
    my $g = $_[0];
    &expect_non_multiedged;
    return unless &has_edge;
    $g->[ _E ]->_del_path_attrs( &_vertex_ids );
}

sub delete_edge_attributes_by_id {
    my $g = $_[0];
    &expect_multiedged;
    return unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_del_path_attrs( &_vertex_ids, $id );
}

sub delete_edge_attribute {
    my $g = $_[0];
    &expect_non_multiedged;
    my $attr = pop;
    return unless &has_edge;
    $g->[ _E ]->_del_path_attr( &_vertex_ids, $attr );
}

sub delete_edge_attribute_by_id {
    my $g = $_[0];
    &expect_multiedged;
    my $attr = pop;
    return unless &has_edge_by_id;
    my $id = pop;
    $g->[ _E ]->_del_path_attr( &_vertex_ids, $id, $attr );
}

sub add_vertices {
    my $g = shift;
    $g->add_vertex( $_ ) for @_;
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
	for grep !$seen{$_}++, map ref() ? $_->[0] : $_, $g->[ _V ]->paths(@_);
    return $g;
}

sub as_hashes {
    my ($g) = @_;
    my (%n, %e);
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
            $e{ $e->[0] }{ $e->[1] } = {
                map +($_ => $g->get_edge_attributes_by_id(@$e, $_) || {}),
                    $g->get_multiedge_ids(@$e)
            };
        }
    } else {
        $e{ $_->[0] }{ $_->[1] } = $g->get_edge_attributes(@$_) || {}
            for $g->edges;
    }
    ( \%n, \%e );
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
    my $g = shift;
    my %opt = _get_options( \@_ );
    my $c =
	(ref $g)->new(map +($_ => $g->$_ ? 1 : 0),
		      qw(directed
			 refvertexed
			 countvertexed
			 multivertexed
			 hyperedged
			 countedged
			 multiedged
			 omniedged
		         __stringified));
    $c->add_vertex($_) for $g->isolated_vertices;
    $c->add_edge(@$_) for $g->_edges05;
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
    my $g = shift;
    my $t = $g->copy;
    return $t if !$t->directed;
    $t->transpose_edge(@$_) for $t->_edges05;
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
  my $s = $g->new;
  my @u = grep $g->has_vertex($_), @$src;
  my @v = defined($dst) ? grep $g->has_vertex($_), @$dst : @u;
  $s->add_vertices(@u, defined($dst) ? @v : ());
  for my $u (@u) {
    $s->add_edges( map [$u, $_], grep $g->has_edge($u, $_), @v );
  }
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
    my $w = pop;
    &add_vertex;
    push @_, $defattr, $w;
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
    my $w = pop;
    &add_vertex_by_id;
    push @_, $defattr, $w;
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

sub expect_non_multivertexed {
    _expected('non-multivertexed') if &is_multivertexed;
}

sub expect_non_multiedged {
    _expected('non-multiedged') if &is_multiedged;
}

sub expect_multiedged {
    _expected('multiedged') unless &is_multiedged;
}

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
    my $g = shift;
    my @V = $g->_vertices05;
    @V[rand @V];
}

sub random_edge {
    my $g = shift;
    my @E = $g->_edges05;
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
                 $g->_edges05;
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
    my $g = shift;
    my %opt = @_ == 1 ? ( first_root => $_[0] ) : _get_options( \@_ );
    my %unseen;
    my @unseen = $g->_vertices05;
    @unseen{ @unseen } = @unseen;
    @unseen = _shuffle @unseen;
    my $r;
    if (exists $opt{ start }) {
	$opt{ first_root } = $opt{ start };
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
    my ($g, $h, $add, $etc) = splice @_, 0, 4; # Leave %opt in @_.

    my ($opt, $unseenh, $unseena, $r, $next, $code, $attr) = $g->_root_opt(@_);
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
    my $g = shift;
    require Graph::MSTHeapElem;
    $g->_heap_walk(Graph->new(directed => 0), \&_MST_add, undef, @_);
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
  my $g = shift;
  my $c = Graph->new(directed => 0);
  $c->add_vertex($_) for $g->isolated_vertices; # TODO: if iv ...
  $c->add_edge(@$_) for $g->_edges05;
  return $c;
}

sub undirected_copy {
    &expect_directed;
    return _check_cache($_[0], 'undirected', \&_undirected_copy_compute);
}

*undirected_copy_graph = \&undirected_copy;

sub directed_copy {
    &expect_undirected;
    my $g = shift;
    my $c = Graph::Directed->new;
    $c->add_vertex($_) for $g->isolated_vertices; # TODO: if iv ...
    for my $e ($g->_edges05) {
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
     'connectivity'        => '_ccc',
     'strong_connectivity' => '_scc',
     'biconnectivity'      => '_bcc',
     'SPT_Dijkstra'        => '_spt_di',
     'SPT_Bellman_Ford'    => '_spt_bf',
     'undirected'          => '_undirected',
    );

sub _check_cache {
    my ($g, $type, $code) = splice @_, 0, 3;
    my $c = $_cache_type{$type};
    __carp_confess "Graph: unknown cache type '$type'" if !defined $c;
    my $a = $g->get_graph_attribute($c);
    unless (defined $a && $a->[ 0 ] == $g->[ _G ]) {
	$a->[ 0 ] = $g->[ _G ];
	$a->[ 1 ] = $code->( $g, @_ );
	$g->set_graph_attribute($c, $a);
    }
    return $a->[ 1 ];
}

sub _clear_cache {
    my ($g, $type) = @_;
    my $c = $_cache_type{$type};
    __carp_confess "Graph: unknown cache type '$type'" if !defined $c;
    $g->delete_graph_attribute($c);
}

sub connectivity_clear_cache {
    my $g = shift;
    _clear_cache($g, 'connectivity');
}

sub strong_connectivity_clear_cache {
    my $g = shift;
    _clear_cache($g, 'strong_connectivity');
}

sub biconnectivity_clear_cache {
    my $g = shift;
    _clear_cache($g, 'biconnectivity');
}

sub SPT_Dijkstra_clear_cache {
    my $g = shift;
    _clear_cache($g, 'SPT_Dijkstra');
    $g->delete_graph_attribute('SPT_Dijkstra_first_root');
}

sub SPT_Bellman_Ford_clear_cache {
    my $g = shift;
    _clear_cache($g, 'SPT_Bellman_Ford');
}

sub undirected_copy_clear_cache {
    my $g = shift;
    _clear_cache($g, 'undirected_copy');
}

###
# Connected components.
#

sub _connected_components_compute {
    my $g = $_[0];
    my %cce;
    my %cci;
    my $cc = 0;
    if (&has_union_find) {
	my $UF = $g->_get_union_find();
	my $V  = $g->[ _V ];
	my %icce; # Isolated vertices.
	my %icci;
	my $icc = 0;
	for my $v ( $g->unique_vertices ) {
	    $cc = $UF->find( $V->_get_path_id( $v ) );
	    if (defined $cc) {
		$cce{ $v } = $cc;
		push @{ $cci{ $cc } }, $v;
	    } else {
		$icce{ $v } = $icc;
		push @{ $icci{ $icc } }, $v;
		$icc++;
	    }
	}
	if ($icc) {
	    @cce{ keys %icce } = values %icce;
	    @cci{ keys %icci } = values %icci;
	}
    } else {
	require Graph::Traversal::DFS;
	my @u = $g->unique_vertices;
	my %r; @r{ @u } = @u;
	my $froot = sub {
	    (each %r)[1];
	};
	my $nroot = sub {
	    $cc++ if keys %r;
	    (each %r)[1];
	};
	my $t = Graph::Traversal::DFS->new($g,
					   first_root => $froot,
					   next_root  => $nroot,
					   pre => sub {
					       my ($v, $t) = @_;
					       $cce{ $v } = $cc;
					       push @{ $cci{ $cc } }, $v;
					       delete $r{ $v };
					   },
					   @_[1..$#_]);
	$t->dfs;
    }
    return [ \%cce, \%cci ];
}

sub _connected_components {
    my $ccc = _check_cache($_[0], 'connectivity',
			   \&_connected_components_compute);
    return @{ $ccc };
}

sub connected_component_by_vertex {
    &expect_undirected;
    my ($g, $v) = @_;
    my ($CCE, $CCI) = &_connected_components;
    return $CCE->{ $v };
}

sub connected_component_by_index {
    &expect_undirected;
    my ($g, $i) = @_;
    my ($CCE, $CCI) = &_connected_components;
    return unless my $value = (values %$CCI)[$i];
    return @$value;
}

sub connected_components {
    &expect_undirected;
    my ($CCE, $CCI) = &_connected_components;
    return values %{ $CCI };
}

sub same_connected_components {
    &expect_undirected;
    my ($g, $u, @args) = @_;
    my ($c, @d);
    if (&has_union_find) {
	my $UF = &_get_union_find;
	my $V  = $g->[ _V ];
	$c = $UF->find( $V->_get_path_id ( $u ) );
	@d = map scalar $UF->find( $V->_get_path_id( $_ ) ), @args;
    } else {
	my ($CCE, $CCI) = &_connected_components;
	$c = $CCE->{ $u };
	@d = map $CCE->{ $_ }, @args;
    }
    return 0 if grep !(defined($_) && $_ eq $c), @d;
    return 1;
}

my $super_component = sub { join("+", sort @_) };

sub connected_graph {
    &expect_undirected;
    my ($g, %opt) = @_;
    my $cg = Graph->new(undirected => 1);
    if ($g->has_union_find && $g->vertices == 1) {
	# TODO: super_component?
	$cg->add_vertices($g->vertices);
    } else {
	my $sc_cb =
	    exists $opt{super_component} ?
		$opt{super_component} : $super_component;
	for my $cc ( $g->connected_components() ) {
	    my $sc = $sc_cb->(@$cc);
	    $cg->add_vertex($sc);
	    $cg->set_vertex_attribute($sc, 'subvertices', [ @$cc ]);
	}
    }
    return $cg;
}

sub is_connected {
    &expect_undirected;
    my ($CCE, $CCI) = &_connected_components;
    return keys %{ $CCI } == 1;
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
    my $g = shift;
    require Graph::Traversal::DFS;
    my $t = Graph::Traversal::DFS->new($g);
    my @d = reverse $t->dfs;
    my @c;
    my $h = $g->transpose_graph;
    my $u =
	Graph::Traversal::DFS->new($h,
				   next_root => sub {
				       my ($t, $u) = @_;
				       my $root;
				       while (defined($root = shift @d)) {
					   last if exists $u->{ $root };
				       }
				       if (defined $root) {
					   push @c, [];
					   return $root;
				       } else {
					   return;
				       }
				   },
				   pre => sub {
				       my ($v, $t) = @_;
				       push @{ $c[-1] }, $v;
				   },
				   @_);
    $u->dfs;
    return \@c;
}

sub _strongly_connected_components {
    my $g = shift;
    my $type = 'strong_connectivity';
    my $scc = _check_cache($g, $type,
			   \&_strongly_connected_components_compute, @_);
    return defined $scc ? @$scc : ( );
}

sub strongly_connected_components {
    &expect_directed;
    my $g = shift;
    $g->_strongly_connected_components(@_);
}

sub strongly_connected_component_by_vertex {
    &expect_directed;
    my $g = shift;
    my $v = shift;
    my @scc = $g->_strongly_connected_components( next_alphabetic => 1, @_ );
    for (my $i = 0; $i <= $#scc; $i++) {
	for (my $j = 0; $j <= $#{ $scc[$i] }; $j++) {
	    return $i if $scc[$i]->[$j] eq $v;
	}
    }
    return;
}

sub strongly_connected_component_by_index {
    &expect_directed;
    my $g = shift;
    my $i = shift;
    my $c = ( $g->_strongly_connected_components(@_) )[ $i ];
    return defined $c ? @{ $c } : ();
}

sub same_strongly_connected_components {
    &expect_directed;
    my $g = shift;
    my @scc = $g->_strongly_connected_components( next_alphabetic => 1, @_ );
    my @i;
    while (@_) {
	my $v = shift;
	for (my $i = 0; $i <= $#scc; $i++) {
	    for (my $j = 0; $j <= $#{ $scc[$i] }; $j++) {
		next if $scc[$i]->[$j] ne $v;
		push @i, $i;
		return 0 if @i > 1 && $i[-1] ne $i[0];
	    }
	}
    }
    return 1;
}

sub is_strongly_connected {
    &expect_directed;
    my $g = shift;
    require Graph::Traversal::DFS;
    my $t = Graph::Traversal::DFS->new($g);
    my @d = reverse $t->dfs;
    my @c;
    my $h = $g->transpose;
    my $u =
	Graph::Traversal::DFS->new($h,
				   next_root => sub {
				       my ($t, $u) = @_;
				       my $root;
				       while (defined($root = shift @d)) {
					   last if exists $u->{ $root };
				       }
				       if (defined $root) {
					   unless (@{ $t->{ roots } }) {
					       push @c, [];
					       return $root;
					   } else {
					       $t->terminate;
					       return;
					   }
				       } else {
					   return;
				       }
				   },
				   pre => sub {
				       my ($v, $t) = @_;
				       push @{ $c[-1] }, $v;
				   },
				   @_);
    $u->dfs;
    return @{ $u->{ roots } } == 1 && keys %{ $u->{ unseen } } == 0;
}

*strongly_connected = \&is_strongly_connected;

sub strongly_connected_graph {
    &expect_directed;
    my $g = shift;
    my %attr = @_;
    require Graph::Traversal::DFS;

    my $t = Graph::Traversal::DFS->new($g);
    my @d = reverse $t->dfs;
    my @c;
    my $h = $g->transpose;
    my $u =
	Graph::Traversal::DFS->new($h,
				   next_root => sub {
				       my ($t, $u) = @_;
				       my $root;
				       while (defined($root = shift @d)) {
					   last if exists $u->{ $root };
				       }
				       if (defined $root) {
					   push @c, [];
					   return $root;
				       } else {
					   return;
				       }
				   },
				   pre => sub {
				       my ($v, $t) = @_;
				       push @{ $c[-1] }, $v;
				   }
				   );

    $u->dfs;

    my $sc_cb;

    _opt_get(\%attr, super_component => \$sc_cb);
    _opt_unknown(\%attr);

    unless (defined $sc_cb) {
	$sc_cb = $super_component;
    }

    my $s = Graph->new;

    my %c;
    my @s;
    for (my $i = 0; $i <  @c; $i++) {
	my $c = $c[$i];
	$s->add_vertex( $s[$i] = $sc_cb->(@$c) );
	$s->set_vertex_attribute($s[$i], 'subvertices', [ @$c ]);
	@c{@$c} = ($i) x @$c;
    }

    my $n = @c;
    for my $v (grep !exists $c{$_}, $g->vertices) {
	$c{$v} = $n;
	$s[$n] = $v;
	$n++;
    }

    for my $e ($g->_edges05) {
	my ($u, $v) = @$e; # @TODO: hyperedges
	unless ($c{$u} == $c{$v}) {
	    my ($p, $q) = @s[ @c{ $u, $v } ];
	    $s->add_edge($p, $q) unless $s->has_edge($p, $q);
	}
    }

    if (my @i = $g->isolated_vertices) {
	$s->add_vertices(map $s[ $c{ $_ } ], @i);
    }

    return $s;
}

###
# Biconnectivity.
#

sub _biconnectivity_out {
  my ($state, $u, $v) = @_;
  if (exists $state->{stack}) {
    my @BC;
    while (@{$state->{stack}}) {
      my $e = pop @{$state->{stack}};
      push @BC, $e;
      last if defined $u && $e->[0] eq $u && $e->[1] eq $v;
    }
    if (@BC) {
      push @{$state->{BC}}, \@BC;
    }
  }
}

sub _biconnectivity_dfs {
  my ($g, $u, $state) = @_;
  $state->{num}->{$u} = $state->{dfs}++;
  $state->{low}->{$u} = $state->{num}->{$u};
  for my $v ($g->successors($u)) {
    unless (exists $state->{num}->{$v}) {
      push @{$state->{stack}}, [$u, $v];
      $state->{pred}->{$v} = $u;
      $state->{succ}->{$u}->{$v}++;
      _biconnectivity_dfs($g, $v, $state);
      if ($state->{low}->{$v} < $state->{low}->{$u}) {
	$state->{low}->{$u} = $state->{low}->{$v};
      }
      if ($state->{low}->{$v} >= $state->{num}->{$u}) {
	_biconnectivity_out($state, $u, $v);
      }
    } elsif (defined $state->{pred}->{$u} &&
	     $state->{pred}->{$u} ne $v &&
	     $state->{num}->{$v} < $state->{num}->{$u}) {
      push @{$state->{stack}}, [$u, $v];
      if ($state->{num}->{$v} < $state->{low}->{$u}) {
	$state->{low}->{$u} = $state->{num}->{$v};
      }
    }
  }
}

sub _biconnectivity_compute {
    my ($g) = @_;
    my %state = (BC=>[], BR=>[], V2BC=>{}, BC2V=>{}, AP=>[], dfs=>0);
    my @u = _shuffle $g->vertices;
    for my $u (@u) {
	next if exists $state{num}->{$u};
	_biconnectivity_dfs($g, $u, \%state);
	_biconnectivity_out(\%state);
	delete $state{stack};
    }

    # Mark the components each vertex belongs to.
    my $bci = 0;
    for my $bc (@{$state{BC}}) {
      $state{V2BC}{$_}{$bci}++ for map @$_, @$bc;
      $bci++;
    }

    # Any isolated vertices get each their own component.
    $state{V2BC}{$_}{$bci++}++
	for grep !exists $state{V2BC}{$_}, $g->vertices;

    for my $v ($g->vertices) {
      $state{BC2V}{$_}{$v}{$_}++ for keys %{$state{V2BC}{$v}};
    }

    # Articulation points / cut vertices are the vertices
    # which belong to more than one component.
    push @{$state{AP}}, grep keys %{$state{V2BC}{$_}} > 1, keys %{$state{V2BC}};

    # Bridges / cut edges are the components of two vertices.
    for my $v (keys %{$state{BC2V}}) {
      my @v = keys %{$state{BC2V}->{$v}};
      if (@v == 2) {
	push @{$state{BR}}, \@v;
      }
    }

    # Create the subgraph components.
    my @sg;
    for my $bc (@{$state{BC}}) {
      my %v;
      my $w = Graph->new(directed => 0);
      for my $e (@$bc) {
	my ($u, $v) = @$e;
	$v{$u}++;
	$v{$v}++;
	$w->add_edge($u, $v);
      }
      push @sg, [ keys %v ];
    }

    return [ $state{AP}, \@sg, $state{BR}, $state{V2BC}, ];
}

sub biconnectivity {
    &expect_undirected;
    my $g = shift;
    my $bcc = _check_cache($g, 'biconnectivity',
			   \&_biconnectivity_compute, @_);
    return defined $bcc ? @$bcc : ( );
}

sub is_biconnected {
    my $g = shift;
    my ($ap) = ($g->biconnectivity(@_))[0];
    return $g->edges >= 2 ? @$ap == 0 : undef ;
}

sub is_edge_connected {
    my $g = shift;
    my ($br) = ($g->biconnectivity(@_))[2];
    return $g->edges >= 2 ? @$br == 0 : undef;
}

sub is_edge_separable {
    my $g = shift;
    my ($br) = ($g->biconnectivity(@_))[2];
    return $g->edges >= 2 ? @$br > 0 : undef;
}

sub articulation_points {
    my $g = shift;
    my ($ap) = ($g->biconnectivity(@_))[0];
    return @$ap;
}

*cut_vertices = \&articulation_points;

sub biconnected_components {
    my $g = shift;
    my ($bc) = ($g->biconnectivity(@_))[1];
    return @$bc;
}

sub biconnected_component_by_index {
    my $g = shift;
    my $i = shift;
    my ($bc) = ($g->biconnectivity(@_))[1];
    return $bc->[ $i ];
}

sub biconnected_component_by_vertex {
    my $g = shift;
    my $v = shift;
    my ($v2bc) = ($g->biconnectivity(@_))[3];
    return defined $v2bc->{ $v } ? keys %{ $v2bc->{ $v } } : ();
}

sub same_biconnected_components {
    my $g = shift;
    my $u = shift;
    my @u = $g->biconnected_component_by_vertex($u, @_);
    return 0 unless @u;
    my %ubc; @ubc{ @u } = ();
    while (@_) {
	my $v = shift;
	next unless my @v = $g->biconnected_component_by_vertex($v);
	my %vbc; @vbc{ @v } = ();
	my ($vi) = grep exists $vbc{ $_ }, keys %ubc;
	return 0 unless defined $vi;
    }
    return 1;
}

sub biconnected_graph {
    my ($g, %opt) = @_;
    my ($bc, $v2bc) = ($g->biconnectivity, %opt)[1, 3];
    my $bcg = Graph->new(directed => 0);
    my $sc_cb =
	exists $opt{super_component} ?
	    $opt{super_component} : $super_component;
    for my $c (@$bc) {
	$bcg->add_vertex(my $s = $sc_cb->(@$c));
	$bcg->set_vertex_attribute($s, 'subvertices', [ @$c ]);
    }
    my %k;
    for my $i (0..$#$bc) {
	my @u = @{ $bc->[ $i ] };
	for my $j (0..$i-1) {
	    my %j; @j{ @{ $bc->[ $j ] } } = ();
	    next if !grep exists $j{ $_ }, @u;
	    next if $k{ $i }{ $j }++;
	    $bcg->add_edge($sc_cb->(@{$bc->[$i]}), $sc_cb->(@{$bc->[$j]}));
	}
    }
    return $bcg;
}

sub bridges {
    my $g = shift;
    my ($br) = ($g->biconnectivity(@_))[2];
    return defined $br ? @$br : ();
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
	    $h->set_vertex_attribute( $s, $attr, $etc->{ $s });
	    $h->set_vertex_attribute( $s, 'p', $r );
	    $HF->add( Graph::SPTHeapElem->new($r, $s, $etc->{ $s }) );
	}
    }
}

sub _SPT_Dijkstra_compute {
}

sub SPT_Dijkstra {
    my $g = shift;
    my %opt = @_ == 1 ? (first_root => $_[0]) : @_;
    my $first_root = $opt{ first_root };
    unless (defined $first_root) {
	$opt{ first_root } = $first_root = $g->random_vertex();
    }
    my $spt_di = $g->get_graph_attribute('_spt_di');
    unless (defined $spt_di &&
            exists $spt_di->{ $first_root } &&
            $spt_di->{ $first_root }->[ 0 ] == $g->[ _G ]) {
	my %etc;
	require Graph::SPTHeapElem;
	my $sptg = $g->_heap_walk($g->new, \&_SPT_add, \%etc, %opt);
	$spt_di->{ $first_root } = [ $g->[ _G ], $sptg ];
	$g->set_graph_attribute('_spt_di', $spt_di);
    }

    my $spt = $spt_di->{ $first_root }->[ 1 ];

    $spt->set_graph_attribute('SPT_Dijkstra_root', $first_root);

    return $spt;
}

*SSSP_Dijkstra = \&SPT_Dijkstra;

*single_source_shortest_paths = \&SPT_Dijkstra;

sub SP_Dijkstra {
    my ($g, $u, $v) = @_;
    my $sptg = $g->SPT_Dijkstra(first_root => $u);
    my @path = ($v);
    my %seen;
    my $V = $g->vertices;
    my $p;
    while (defined($p = $sptg->get_vertex_attribute($v, 'p'))) {
	last if exists $seen{$p};
	push @path, $p;
	$v = $p;
	$seen{$p}++;
	last if keys %seen == $V || $u eq $v;
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
}

sub SPT_Bellman_Ford {
    my $g = shift;

    my ($opt, $unseenh, $unseena, $r, $next, $code, $attr) = $g->_root_opt(@_);

    unless (defined $r) {
	$r = $g->random_vertex();
	return unless defined $r;
    }

    my $spt_bf = $g->get_graph_attribute('_spt_bf');
    unless (defined $spt_bf &&
	    exists $spt_bf->{ $r } && $spt_bf->{ $r }->[ 0 ] == $g->[ _G ]) {
	my ($p, $d) =
	    $g->_SPT_Bellman_Ford($opt, $unseenh, $unseena,
				  $r, $next, $code, $attr);
	my $h = $g->new;
	for my $v (keys %$p) {
	    my $u = $p->{ $v };
	    $h->set_edge_attribute( $u, $v, $attr,
				    $g->get_edge_attribute($u, $v, $attr));
	    $h->set_vertex_attribute( $v, $attr, $d->{ $v } );
	    $h->set_vertex_attribute( $v, 'p', $u );
	}
	$spt_bf->{ $r } = [ $g->[ _G ], $h ];
	$g->set_graph_attribute('_spt_bf', $spt_bf);
    }

    my $spt = $spt_bf->{ $r }->[ 1 ];

    $spt->set_graph_attribute('SPT_Bellman_Ford_root', $r);

    return $spt;
}

*SSSP_Bellman_Ford = \&SPT_Bellman_Ford;

sub SP_Bellman_Ford {
    my ($g, $u, $v) = @_;
    my $sptg = $g->SPT_Bellman_Ford(first_root => $u);
    my @path = ($v);
    my %seen;
    my $V = $g->vertices;
    my $p;
    while (defined($p = $sptg->get_vertex_attribute($v, 'p'))) {
	last if exists $seen{$p};
	push @path, $p;
	$v = $p;
	$seen{$p}++;
	last if keys %seen == $V;
    }
    # @path = () if @path && "$path[-1]" ne "$u";
    return reverse @path;
}

###
# Transitive Closure.
#

sub TransitiveClosure_Floyd_Warshall {
    my $self = shift;
    my $class = ref $self || $self;
    $self = shift unless ref $self;
    require Graph::TransitiveClosure;
    bless Graph::TransitiveClosure->new($self, @_), $class;
}

*transitive_closure = \&TransitiveClosure_Floyd_Warshall;

sub APSP_Floyd_Warshall {
    my $self = shift;
    my $class = ref $self || $self;
    $self = shift unless ref $self;
    require Graph::TransitiveClosure;
    bless Graph::TransitiveClosure->new($self, path => 1, @_), $class;
}

*all_pairs_shortest_paths = \&APSP_Floyd_Warshall;

sub _transitive_closure_matrix_compute {
}

sub transitive_closure_matrix {
    my $g = shift;
    my $tcm = $g->get_graph_attribute('_tcm');
    if (defined $tcm) {
	if (ref $tcm eq 'ARRAY') { # YECHHH!
	    if ($tcm->[ 0 ] == $g->[ _G ]) {
		$tcm = $tcm->[ 1 ];
	    } else {
		undef $tcm;
	    }
	}
    }
    unless (defined $tcm) {
	my $apsp = $g->APSP_Floyd_Warshall(@_);
	$tcm = $apsp->get_graph_attribute('_tcm');
	$g->set_graph_attribute('_tcm', [ $g->[ _G ], $tcm ]);
    }

    return $tcm;
}

sub path_length {
    shift->transitive_closure_matrix->path_length(@_);
}

sub path_predecessor {
    shift->transitive_closure_matrix->path_predecessor(@_);
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
    my $g = shift;
    my $multiedges = 0;
    for my $e ($g->_edges05) {
	my ($u, @v) = @$e;
	return 0 if grep $u eq $_, @v;
	$multiedges++ if $g->get_edge_count(@$e) > 1;
    }
    return $multiedges;
}

sub is_simple_graph {
    return 1 unless &is_multiedged || &is_countedged;
    my $g = shift;
    return 0 if grep $g->get_edge_count(@$_) > 1, $g->_edges05;
    return 1;
}

sub is_pseudo_graph {
    my $m = &is_countedged || &is_multiedged;
    my $g = shift;
    for my $e ($g->_edges05) {
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
    return 0 unless $g0->vertices == $g1->vertices;
    return 0 unless $g0->_edges05  == $g1->_edges05;
    my %d0;
    $d0{ $g0->in_degree($_) }{ $g0->out_degree($_) }++ for $g0->vertices;
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

sub subgraph_by_radius
{
    my ($g, $n, $rad) = @_;

    return unless defined $n && defined $rad && $rad >= 0;

    my $r = (ref $g)->new;

    return $r->add_vertex($n) if $rad == 0;

    my %h;
    $h{1} = [ [ $n, $g->successors($n) ] ];
    for my $i (1..$rad) {
	$h{$i+1} = [];
	for my $arr (@{ $h{$i} }) {
	    my ($p, @succ) = @{ $arr };
	    for my $s (@succ) {
		$r->add_edge($p, $s);
		push(@{ $h{$i+1} }, [$s, $g->successors($s)]) if $i < $rad;
	    }
	}
    }

    return $r;
}

sub clustering_coefficient {
    my ($g) = @_;
    return unless my @v = $g->vertices;
    my %clustering;

    my $gamma = 0;

    for my $n (@v) {
	my $gamma_v = 0;
	my @neigh = $g->successors($n);
	my %c;
	for my $u (@neigh) {
	    for my $v (grep +(!$c{"$u-$_"} && $g->has_edge($u, $_)), @neigh) {
		$gamma_v++;
		$c{"$u-$v"} = 1;
		$c{"$v-$u"} = 1;
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
