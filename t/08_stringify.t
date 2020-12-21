use strict; use warnings;
use Test::More;

use Graph::Undirected;
use Graph::Directed;

my $g0 = Graph::Undirected->new;
my $g1 = Graph::Directed->new;
is $_, "" for $g0, $g1;
ok !$_->has_edge('a') for $g0, $g1;
ok !$_->has_edge('a', 'a') for $g0, $g1;

my @EDGES = (
    [qw(a b)], [qw(a c)], [qw(a d)], [qw(a e)], [qw(a f)],
    [qw(b c)], [qw(c b)], [qw(b d)], [qw(d b)],
    [qw(b e)], [qw(e b)], [qw(b f)], [qw(f b)],
    [qw(d c)], [qw(e c)], [qw(f c)],
    [qw(d e)], [qw(d f)],
    [qw(e f)],
);
$g0->add_edge(@$_) for @EDGES;
$g1->add_edge(@$_) for @EDGES;
is(Graph::Undirected->new->add_edges(@EDGES), $g0, 'add_edges equivalence');
is(Graph::Directed->new->add_edges(@EDGES), $g1, 'add_edges equivalence');
is(Graph::Undirected->new(edges=>\@EDGES), $g0, 'new(edges) equivalence');
is(Graph::Directed->new(edges=>\@EDGES), $g1, 'new(edges) equivalence');

ok !$_->has_edge('a') for $g0, $g1;
ok !$_->has_edge('a', 'a') for $g0, $g1;
ok !$_->has_edge('a', 'x') for $g0, $g1;

# undirected, directed
my %SUCCESSORS = (
    a => [ [qw(b c d e f)], [qw(b c d e f)] ],
    b => [ [qw(a c d e f)], [qw(c d e f)] ],
    c => [ [qw(a b d e f)], [qw(b)] ],
    d => [ [qw(a b c e f)], [qw(b c e f)] ],
    e => [ [qw(a b c d f)], [qw(b c f)] ],
    f => [ [qw(a b c d e)], [qw(b c)] ],
);
for my $v (sort keys %SUCCESSORS) {
    my ($u, $d) = @{ $SUCCESSORS{$v} };
    for my $t ([$g0, $u], [$g1, $d]) {
	my ($g, $r) = @$t;
	is("@{[sort $g->successors($v)]}", "@$r", "successors u $v");
	my %expected_edges; @expected_edges{ @$r } = ();
	my %not_edges; @not_edges{ grep $_ ne $v, $g->vertices } = ();
	delete @not_edges{ keys %expected_edges };
	ok $g->has_edge($v, $_) for keys %expected_edges;
	ok !$g->has_edge($v, $_) for keys %not_edges;
    }
}

my %PREDECESSORS = (
    a => [ [qw(b c d e f)], [qw()] ],
    b => [ [qw(a c d e f)], [qw(a c d e f)] ],
    c => [ [qw(a b d e f)], [qw(a b d e f)] ],
    d => [ [qw(a b c e f)], [qw(a b)] ],
    e => [ [qw(a b c d f)], [qw(a b d)] ],
    f => [ [qw(a b c d e)], [qw(a b d e)] ],
);
for my $v (sort keys %PREDECESSORS) {
    my ($u, $d) = @{ $PREDECESSORS{$v} };
    for my $t ([$g0, $u], [$g1, $d]) {
	my ($g, $r) = @$t;
	is("@{[sort $g->predecessors($v)]}", "@$r", "predecessors u $v");
	my %expected_edges; @expected_edges{ @$r } = ();
	my %not_edges; @not_edges{ grep $_ ne $v, $g->vertices } = ();
	delete @not_edges{ keys %expected_edges };
	ok $g->has_edge($_, $v) for keys %expected_edges;
	ok !$g->has_edge($_, $v) for keys %not_edges;
    }
}

is_deeply [ $g0->as_hashes ], [
    { map +($_ => {}), keys %SUCCESSORS },
    { map +($_ => { map +($_ => {}), @{ $SUCCESSORS{$_}[0] } }), keys %SUCCESSORS },
], "undirected as_hashes" or diag explain [ $g0->as_hashes ];
is_deeply [ $g1->as_hashes ], [
    { map +($_ => {}), keys %SUCCESSORS },
    { map +($_ => { map +($_ => {}), @{ $SUCCESSORS{$_}[1] } }), keys %SUCCESSORS },
], "directed as_hashes" or diag explain [ $g1->as_hashes ];

is($g0, 'a=b,a=c,a=d,a=e,a=f,b=c,b=d,b=e,b=f,c=d,c=e,c=f,d=e,d=f,e=f')
    for 1..10;
is($g1, 'a-b,a-c,a-d,a-e,a-f,b-c,b-d,b-e,b-f,c-b,d-b,d-c,d-e,d-f,e-b,e-c,e-f,f-b,f-c')
    for 1..10;

is $g0->[ Graph::_V ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=1 flags: _LIGHT
   a    0
   b    1
   c    2
   d    3
   e    4
   f    5
EOF
is $g0->[ Graph::_E ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=2 flags: _UNORD|_LIGHT
 to:    1    2    3    4    5
   0    1    1    1    1    1
   1         1    1    1    1
   2              1    1    1
   3                   1    1
   4                        1
EOF

is $g1->[ Graph::_V ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=1 flags: _LIGHT
   a    0
   b    1
   c    2
   d    3
   e    4
   f    5
EOF
is $g1->[ Graph::_E ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=2 flags: _LIGHT
 to:    1    2    3    4    5
   0    1    1    1    1    1
   1         1    1    1    1
   2    1                    
   3    1    1         1    1
   4    1    1              1
   5    1    1               
EOF

$g1->set_edge_attribute(qw(a b weight 2));
$g1->set_vertex_attribute(qw(a size 2));
is $g1->[ Graph::_V ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=1 flags: _LIGHT
   a 0,{'size' => '2'}
   b    1
   c    2
   d    3
   e    4
   f    5
EOF
is $g1->[ Graph::_E ]->stringify, <<'EOF';
Graph::AdjacencyMap::Light arity=2 flags: _LIGHT
 to:    1    2    3    4    5
   0 {'weight' => '2'}    1    1    1    1
   1         1    1    1    1
   2    1                    
   3    1    1         1    1
   4    1    1              1
   5    1    1               
EOF

my $g2 = Graph::Directed->new(multivertexed => 1, multiedged => 1);
$g2->add_edge(qw(a c));
$g2->set_edge_attribute_by_id(qw(a b x weight 2));
$g2->set_vertex_attribute_by_id(qw(a z other 5));
$g2->set_vertex_attribute_by_id(qw(a 0 other2 6));
is $g2->[ Graph::_V ]->stringify, <<'EOF';
Graph::AdjacencyMap arity=1 flags: _MULTI
   a 0,{'0' => {'other2' => '6'},'z' => {'other' => '5'}}
   b 2,{'0' => {}}
   c 1,{'0' => {}}
EOF
is $g2->[ Graph::_E ]->stringify, <<'EOF';
Graph::AdjacencyMap arity=2 flags: _MULTI
 to:    1    2
   0 {'0' => {}} {'x' => {'weight' => '2'}}
EOF

for my $cv (0, 1) {
    my $g3 = Graph::Directed->new(hyperedged => 1, countvertexed => $cv);
    $g3->add_edge([qw(a c)], [qw(d e f)]);
    $g3->set_edge_attribute([qw(a c)], [qw(e g)], qw(weight 2));
    is $g3->[ Graph::_E ]->stringify, <<'EOF';
Graph::AdjacencyMap arity=0 flags: 0
[[0,1],[2,3,4]]    0
[[0,1],[3,5]] 1,{'weight' => '2'}
EOF
    my $got = [ $g3->as_hashes ];
    is_deeply $got->[1], [
	{ predecessors => [qw(a c)], successors => [qw(d e f)], attributes => {} },
	{ predecessors => [qw(a c)], successors => [qw(e g)], attributes => { weight => 2 } },
    ] or diag explain $got;
}

{
  my $null = Graph->new;
  ok($null, "boolify wins over stringify for empty graph");
  ok($g0, "boolify");
}

{
  # Inspired by
  # rt.cpan.org 93278: SPT_Dijkstra sometimes returns a wrong answer
  use Graph::Directed;
  my $null = Graph::Directed->new;
  for ( 1..5 ) {  # Adds _NOTHING_ -- but dies.
    eval { $null->add_vertex };
    like($@, qr/Graph::add_vertex: use add_vertices for more than one vertex/);
  }
  is($null, "");
}

done_testing;
