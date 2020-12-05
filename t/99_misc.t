use strict; use warnings;

use Test::More tests => 38;

use Graph::Directed;
use Graph::Undirected;

my $g0 = Graph::Directed->new;

my @E = ([qw(a b)], [qw(a c)], [qw(b d)], [qw(b e)], [qw(c f)], [qw(c g)]);

$g0->add_edge(@$_) for @E;

is_deeply [ $g0->as_hashes ], [
    { map +($_ => {}), qw(a b c d e f g) },
    {
      a => { map +($_ => {}), qw(b c) },
      b => { map +($_ => {}), qw(d e) },
      c => { map +($_ => {}), qw(f g) },
    },
];

is($g0->subgraph_by_radius('a', 0)->stringify, "a");
is($g0->subgraph_by_radius('a', 1)->stringify, "a-b,a-c");
is($g0->subgraph_by_radius('a', 2)->stringify, "a-b,a-c,b-d,b-e,c-f,c-g");
is($g0->subgraph_by_radius('a', 3)->stringify, "a-b,a-c,b-d,b-e,c-f,c-g");

is($g0->subgraph_by_radius('b', 0)->stringify, "b");
is($g0->subgraph_by_radius('b', 1)->stringify, "b-d,b-e");
is($g0->subgraph_by_radius('b', 2)->stringify, "b-d,b-e");
is($g0->subgraph_by_radius('b', 3)->stringify, "b-d,b-e");

{
    my $gi0 = Graph->new;
    $gi0->set_edge_attribute(qw(a b), weight => 1);
    my $gi1 = Graph->new;
    $gi1->set_vertex_attribute('x', shape => 1);
    $gi1->set_edge_attribute(qw(x y), weight => 2);
    is_deeply [ $gi0->ingest($gi1)->as_hashes ], [
        { x => { shape => 1 }, map +($_ => {}), qw(a b y) },
        { a => { b => { weight => 1 } }, x => { y => { weight => 2 } } },
    ];
}

{
  my $gh = Graph->new(hypervertexed => 1);
  $gh->add_vertex(@$_) for (
      ['a'], [qw(a c)], [qw(a b c)], [qw(a c e)], [qw(a c d)], [],
  );
  $gh->add_edge('a', 'b');
  $gh->add_edge('c', 'd');
  is $gh, "a-b,c-d,[],[a b c],[a c d],[a c e],[a c],[a],[b],[c],[d],[e]";
}

for ({}, {countvertexed => 1}, {hypervertexed => 1}, {multivertexed => 1}) {
  my $gr = Graph::Directed->new(%$_);
  $gr->add_edge(@$_) for @E;
  $gr->rename_vertex('b', 'b1');
  is $gr->subgraph_by_radius('a', 3), "a-b1,a-c,b1-d,b1-e,c-f,c-g";
  $gr->rename_vertices(sub { uc $_[0] });
  is $gr->subgraph_by_radius('A', 3), "A-B1,A-C,B1-D,B1-E,C-F,C-G";
}

my $g1 = Graph::Undirected->new;
$g1->add_edge(@$_) for @E;

is($g1->subgraph_by_radius('a', 0)->stringify, "a");
is($g1->subgraph_by_radius('a', 1)->stringify, "a=b,a=c");
is($g1->subgraph_by_radius('a', 2)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");
is($g1->subgraph_by_radius('a', 3)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");

is($g1->subgraph_by_radius('b', 0)->stringify, "b");
is($g1->subgraph_by_radius('b', 1)->stringify, "a=b,b=d,b=e");
is($g1->subgraph_by_radius('b', 2)->stringify, "a=b,a=c,b=d,b=e");
is($g1->subgraph_by_radius('b', 3)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");

my $g2 = Graph->new;
is_deeply [ $g2->clustering_coefficient ], [],
  'clustering_coefficient with no vertices = empty list';

for my $p (qw(zero
	      one
	      two
	      three
	      four
	      five
	      six
	      seven
	      eight
	      nine
	      ten)) {
    $g2->add_path(split(//, $p));
}

my ($gamma, %clustering) = $g2->clustering_coefficient;

my $eps = 1e-6;

ok(abs($gamma - 0.402222222222222) <= $eps);
ok(abs($clustering{e} - 0.7) <= $eps);
ok(abs($clustering{t} - 1/3) <= $eps);
is($clustering{z}, 0.0);
is($clustering{r}, 1.0);

my %betweenness = $g2->betweenness;

ok(abs($betweenness{e} - 60.3333333333333) <= $eps);
ok(abs($betweenness{t} - 17.1666666666667) <= $eps);
is($betweenness{x}, 0.0);
is($betweenness{u}, 3.0);

{
my $w = '';
local $SIG{__WARN__} = sub { $w = shift };
my $g3 = Graph->new;
$g3->add_edge(0,1);
my @dummy = $g3->SP_Dijkstra(1,0);
is $w, '';
}
