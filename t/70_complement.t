use strict; use warnings;
use Test::More tests => 9;

use Graph;
use Graph::Directed;
use Graph::Undirected;

is(Graph->new->complement->edges, 0);

my $g0 = Graph::Directed->new();
$g0->add_edge(qw(a b));
$g0->add_edge(qw(a c));
is($g0, "a-b,a-c");
is($g0->complement, "b-a,b-c,c-a,c-b");

my $g1 = Graph::Undirected->new();
$g1->add_edge(qw(a b));
$g1->add_edge(qw(a c));
is($g1, "a=b,a=c");
is($g1->complement, "b=c,a");

my $g2 = Graph::Directed->new(countedged => 1);
$g2->add_edge(qw(a b));
$g2->add_edge(qw(a c));
is $g2->complement_graph, "b-a,b-c,c-a,c-b";

my $g3 = Graph::Undirected->new();
$g3->add_edge(qw(a b));
is scalar($g3->vertices), 2;
my $c3 = $g3->complement_graph;
is scalar($c3->vertices), 2;
is scalar($c3->edges), 0;
