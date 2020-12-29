use strict; use warnings;
use Test::More tests => 12;

use Graph;
use Graph::Directed;
use Graph::Undirected;

my $g = Graph->new;
my $c = $g->complete;

is($c->edges, 0);

my $g0 = Graph::Directed->new();
$g0->add_edge(qw(a b));
$g0->add_edge(qw(a c));

my $g1 = Graph::Undirected->new();
$g1->add_edge(qw(a b));
$g1->add_edge(qw(a c));

is($g0, "a-b,a-c");
is($g0->complete, "a-b,a-c,b-a,b-c,c-a,c-b");

is($g1, "a=b,a=c");
is($g1->complete, "a=b,a=c,b=c");

my $g2 = Graph::Directed->new(countedged => 1);
$g2->add_edge(qw(a b));
$g2->add_edge(qw(a c));

my $c2 = $g2->complete_graph;
is $c2, "a-b,a-c,b-a,b-c,c-a,c-b";

for my $u (qw(a b c)) {
    for my $v (qw(a b c)) {
	next if $u eq $v;
	is($c2->get_edge_count($u, $v), 1);
    }
}
