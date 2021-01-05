use strict;
use warnings;

use Graph;
use Graph::Directed;
use Graph::Undirected;

use Test::More;

my $g0 = Graph::Directed->new;
my @E = ([qw(a b)], [qw(a c)], [qw(b d)], [qw(b e)], [qw(c f)], [qw(c g)]);
$g0->add_edges(@E);

is $g0->subgraph([qw(a b c)], [qw(d e f)]), "b-d,b-e,c-f,a";
is $g0->subgraph([qw(a b c)]), "a-b,a-c";
is $g0->subgraph(['a'],['e']), "a,e";

is($g0->subgraph_by_radius('a', 0)->stringify, "a");
is($g0->subgraph_by_radius('a', 1)->stringify, "a-b,a-c");
is($g0->subgraph_by_radius('a', 2)->stringify, "a-b,a-c,b-d,b-e,c-f,c-g");
is($g0->subgraph_by_radius('a', 3)->stringify, "a-b,a-c,b-d,b-e,c-f,c-g");

is($g0->subgraph_by_radius('b', 0)->stringify, "b");
is($g0->subgraph_by_radius('b', 1)->stringify, "b-d,b-e");
is($g0->subgraph_by_radius('b', 2)->stringify, "b-d,b-e");
is($g0->subgraph_by_radius('b', 3)->stringify, "b-d,b-e");

is($g0->subgraph_by_radius('a', 'b', 1)->stringify, "a-b,a-c,b-d,b-e");

my $g1 = Graph::Undirected->new;
$g1->add_edges(@E);

is($g1->subgraph_by_radius('a', 0)->stringify, "a");
is($g1->subgraph_by_radius('a', 1)->stringify, "a=b,a=c");
is($g1->subgraph_by_radius('a', 2)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");
is($g1->subgraph_by_radius('a', 3)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");

is($g1->subgraph_by_radius('b', 0)->stringify, "b");
is($g1->subgraph_by_radius('b', 1)->stringify, "a=b,b=d,b=e");
is($g1->subgraph_by_radius('b', 2)->stringify, "a=b,a=c,b=d,b=e");
is($g1->subgraph_by_radius('b', 3)->stringify, "a=b,a=c,b=d,b=e,c=f,c=g");

done_testing();
