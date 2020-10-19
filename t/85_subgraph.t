use Graph;

use strict;
use warnings;

use Test::More;

my $g = Graph->new;

$g->add_edge(qw(a b));
$g->add_edge(qw(a c));
$g->add_edge(qw(b d));
$g->add_edge(qw(b e));
$g->add_edge(qw(c f));

my $s1 = $g->subgraph([qw(a b c)], [qw(d e f)]);
my $s2 = $g->subgraph([qw(a b c)]);
my $s3 = $g->subgraph(['a'],['e']);

is($s1, "b-d,b-e,c-f,a");
is($s2, "a-b,a-c");
is($s3, "a,e");

done_testing();

