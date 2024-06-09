use strict; use warnings;
use Test::More;

use Graph::Undirected;

my $g0 = Graph::Undirected->new;
$g0->add_path('A'..'Z');
ok($g0->is_bipartite);

my $g1 = Graph::Undirected->new;
$g1->add_cycle('A'..'C');
ok(!$g1->is_bipartite);

my $g2 = Graph::Undirected->new;
$g2->add_cycle('A'..'D');
ok($g2->is_bipartite);

my $g3 = Graph::Undirected->new;
for my $A ('A'..'Z') {
    for my $B ('1'..'9') {
        $g3->add_edge($A, $B);
    }
}
ok($g3->is_bipartite);

my $g4 = Graph::Undirected->new;
$g4->add_cycle('A'..'C');
$g4->add_cycle('1'..'6');
ok(!$g4->is_bipartite);

my $g5 = Graph::Undirected->new;
$g5->add_cycle('A'..'D');
$g5->add_cycle('1'..'6');
ok($g5->is_bipartite);

done_testing;
