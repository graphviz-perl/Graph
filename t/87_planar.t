use strict;
use warnings;

use Graph::Undirected;

use Test::More;

my $g0 = Graph::Undirected->new;
$g0->add_path('A'..'Z');
ok($g0->is_planar);

my $g1 = Graph::Undirected->new;
$g1->add_cycle('A'..'C');
ok($g1->is_planar);

my $g2 = Graph::Undirected->new;
$g2->add_cycle('A'..'D');
ok($g2->is_planar);

my $g3 = Graph::Undirected->new;
for my $A ('A'..'Z') {
    for my $B ('1'..'9') {
        $g3->add_edge($A, $B);
    }
}
ok(!$g3->is_planar);

my $g4 = Graph::Undirected->new;
$g4->add_cycle('A'..'C');
$g4->add_cycle('1'..'6');
ok($g4->is_planar);

my $g5 = Graph::Undirected->new;
$g5->add_cycle('A'..'D');
$g5->add_cycle('1'..'6');
ok($g5->is_planar);

my $g6 = Graph::Undirected->new; # K5
for my $A ('A'..'E') {
    for my $B ('A'..'E') {
        next if $A eq $B;
        $g6->add_edge( $A, $B );
    }
}
ok(!$g6->is_planar);

my $g7 = Graph::Undirected->new; # K3,3
for my $A ('A'..'C') {
    for my $B ('1'..'3') {
        $g7->add_edge( $A, $B );
    }
}
ok(!$g7->is_planar);

done_testing;
