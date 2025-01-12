use strict;
use warnings;

use Graph::Undirected;

use Test::More;

my $g0 = Graph::Undirected->new;
$g0->add_vertices('A', 'B');

is_deeply [map [sort @$_], @{ $g0->max_cliques }], [['A'], ['B']], 'no edges';

my $g1 = Graph::Undirected->new;
$g1->add_edges(map split(/-/), qw( a-b b-c a-c x-y y-z u-v ));
$g1->add_vertex('w');
my $mc = $g1->max_cliques;  # Scalar context.
is_deeply [sort { $a->[0] cmp $b->[0] } map [sort @$_], @$mc],
    [['a', 'b', 'c'], ['u', 'v'], ['w'], ['x', 'y'], ['y', 'z']],
    'cliques of sizes 1..3';

my $g2 = Graph::Undirected->new;
$g2->add_edges(map split(/-/), qw( kh-tc qp-kh de-cg ka-co yn-aq qp-ub
                                   cg-tb vc-aq tb-ka wh-tc yn-cg kh-ub
                                   ta-co de-co tc-td tb-wq wh-td ta-ka
                                   td-qp aq-cg wq-ub ub-vc de-ta wq-aq
                                   wq-vc wh-yn ka-de kh-ta co-tc wh-qp
                                   tb-vc td-yn ));
my @mc = $g2->max_cliques;  # List context.
is_deeply [sort { "@$a" cmp "@$b" } map [sort @$_], @mc],
    [
        [qw[ aq cg yn ]],
        [qw[ aq vc wq ]],
        [qw[ cg de ]],
        [qw[ cg tb ]],
        [qw[ co de ka ta ]],
        [qw[ co tc ]],
        [qw[ ka tb ]],
        [qw[ kh qp ub ]],
        [qw[ kh ta ]],
        [qw[ kh tc ]],
        [qw[ qp td wh ]],
        [qw[ tb vc wq ]],
        [qw[ tc td wh ]],
        [qw[ td wh yn ]],
        [qw[ ub vc wq ]]
    ],
    'Advent of Code 2024 Day 23 Part 2';

done_testing;
