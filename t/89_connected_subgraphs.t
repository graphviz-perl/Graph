use strict;
use warnings;

use Graph::Undirected;

use Test::More;

my $g;
my @subgraphs;

# Caffeine molecule, heavy atoms only
$g = Graph::Undirected->new;
$g->add_path('A'..'K');
$g->add_edge('C', 'L');
$g->add_edge('E', 'M');
$g->add_edge('I', 'N');
$g->add_edge('B', 'J');
$g->add_edge('D', 'H');

@subgraphs = $g->connected_subgraphs;
is(@subgraphs, 1153);

my @by_size;
for (@subgraphs) {
    $by_size[scalar $_->vertices]++;
}

my @result0 = qw( 0 14 15 23 40 68 112 165 206 208 162 93 37 9 1 );

for (1..$#result0) {
    is($by_size[$_], $result0[$_]);
}

# K5
$g = Graph::Undirected->new;
for my $i (1..5) {
    for my $j ($i+1..5) {
        $g->add_edge($i, $j);
    }
}

@subgraphs = $g->connected_subgraphs;
is(@subgraphs, 31);

# Line of 5 vertices
$g = Graph::Undirected->new;
$g->add_path(1..5);

@subgraphs = $g->connected_subgraphs;
is(@subgraphs, 15);

done_testing;
