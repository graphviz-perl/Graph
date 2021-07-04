use strict; use warnings;
use Test::More;

use Graph;
my $g0 = Graph->new;
my $g1 = Graph->new(undirected => 1);

my @E = ([1=>1], [1=>2], [1=>3], [2=>4], [5=>4]);
$g0->add_edge(@$_) for @E;
$g1->add_edge(@$_) for @E;
$g0->add_vertex(6);  $g1->add_vertex(6);  

is $g0, "1-1,1-2,1-3,2-4,5-4,6";
is $g1, "1=1,1=2,1=3,2=4,4=5,6";

my %methods = (
  neighbours => [
    [ [1], "1 2 3", "1 2 3" ],
    [ [2], "1 4", "1 4" ],
    [ [3], "1", "1" ],
    [ [4], "2 5", "2 5" ],
    [ [5], "4", "4" ],
    [ [6], "", "" ],
  ],
  neighbours_by_radius => [
    [ [1, 1], "1 2 3", "1 2 3" ],
    [ [2, 1], "1 4", "1 4" ],
    [ [3, 1], "1", "1" ],
    [ [4, 1], "2 5", "2 5" ],
    [ [5, 2], "2 4", "2 4" ],
    [ [6, 1], "", "" ],
  ],
  is_successorless_vertex => [
    [ [1], "", "" ],
    [ [2], "", "" ],
    [ [3], 1, "" ],
    [ [4], 1, "" ],
    [ [5], "", "" ],
    [ [6], 1, 1 ],
  ],
  is_successorful_vertex => [
    [ [1], 1, 1 ],
    [ [2], 1, 1 ],
    [ [3], "", 1 ],
    [ [4], "", 1 ],
    [ [5], 1, 1 ],
    [ [6], "", "" ],
  ],
  is_predecessorless_vertex => [
    [ [1], "", "" ],
    [ [2], "", "" ],
    [ [3], "", "" ],
    [ [4], "", "" ],
    [ [5], 1, "" ],
    [ [6], 1, 1 ],
  ],
  is_predecessorful_vertex => [
    [ [1], 1, 1 ],
    [ [2], 1, 1 ],
    [ [3], 1, 1 ],
    [ [4], 1, 1 ],
    [ [5], "", 1 ],
    [ [6], "", "" ],
  ],
  successorless_vertices => [
    [ [], "3 4 6", "6" ],
  ],
  successorful_vertices => [
    [ [], "1 2 5", "1 2 3 4 5" ],
  ],
  predecessorless_vertices => [
    [ [], "5 6", "6" ],
  ],
  predecessorful_vertices => [
    [ [], "1 2 3 4", "1 2 3 4 5" ],
  ],
);
for my $m (sort keys %methods) {
  for my $t ( @{ $methods{$m} } ) {
    my ($args, $expected0, $expected1) = @$t;
    my $got0_count = scalar $g0->$m(@$args);
    my $expected0_count = scalar split ' ', $expected0;
    is $got0_count+0, $expected0_count, "right number for scalar context $m";
    is( "@{[sort $g0->$m(@$args)]}", $expected0, "directed $m (@$args)" );
    is( "@{[sort $g1->$m(@$args)]}", $expected1, "undirected $m (@$args)" );
  }
}

done_testing;
