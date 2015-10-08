use strict; use warnings;
use Test::More tests => 6;

use Graph;

my $g1 = Graph->new;

ok ( !$g1->refvertexed );

my $g2 = Graph->new( refvertexed => 1 );

ok (  $g2->refvertexed );

{
  # rt.cpan.org 78465 find_a_cycle and has_cycle are broken
  my $v1 = \1;
  my $v2 = \2;
  my $graph = Graph->new( directed => 1, refvertexed => 1, edges => [[$v1, $v2], [$v2, $v1]] );
  my @v = $graph->vertices();
  ok($graph->has_a_cycle);
  my @c = $graph->find_a_cycle;
  is(@c, 2);
  if ($c[0] == $v1) {
    is_deeply($c[0], $v1);
    is_deeply($c[1], $v2);
  } else {
    is_deeply($c[0], $v2);
    is_deeply($c[1], $v1);
  }
}
