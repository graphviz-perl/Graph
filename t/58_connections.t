use strict; use warnings;
use Test::More tests => 24;

use Graph;
my $g0 = Graph->new;
my $g1 = Graph->new(undirected => 1);

my @E = ([1=>1], [1=>2], [1=>3], [2=>4], [5=>4]);
$g0->add_edge(@$_) for @E;
$g1->add_edge(@$_) for @E;
$g0->add_vertex(6);  $g1->add_vertex(6);  

test_graphs($g0, $g1, {
  sink_vertices => [ [ [], "3 4", "" ] ],
  source_vertices => [ [ [], "5", "" ] ],
  isolated_vertices => [ [ [], "6", "6" ] ],
  interior_vertices => [ [ [], "2", "1 2 3 4 5" ] ],
  exterior_vertices => [ [ [], "3 4 5 6", "6" ] ],
  self_loop_vertices => [ [ [], "1", "1" ] ],
});

sub test_graphs {
  my ($g0, $g1, $methods) = @_;
  for my $m (sort keys %$methods) {
    for my $t ( @{ $methods->{$m} } ) {
      my ($args, $expected0, $expected1) = @$t;
      is( "@{[sort $g0->$m(@$args)]}", $expected0, "directed $m (@$args)" );
      is( "@{[sort $g1->$m(@$args)]}", $expected1, "undirected $m (@$args)" );
    }
  }
}

use Graph::Directed;
use Graph::Undirected;

$g0 = Graph::Directed->new;
$g1 = Graph::Undirected->new;

my @P = ([qw(a b d)], [qw(b e)], [qw(a c f f)], [qw(g h)], [qw(i i)], [qw(k k l)]);
$g0->add_path(@$_) for @P;
$g1->add_path(@$_) for @P;
$_->add_vertex(qw(j)) for $g0, $g1;

test_graphs($g0, $g1, {
  sink_vertices => [ [ [], "d e h l", "" ] ],
  source_vertices => [ [ [], "a g", "" ] ],
  isolated_vertices => [ [ [], "j", "j" ] ],
  interior_vertices => [ [ [], "b c", "a b c d e f g h k l" ] ],
  exterior_vertices => [ [ [], "a d e g h j l", "j" ] ],
  self_loop_vertices => [ [ [], "f i k", "f i k" ] ],
});
