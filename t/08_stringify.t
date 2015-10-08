use strict; use warnings;
use Test::More tests => 30;

use Graph::Undirected;
use Graph::Directed;

my $g0 = Graph::Undirected->new;
my $g1 = Graph::Directed->new;

$g0->add_edge(qw(a b)); $g1->add_edge(qw(a b));
$g0->add_edge(qw(a c)); $g1->add_edge(qw(a c));
$g0->add_edge(qw(a d)); $g1->add_edge(qw(a d));
$g0->add_edge(qw(a e)); $g1->add_edge(qw(a e));
$g0->add_edge(qw(a f)); $g1->add_edge(qw(a f));

$g0->add_edge(qw(b c)); $g1->add_edge(qw(b c));
$g0->add_edge(qw(b d)); $g1->add_edge(qw(b d));
$g0->add_edge(qw(b e)); $g1->add_edge(qw(b e));
$g0->add_edge(qw(b f)); $g1->add_edge(qw(b f));

$g0->add_edge(qw(c b)); $g1->add_edge(qw(c b));
$g0->add_edge(qw(d b)); $g1->add_edge(qw(d b));
$g0->add_edge(qw(e b)); $g1->add_edge(qw(e b));
$g0->add_edge(qw(f b)); $g1->add_edge(qw(f b));

$g0->add_edge(qw(d c)); $g1->add_edge(qw(d c));
$g0->add_edge(qw(e c)); $g1->add_edge(qw(e c));
$g0->add_edge(qw(f c)); $g1->add_edge(qw(f c));

$g0->add_edge(qw(d e)); $g1->add_edge(qw(d e));
$g0->add_edge(qw(d f)); $g1->add_edge(qw(d f));

$g0->add_edge(qw(e f)); $g1->add_edge(qw(e f));

is($g0, 'a=b,a=c,a=d,a=e,a=f,b=c,b=d,b=e,b=f,c=d,c=e,c=f,d=e,d=f,e=f')
    for 1..10;
is($g1, 'a-b,a-c,a-d,a-e,a-f,b-c,b-d,b-e,b-f,c-b,d-b,d-c,d-e,d-f,e-b,e-c,e-f,f-b,f-c')
    for 1..10;

{
  my $null = Graph->new;
  ok($null, "boolify wins over stringify for empty graph");
  ok($g0, "boolify");
}

{
  # Inspired by
  # rt.cpan.org 93278: SPT_Dijkstra sometimes returns a wrong answer
  use Graph::Directed;
  my $null = Graph::Directed->new;
  for ( 1..5 ) {  # Adds _NOTHING_ -- but dies.
    eval { $null->add_vertex };
    like($@, qr/Graph::add_vertex: expected hypervertexed graph/);
  }
  is($null, "");

  my $hyper = Graph::Directed->new(hypervertexed => 1);
  $hyper->add_vertex for 1..5;  # Adds the "empty vertex", five times.
  is($hyper->vertices, 1);

  $hyper->add_vertex($_) for 1..5;
  is($hyper->vertices, 6);
}
