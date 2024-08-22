use strict; use warnings;
use Test::More;

use Graph;
use Graph::Directed;
use Graph::Undirected;

my $g = Graph::Directed->new;
$g->add_weighted_path("b", 1, "f", 2, "c", 3, "d", 3,
		      "f", 2, "g", 2, "e");
$g->add_weighted_edges("d", "e", 3,
		       "g", "a", 3,
		       "g", "f", 2,
		       "b", "a", 3,
		       "h", "b", 1,
		       "h", "c", 1);

my $sgb_d = $g->SPT_Dijkstra(first_root => "b");
is $sgb_d->get_graph_attribute('SPT_Dijkstra_root'), "b";
is( $sgb_d, "b-a,b-f,c-d,f-c,f-g,g-e" );

my $sgb_bf = $g->SPT_Bellman_Ford(first_root => "b");
is $sgb_bf->get_graph_attribute('SPT_Bellman_Ford_root'), "b";
is( $sgb_bf, "b-a,b-f,c-d,f-c,f-g,g-e" );

my $sgh_d = $g->SPT_Dijkstra(first_root => sub { "h" });
is $sgh_d->get_graph_attribute('SPT_Dijkstra_root'), "h";
is( $sgh_d, "b-a,b-f,c-d,f-g,g-e,h-b,h-c" );

my $sga_d = $g->SPT_Dijkstra(start => "a");
is $sga_d->get_graph_attribute('SPT_Dijkstra_root'), "a";
is( $sga_d, '' );

my $u = Graph::Undirected->new;
$u->add_weighted_path("b", 1, "f",
		           2, "c",
		           3, "d",
		           3, "f",
		           2, "g",
		           2, "e");
$u->add_weighted_edges("d", "e", 3,
		       "g", "a", 3,
		       "g", "f", 2,
		       "b", "a", 3,
		       "h", "b", 1,
		       "h", "c", 1);

my $sub = $u->SPT_Dijkstra(first_root => "b");
is $sub->get_graph_attribute('SPT_Dijkstra_root'), "b";
is( $sub, "a=b,b=f,b=h,c=h,d=f,e=g,f=g" );

my $suh = $u->SPT_Dijkstra(first_root => "h");
is $suh->get_graph_attribute('SPT_Dijkstra_root'), "h";
is( $suh, "a=b,b=f,b=h,c=d,c=h,e=g,f=g" );

my $sua = $u->SPT_Dijkstra(first_root => "a");
is $sua->get_graph_attribute('SPT_Dijkstra_root'), "a";
ok( $sua eq "a=b,a=g,b=f,b=h,c=h,d=e,e=g" ||
    $sua eq "a=b,a=g,b=f,b=h,c=h,d=f,e=g" ||
    $sua eq "a=b,a=g,c=f,c=h,d=e,e=g,f=g" ||
    $sua eq "a=b,a=g,c=f,c=h,d=f,e=g,f=g" );

# Sedgewick, Algorithms in C, Third Edition
# Chapter 21, "Shortest Paths", Figure 21.10 (p 282)
my $g2 = Graph::Directed->new;
$g2->add_weighted_edge(@$_) for [0,1,0.41], [1,2,0.51], [2,3,0.50], [4,3,0.36],
  [3,5,0.38], [3,0,0.45], [0,5,0.29], [5,4,0.21], [1,4,0.32], [4,2,0.32],
  [5,1,0.29];

my @s2_tests = (
  [0,0,"0"], [0,1,"0 1"], [0,2,"0 5 4 2"], [0,3,"0 5 4 3"],
    [0,4,"0 5 4"], [0,5,"0 5"],
  [1,0,"1 4 3 0"], [1,1,"1"], [1,2,"1 2"], [1,3,"1 4 3"],
    [1,4,"1 4"], [1,5,"1 4 3 5"]
);
is("@{[$g2->SP_Dijkstra(@$_[0,1])]}", $_->[2], "path @$_[0,1]")
  for @s2_tests;

my $s2_di = $g2->SPT_Dijkstra(first_root => "0");
is( $s2_di, "0-1,0-5,4-2,4-3,5-4" );
is($s2_di->get_edge_attribute(@$_[0,1], 'weight'), $_->[2], "edge @$_[0,1]")
  for [0,1,0.41], [0,5,0.29], [5,4,0.50], [4,3,0.86], [4,2,0.82],
    [0,3,undef], [3,5,undef], [5,1,undef], [1,2,undef], [2,3,undef],
    [1,0,undef], [5,0,undef], [4,5,undef], [3,4,undef], [2,4,undef],
    [3,0,undef], [5,3,undef], [1,5,undef], [2,1,undef], [3,2,undef];
is($s2_di->get_vertex_attribute(@$_[0,1]), $_->[2], "vertex @$_[0,1]")
  for [0,'weight',undef], [1,'weight',0.41], [2,'weight',0.82],
      [3,'weight',0.86], [4,'weight',0.50], [5,'weight',0.29],
    [0,'p',undef], [1,'p',0], [2,'p',4], [3,'p',4], [4,'p',5], [5,'p',0];

is +($s2_di->get_edge_attribute_all(0, 1, 'weight'))[0], 0.41, "get_edge_attribute_all";

my $s2_bf = $g2->SPT_Bellman_Ford(first_root => "0");
is( $s2_bf, "0-1,0-5,4-2,4-3,5-4" );
is($s2_bf->get_edge_attribute(@$_[0,1], 'weight'), $_->[2], "edge @$_[0,1]")
  for [0,1,0.41], [0,5,0.29], [5,4,0.21], [4,3,0.36], [4,2,0.32],
  [0,3,undef], [3,5,undef], [5,1,undef], [1,2,undef], [2,3,undef],
  [1,0,undef], [5,0,undef], [4,5,undef], [3,4,undef], [2,4,undef],
  [3,0,undef], [5,3,undef], [1,5,undef], [2,1,undef], [3,2,undef];
is($s2_bf->get_vertex_attribute(@$_[0,1]), $_->[2], "vertex @$_[0,1]")
  for [0,'weight',undef], [1,'weight',0.41], [2,'weight',0.82],
    [3,'weight',0.86], [4,'weight',0.50], [5,'weight',0.29],
  [0,'p',undef], [1,'p',0], [2,'p',4], [3,'p',4], [4,'p',5], [5,'p',0];
is("@{[$g2->SP_Bellman_Ford(@$_[0,1])]}", $_->[2], "path @$_[0,1]")
  for @s2_tests;

my $g3 = Graph::Directed->new;
$g3->add_weighted_path(qw(a 1 b 2 c 3 d -1 e 4 f));

my $s3_da = eval { $g3->SPT_Dijkstra(first_root => "a") };
like($@, qr/Graph::SPT_Dijkstra: edge d-e is negative \(-1\)/);
is( $s3_da, undef );

my $s3_bf = eval { $g3->SPT_Bellman_Ford(first_root => "a") };
is($@, '');
is( $s3_bf, "a-b,b-c,c-d,d-e,e-f");

$g3->add_weighted_path(qw(b -2 a));
$s3_bf = eval { $g3->SPT_Bellman_Ford(first_root => "a") };
like($@, qr/Graph::SPT_Bellman_Ford: negative cycle exists/);
is( $s3_bf, undef );

# http://rt.cpan.org/NoAuth/Bug.html?id=516
my $g4 = new Graph::Undirected;
$g4->add_weighted_edge("r1", "l1", 1);
my $d4 = $g4->SSSP_Dijkstra("r1");
is($g4, "l1=r1");
is($d4, "l1=r1");

# Nathan Goodman
my $g5 = Graph::Directed->new;
$g5->add_path(qw(0 1 2));
my $sg5 = $g5->SPT_Dijkstra(first_root => "0");
is($sg5, "0-1,1-2");

{
    my $g = Graph::Directed->new(refvertexed => 1);
    $g->add_edge(qw(a b));
    $g->add_edge(qw(a c));
    $g->add_edge(qw(c d));
    $g->add_edge(qw(c e));
    $g->add_edge(qw(e f));
    my $r = [1, 2, 3];
    $g->add_edge('f', $r);

    my $s0 = $g->SPT_Dijkstra(first_root => 'a');
    ok($s0->has_vertex('f'));
    my @e0 = $s0->successors('f');
    is(@e0, 1);
    is_deeply($e0[0], $r);

    my $s1 = $g->SPT_Bellman_Ford(first_root => 'a');
    ok($s1->has_vertex('f'));
    my @e1 = $s1->successors('f');
    is(@e1, 1);
    is($e1[0], $r);
}

done_testing;
