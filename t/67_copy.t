use strict; use warnings;
use Test::More;

use Graph::Directed;
use Graph::Undirected;

my ($g0, $g2, $g4) = map Graph::Directed->new, 1..3;
my ($g1, $g3, $g5) = map Graph::Undirected->new, 1..3;

$_->add_path(qw(a b c)) for $g0, $g1;
$_->add_path(qw(d b e)) for $g0, $g1;

$_->add_path(qw(a b c d)) for $g2, $g3;
$_->add_path(qw(c a)) for $g2, $g3;

$_->add_path(qw(a b c)) for $g4, $g5;
$_->add_path(qw(b a)) for $g4, $g5;

is $g0->copy, "a-b,b-c,b-e,d-b";
is $g1->copy, "a=b,b=c,b=d,b=e";
is $g2->copy, "a-b,b-c,c-a,c-d";
is $g3->copy, "a=b,a=c,b=c,c=d";
is $g4->copy, "a-b,b-a,b-c";
is $g5->copy, "a=b,b=c";

is $g0->undirected_copy, $g1;
is $g2->undirected_copy, $g3;
is $g4->undirected_copy, $g5;

is $g1->directed_copy, "a-b,b-a,b-c,b-d,b-e,c-b,d-b,e-b";
is $g3->directed_copy, "a-b,a-c,b-a,b-c,c-a,c-b,c-d,d-c";
is $g5->directed_copy, "a-b,b-a,b-c,c-b";

is $g0->transpose, "b-a,b-d,c-b,e-b";
is $g1->transpose, "a=b,b=c,b=d,b=e";
is $g2->transpose, "a-c,b-a,c-b,d-c";
is $g3->transpose, "a=b,a=c,b=c,c=d";
is $g4->transpose, "a-b,b-a,c-b";
is $g5->transpose, "a=b,b=c";

my $g6 = Graph->new;
is($g6->complete->edges, 0);
is($g6->complement->edges, 0);

my $g7 = Graph::Directed->new();
$g7->add_edge(qw(a b));
$g7->add_edge(qw(a c));
is($g7, "a-b,a-c");
is($g7->complete, "a-b,a-c,b-a,b-c,c-a,c-b");
is($g7->complement, "b-a,b-c,c-a,c-b");

my $g8 = Graph::Undirected->new();
$g8->add_edge(qw(a b));
$g8->add_edge(qw(a c));
is($g8, "a=b,a=c");
is($g8->complete, "a=b,a=c,b=c");
is($g8->complement, "b=c,a");

my $g9 = Graph::Directed->new(countedged => 1);
$g9->add_edge(qw(a b));
$g9->add_edge(qw(a c));
my $c9 = $g9->complete_graph;
is $c9, "a-b,a-c,b-a,b-c,c-a,c-b";
for my $u (qw(a b c)) {
    for my $v (qw(a b c)) {
	next if $u eq $v;
	is($c9->get_edge_count($u, $v), 1);
    }
}
is $g9->complement_graph, "b-a,b-c,c-a,c-b";

my $g10 = Graph::Undirected->new();
$g10->add_edge(qw(a b));
is scalar($g10->vertices), 2;
my $c10 = $g10->complement_graph;
is scalar($c10->vertices), 2;
is scalar($c10->edges), 0;

{
    my $g = Graph->new;
    $g->set_graph_attribute('color' => 'deep_purple');
    $g->set_graph_attribute('hunky' => sub { "hunky $_[0]" });
 SKIP: {
	skip("no coderef Deparse", 2) unless $] >= 5.008;
	my $c = $g->deep_copy;
	is($c->get_graph_attribute('color'), 'deep_purple');
	is($c->get_graph_attribute('hunky')->('dory'), 'hunky dory');
    }
}

SKIP: {
    skip("no coderef Deparse", 1) unless $] >= 5.008;
    my $g = Graph->new;
    $g->set_graph_attribute('color' => sub { $_[0] ** 2 });
    my $c = $g->deep_copy;
    is($c->get_graph_attribute('color')->(7), 49);
}

SKIP: {
    skip("no coderef Deparse", 1) unless $] >= 5.008;
    skip("no coderef Deparse with Storable", 1)
	unless Graph::_can_deep_copy_Storable();
    require Storable;
    my $g = Graph->new;
    $g->set_graph_attribute('color' => sub { $_[0] ** 3 });
    my $c = $g->_deep_copy_Storable;
    is($c->get_graph_attribute('color')->(2), 8);
}

SKIP: {
    skip("no coderef Deparse", 1) unless $] >= 5.008;
    my $g = Graph->new;
    $g->set_graph_attribute('color' => sub { $_[0] ** 4 });
    my $c = $g->_deep_copy_DataDumper;
    is($c->get_graph_attribute('color')->(3), 81);
}

my $edges = [[{ name => 'A' }, { name => 'B' }]];
SKIP: {
    my $orig = Graph::Undirected->new(refvertexed=>1, edges=>$edges);
    for my $g ($orig, $orig->deep_copy) {
        is scalar $g->neighbours( $_ ), 1, 'still linked up' for $g->vertices;
    }
}

done_testing;
