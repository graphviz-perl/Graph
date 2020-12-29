use strict; use warnings;
use Test::More tests => 17;

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
