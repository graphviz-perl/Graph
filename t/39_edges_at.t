use strict; use warnings;
use Test::More tests => 14;

use Graph;
my $g = Graph->new(hyperedged => 1, directed => 0);

$g->add_edge("a", "b");
$g->add_edge("d" ,"e");
$g->add_edge("a", "b", "c");

sub deref {
    my $r = shift;
    ref $r ? "[" . join(" ", sort map { deref($_) } @$r) . "]" : $_;
}

sub at {
    join(" ", sort map { deref($_) } $g->edges_at(@_));
}

is( at("a"), "[a b c] [a b]");
is( at("b"), "[a b c] [a b]");
is( at("c"), "[a b c]");
is( at("d"), "[d e]");
is( at("e"), "[d e]");
is( at("x"), "");

{
    # [cpan #11543] self-edges reported twice in edges_at
    use Graph::Directed;
    my $g1 = new Graph::Directed();
    $g1->add_edge(0,0);
    is(scalar $g1->edges_at(0), 1);
    is("@{ ($g1->edges_at(0))[0] }", "0 0");
}

{
    my $g2 = new Graph::Directed();
    $g2->add_edge(1,1);
    $g2->add_edge(1,2);
    my @e1 = $g2->edges_at(1);
    is(@e1, 2);
    @e1[1, 0] = @e1[0, 1] if $e1[0]->[1] > $e1[1]->[1];
    is("@{ $e1[0] }", "1 1");
    is("@{ $e1[1] }", "1 2");
    my @e2 = $g2->edges_at(2);
    is(@e2, 1);
    is("@{ $e2[0] }", "1 2");
    my @e3 = $g2->edges_at(3);
    is(@e3, 0);
}
