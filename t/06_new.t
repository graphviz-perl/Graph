use strict; use warnings;
use Test::More tests => 49;

use Graph;

test_prop(@$_) for (
    # 2nd is whether default is true, then aliases, then opposites
    [refvertexed => 0, [], []],
    [countvertexed => 0, [], []],
    [multivertexed => 0, [], []],
    [undirected => 0, [], [qw(directed)]],
    [directed => 1, [], [qw(undirected)]],
    [countedged => 0, [], []],
    [multiedged => 0, [], []],
    [hyperedged => 0, [], []],
);

sub test_prop {
    my ($prop, $true_by_default, $aliases, $opposites) = @_;
    my $g = Graph->new;
    my $got = $g->$prop;
    $got = !$got if !$true_by_default;
    ok $got, "$prop correct default value";
    $g = Graph->new( $prop => 0 );
    ok !$g->$prop, "$prop reflects given false value";
    ok $g->$_, "$prop opposite=$_ right" for @$opposites;
    $g = Graph->new( $prop => 1 );
    ok $g->$prop, "$prop reflects given true value";
    ok $g->$_, "$prop alias=$_ right" for @$aliases;
    ok !$g->$_, "$prop opposite=$_ right" for @$opposites;
    $g = $g->copy;
    ok $g->$prop, "$prop survives copy";
}

{
    local $SIG{__DIE__} = sub { $@ = shift };

    eval { my $gna = Graph->new(foobar => 1) };
    like($@, qr/Graph::new: Unknown option: 'foobar' /);

    eval { my $gna = Graph->new(foobar => 0) };
    like($@, qr/Graph::new: Unknown option: 'foobar' /);

    eval { my $gna = Graph->new(foobar => 1, barfoo => 1) };
    like($@, qr/Graph::new: Unknown options: 'barfoo' 'foobar' /);
}

{
    my $g = Graph->new(vertices => [0, 1, 2]);
    ok($g->has_vertex(0));
    ok($g->has_vertex(1));
    ok($g->has_vertex(2));
}

{
    my $g = Graph->new(edges => [[0, 1], [2, 3]]);
    is $g, "0-1,2-3";
}

{
    my $g = Graph->new(vertices => [0], edges => [[1, 2], [2, 3]]);
    ok($g->has_vertex(0));
    is $g, "1-2,2-3,0";
}

{
    my $g = Graph->new(multiedged => 1);
    my $h = $g->new; # The flags should be inherited.
    ok($h->is_multiedged);
    $h = $g->new(multiedged => 0); # The flags should be overridable
    ok !$h->is_multiedged;
}

use Graph::Directed;
my $d = Graph::Directed->new;
is(ref $d, 'Graph::Directed');

use Graph::Undirected;
my $u = Graph::Undirected->new;
is(ref $u, 'Graph::Undirected');
