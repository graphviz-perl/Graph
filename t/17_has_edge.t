use strict; use warnings;
use Test::More tests => 4;

use Graph;
my $g = Graph->new;

ok( !$g->any_edge("a", "b") );

$g->add_edge(0, 1);
ok( $g->any_edge(0, 1));
ok(!$g->any_edge(1, 1));
ok(!$g->any_edge(1, 0));
