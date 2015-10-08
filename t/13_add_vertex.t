use strict; use warnings;
use Test::More tests => 7;

use Graph;
my $g = Graph->new;

ok( $g->add_vertex("a") );
ok( $g->add_vertex("b") );

is( $g->add_vertex("c"), $g );

eval '$g->add_vertex("d", "e")';
like($@,
     qr/Graph::add_vertex: expected hypervertexed graph/);

eval '$g->add_vertex(undef)';
like($@,
     qr/Graph::add_vertex: undef vertex/);

is( $g->add_vertices("x", "y"), $g );

is( $g, "a,b,c,x,y" );
