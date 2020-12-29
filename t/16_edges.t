use strict; use warnings;
use Test::More tests => 48;

use Graph;
my $g = Graph->new;

is $g, "";
ok( !$g->has_edges() );
is( $g->edges, 0 );
is( "@{[$g->edges]}", "" );
ok( !$g->any_edge("a", "b") );
ok( !$g->any_edge("b", "a") );
ok( !$g->any_edge("b", "c") );
ok( !$g->any_edge("c", "b") );

$g->add_edge("a", "b");
is $g, "a-b";
ok( $g->has_edges() );
is( $g->edges, 1 );
is( "@{[map { qq{[@$_]} } $g->edges]}", "[a b]" );
ok( $g->any_edge("a", "b") );
ok( !$g->any_edge("b", "a") );
ok( !$g->any_edge("b", "c") );
ok( !$g->any_edge("c", "b") );

$g->add_edge("b", "c");
is $g, "a-b,b-c";
ok( $g->has_edges() );
is( $g->edges, 2 );
is( "@{[sort map { qq{[@$_]} } $g->edges]}", "[a b] [b c]" );
ok( $g->any_edge("a", "b") );
ok( !$g->any_edge("b", "a") );
ok( $g->any_edge("b", "c") );
ok( !$g->any_edge("c", "b") );

eval { $g->add_edges("x") };
like($@, qr/Graph::add_edges: missing end vertex/);

is($g->add_edges("x", "y"), $g);
is($g, "a-b,b-c,x-y");

eval { $g->add_edge("c", "d", "e", "f") };
like($@,
     qr/Graph::add_edge: expected hyperedged graph/);

eval { $g->add_edge("c") };
like($@,
     qr/Graph::add_edge: expected hyperedged graph/);

ok(   $g->delete_edge("b", "c") );
is $g, "a-b,x-y,c";

ok(   $g->delete_edge("b", "d") );
is $g, "a-b,x-y,c";

is( $g->delete_edge("a", "b"), 'x-y,a,b,c' );
is( $g->delete_edge("a", "b"), 'x-y,a,b,c' );

$g->add_edges(qw(a b b x c d c y));
is $g, "a-b,b-x,c-d,c-y,x-y";

$g->delete_edges(qw(a b c d));
is $g, "b-x,c-y,x-y,a,d";

$g->delete_edges(qw(a b c d));
is $g, "b-x,c-y,x-y,a,d";

$g->delete_edges(qw(b x c y));
is $g, "x-y,a,b,c,d";

is( $g->delete_edge(), $g );
is( $g->delete_edges(), $g );

my $h = Graph->new(countedged => 1);

$h->add_edges(qw(a x a x b y b y));
is $h, "a-x,b-y";

$h->delete_edge('a', 'x');
is $h, "a-x,b-y";
$h->delete_edge('a', 'x');
is $h, "b-y,a,x";

$h->delete_edges('b', 'y');
ok(   $h->has_edges ); # takes no args
ok(   $h->has_edges("b", "y") );
$h->delete_edges('b', 'y');
ok( ! $h->has_edges );
ok( ! $h->has_edges("b", "y") );
