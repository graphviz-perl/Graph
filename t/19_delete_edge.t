use strict; use warnings;
use Test::More tests => 21;

use Graph;
my $g = Graph->new;

ok( $g->add_edge("a", "b") );
ok( $g->add_edge("b", "c") );

ok(   $g->delete_edge("b", "c") );
is $g, "a-b,c";

ok(   $g->delete_edge("b", "d") );
is $g, "a-b,c";

is( $g->delete_edge("a", "b"), 'a,b,c' );
is( $g->delete_edge("a", "b"), 'a,b,c' );

$g->add_edges(qw(a b b x c d c y));
is $g, "a-b,b-x,c-d,c-y";

$g->delete_edges(qw(a b c d));
is $g, "b-x,c-y,a,d";

$g->delete_edges(qw(a b c d));
is $g, "b-x,c-y,a,d";

$g->delete_edges(qw(b x c y));
is $g, "a,b,c,d,x,y";

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
