use strict; use warnings;
use Test::More tests => 50;

use Graph;
my $g = Graph->new(multivertexed => 1);
ok $g->multivertexed;

is( $g->get_vertex_count('a'), 0 );
ok( $g->add_vertex_by_id('a', 'red') );
is( $g->get_vertex_count('a'), 1 );

for (1,2) {
    ok( $g->has_vertex('a') );
    ok(!$g->has_vertex('b') );
    ok( $g->has_vertex_by_id('a', 'red') );
    ok(!$g->has_vertex_by_id('a', 'blue') );
}

$g->add_vertex_by_id('a', 'blue');
is( $g->get_vertex_count('a'), 2 );

ok( $g->has_vertex_by_id('a', $_) ) for qw(blue red);

$g->add_vertex('a');
ok( $g->has_vertex('a') );
ok(!$g->has_vertex('b') );
is( $g->get_vertex_count('a'), 3 );

is( $g->add_vertex_get_id('a'), $_) for 1..3;
ok( $g->has_vertex_by_id('a', $_) ) for 0..3;

is( $g->get_vertex_count('a'), 6 );

ok( $g->delete_vertex_by_id('a', 'blue') );
ok(!$g->has_vertex_by_id('a', 'blue') );
ok( $g->has_vertex_by_id('a', 'red') );

ok( $g->delete_vertex_by_id('a', 'green') );
ok(!$g->has_vertex_by_id('a', $_)) for qw(blue green);

ok( $g->has_vertex_by_id('a', 'red') );
ok( $g->delete_vertex_by_id('a', 'red') );

my $got = [ sort $g->get_multivertex_ids('a') ];
is_deeply $got, [ qw(0 1 2 3) ] or diag explain $got;
is( $g->get_vertex_count('a'), 4 );

ok $g->add_edge('a', 'b');
$got = [ $g->successors('a') ];
is_deeply $got, [ 'b' ] or diag explain $got;
$got = [ $g->predecessors('b') ];
is_deeply $got, [ 'a' ] or diag explain $got;

is( $g->delete_vertex('a'), 'b' );
ok(!$g->has_vertex_by_id('a', $_) ) for 0..3;
is( $g->get_multivertex_ids('a'), undef );

my $h = Graph->new;

eval { $h->add_vertex_by_id("b", "black") };
like($@, qr/add_vertex_by_id: expected multivertexed/);

eval { $h->has_vertex_by_id("b", "black") };
like($@, qr/has_vertex_by_id: expected multivertexed/);

eval { $h->get_multivertex_ids() };
like($@, qr/get_multivertex_ids: expected multivertexed/);

eval { $h->delete_vertex_by_id("b", "black") };
like($@, qr/delete_vertex_by_id: expected multivertexed/);

eval { Graph->new( multivertexed => 1, countvertexed => 1 ) };
like ( $@, qr/both countvertexed and multivertexed/ );
