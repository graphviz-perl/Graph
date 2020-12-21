use strict; use warnings;
use Test::More;

use Graph;
my $g = Graph->new(multiedged => 1);

$g->add_edge_by_id("a", "b", "hot");

ok( !$g->has_edge_attributes_by_id("a", "b", "hot") );
ok( !$g->has_edge_attributes_by_id("a", "b", "hot") );

ok( $g->set_edge_attribute_by_id("a", "b", "hot", "color", "red") );

ok( $g->has_edge_attribute_by_id("a", "b", "hot", "color") );
ok( $g->has_edge_attribute_by_id("a", "b", "hot", "color") );

ok( $g->has_edge_attributes_by_id("a", "b", "hot") );
ok( $g->has_edge_attributes_by_id("a", "b", "hot") );

is( $g->get_edge_attribute_by_id("a", "b", "hot", "color"),  "red" );
is( $g->get_edge_attribute_by_id("a", "b", "hot", "color"),  "red" );

is( $g->get_edge_attribute_by_id("a", "b", "hot", "colour"), undef );
is( $g->get_edge_attribute_by_id("a", "b", "hot", "colour"), undef );

ok( $g->set_edge_attribute_by_id("a", "b", "hot", "color", "green") );

ok( $g->has_edge_attributes_by_id("a", "b", "hot") );
ok( $g->has_edge_attributes_by_id("a", "b", "hot") );

is( $g->get_edge_attribute_by_id("a", "b", "hot", "color"),  "green" );
is( $g->get_edge_attribute_by_id("a", "b", "hot", "color"),  "green" );

my $attr = $g->get_edge_attributes_by_id("a", "b", "hot");
my @name = $g->get_edge_attribute_names_by_id("a", "b", "hot");
my @val  = $g->get_edge_attribute_values_by_id("a", "b", "hot");

is_deeply $attr, { color => "green" };
is_deeply \@name, [ "color" ];
is_deeply \@val, [ "green" ];

ok( $g->set_edge_attribute_by_id("a", "b", "hot", "taste", "rhubarb") );

ok( $g->has_edge_attributes_by_id("a", "b", "hot") );
ok( $g->has_edge_attributes_by_id("a", "b", "hot") );

is( $g->get_edge_attribute_by_id("a", "b", "hot", "taste"),  "rhubarb" );
is( $g->get_edge_attribute_by_id("a", "b", "hot", "taste"),  "rhubarb" );

is( $g->get_edge_attribute_by_id("a", "b", "hot", "color"),  "green" );
is( $g->get_edge_attribute_by_id("a", "b", "hot", "taste"),  "rhubarb" );

$attr = $g->get_edge_attributes_by_id("a", "b", "hot");
@name = sort $g->get_edge_attribute_names_by_id("a", "b", "hot");
@val  = sort $g->get_edge_attribute_values_by_id("a", "b", "hot");

is_deeply $attr, { color => "green", taste => "rhubarb" };
is_deeply \@name, [ "color", "taste" ];
is_deeply \@val, [ "green", "rhubarb" ];
is_deeply(($g->as_hashes)[1], { a => { b => { hot => { color => "green", taste => "rhubarb" } } } });

ok( $g->delete_edge_attribute_by_id("a", "b", "hot", "color" ) );

ok( !$g->has_edge_attribute_by_id("a", "b", "hot", "color" ) );
ok(  $g->has_edge_attributes_by_id("a", "b", "hot") );
is(  $g->get_edge_attribute_by_id("a", "b", "hot", "taste"),  "rhubarb" );

ok(  $g->delete_edge_attributes_by_id("a", "b", "hot") );
ok( !$g->has_edge_attributes_by_id("a", "b", "hot") );
is(  $g->get_edge_attribute_by_id("a", "b", "hot", "taste"),  undef );

ok( !$g->delete_edge_attribute_by_id("a", "b", "hot", "taste" ) );
ok( $g->delete_edge_attributes_by_id("a", "b", "hot") );

$attr = $g->get_edge_attributes_by_id("a", "b", "hot");
@name = $g->get_edge_attribute_names_by_id("a", "b", "hot");
@val  = $g->get_edge_attribute_values_by_id("a", "b", "hot");

is_deeply $attr, undef;
is_deeply \@name, [];
is_deeply \@val, [];

is( $g->edges, 1 ); # Deleting attributes does not delete edge

$g->add_weighted_edge_by_id("c", "d", "hot", 42);

ok( $g->has_edge_by_id("c", "d", "hot") );
is( $g->get_edge_attribute_by_id("c", "d", "hot", "weight"),  42 );

is( $g->edges, 2 );

$g->add_weighted_edges_by_id("c", "d", 43, "e", "f", 44, "hot");
is( $g->get_edge_weight_by_id("c", "d", "hot"),  43 );
is( $g->get_edge_weight_by_id("e", "f", "hot"),  44 );

is( $g->edges, 3 );

$g->add_weighted_path_by_id("c", 45, "d", 46, "e", "hot");
is( $g->get_edge_weight_by_id("c", "d", "hot"),  45 );
is( $g->get_edge_weight_by_id("d", "e", "hot"),  46 );

is( $g->edges, 4 );

use Graph::Undirected;
my $u = Graph::Undirected->new(multiedged => 1);

$u->add_weighted_edge_by_id('a', 'b', 'hot', 123);

is($u->get_edge_weight_by_id('a', 'b', 'hot'), 123);
is($u->get_edge_weight_by_id('b', 'a', 'hot'), 123);

ok($u->set_edge_attributes_by_id('a', 'b', 'hot',
		           { 'color' => 'pearl', 'weight' => 'heavy' }));
$attr = $u->get_edge_attributes_by_id('a', 'b', 'hot');
is_deeply $attr, { color => "pearl", weight => 'heavy' };

ok( $g->set_edge_weight_by_id("a", "b", "hot", 42));
is( $g->get_edge_weight_by_id("a", "b", "hot"), 42);
ok( $g->has_edge_weight_by_id("a", "b", "hot"));
ok(!$g->has_edge_weight_by_id("a", "c", "hot"));
ok( $g->delete_edge_weight_by_id("a", "b", "hot"));
ok(!$g->has_edge_weight_by_id("a", "b", "hot"));
is( $g->get_edge_weight_by_id("a", "b", "hot"), undef);

my $h = Graph->new(multiedged => 1);

eval { $h->set_edge_attribute("foo", "bar", "color", "gold") };
like($@, qr/expected non-multiedged/);

$h->ingest($g);
my $got = ($h->as_hashes)[1];
is_deeply $got, {
    a => { b => { hot => {} } },
    c => { d => { hot => { weight => 45 } } },
    d => { e => { hot => { weight => 46 } } },
    e => { f => { hot => { weight => 44 } } }
} or diag explain $got;

$g = Graph->new(hyperedged => 1, multiedged => 1, directed => 0);
$g->set_edge_attributes_by_id('a', 'b', 'x', 'hot',
		           { color => 'pearl', weight => 'heavy' });
$g->add_weighted_edge_by_id('a', 'b', 'hot', 123);
$g->add_weighted_path_by_id("c", 45, "d", 46, "e", "hot");
$got = ($g->as_hashes)[1];
is_deeply $got, [
    {
	vertices => ['a', 'b', 'x'],
	attributes => { hot => { color => 'pearl', weight => 'heavy' } },
    },
    { vertices => ['a', 'b'], attributes => { hot => { weight => 123 } } },
    { vertices => ['c', 'd'], attributes => { hot => { weight => 45 } } },
    { vertices => ['d', 'e'], attributes => { hot => { weight => 46 } } },
], 'undirected hyperedge as_hashes' or diag explain $got;

$g = Graph->new(hyperedged => 1, multiedged => 1);
$g->set_edge_attributes_by_id([qw(a b c)], [qw(f g)], 'hot',
		           { color => 'pearl', weight => 'heavy' });
$g->add_weighted_edge_by_id([qw(a b c)], [qw(f h)], 'hot', 123);
$g->add_weighted_path_by_id(["c"], 45, ["d"], 46, ["e"], "hot");
$got = ($g->as_hashes)[1];
is_deeply $got, [
    {
        predecessors => [qw(a b c)],
        successors => [qw(f g)],
	attributes => { hot => { color => 'pearl', weight => 'heavy' } },
    },
    { predecessors => [qw(a b c)], successors => [qw(f h)], attributes => { hot => { weight => 123 } } },
    { predecessors => ['c'], successors => ['d'], attributes => { hot => { weight => 45 } } },
    { predecessors => ['d'], successors => ['e'], attributes => { hot => { weight => 46 } } },
], 'directed hyperedge as_hashes' or diag explain $got;

done_testing;
