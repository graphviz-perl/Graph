use strict; use warnings;
use Test::More tests => 62;

use Graph;
my $g = Graph->new(multivertexed => 1);

$g->add_vertex_by_id("a", "hot");

ok( !$g->has_vertex_attributes_by_id("a", "hot") );
ok( !$g->has_vertex_attributes_by_id("a", "hot") );

ok( $g->set_vertex_attribute_by_id("a", "hot", "color", "red") );

ok( $g->has_vertex_attribute_by_id("a", "hot", "color") );
ok( $g->has_vertex_attribute_by_id("a", "hot", "color") );

ok( $g->has_vertex_attributes_by_id("a", "hot") );
ok( $g->has_vertex_attributes_by_id("a", "hot") );

is( $g->get_vertex_attribute_by_id("a", "hot", "color"),  "red" );
is( $g->get_vertex_attribute_by_id("a", "hot", "color"),  "red" );

is( $g->get_vertex_attribute_by_id("a", "hot", "colour"), undef );
is( $g->get_vertex_attribute_by_id("a", "hot", "colour"), undef );

ok( $g->set_vertex_attribute_by_id("a", "hot", "color", "green") );

ok( $g->has_vertex_attributes_by_id("a", "hot") );
ok( $g->has_vertex_attributes_by_id("a", "hot") );

is( $g->get_vertex_attribute_by_id("a", "hot", "color"),  "green" );
is( $g->get_vertex_attribute_by_id("a", "hot", "color"),  "green" );

my $attr = $g->get_vertex_attributes_by_id("a", "hot");
my @name = $g->get_vertex_attribute_names_by_id("a", "hot");
my @val  = $g->get_vertex_attribute_values_by_id("a", "hot");

is_deeply $attr, { color => "green" };
is_deeply \@name, [ "color" ];
is_deeply \@val, [ "green" ];

ok( $g->set_vertex_attribute_by_id("a", "hot", "taste", "rhubarb") );

ok( $g->has_vertex_attributes_by_id("a", "hot") );
ok( $g->has_vertex_attributes_by_id("a", "hot") );

is( $g->get_vertex_attribute_by_id("a", "hot", "taste"),  "rhubarb" );
is( $g->get_vertex_attribute_by_id("a", "hot", "taste"),  "rhubarb" );

is( $g->get_vertex_attribute_by_id("a", "hot", "color"),  "green" );
is( $g->get_vertex_attribute_by_id("a", "hot", "taste"),  "rhubarb" );

$attr = $g->get_vertex_attributes_by_id("a", "hot");
@name = sort $g->get_vertex_attribute_names_by_id("a", "hot");
@val  = sort $g->get_vertex_attribute_values_by_id("a", "hot");

is_deeply $attr, { color => "green", taste => "rhubarb" };
is_deeply \@name, [ "color", "taste" ];
is_deeply \@val, [ "green", "rhubarb" ];
is_deeply(($g->as_hashes)[0], { a => { hot => { color => "green", taste => "rhubarb" } } });

ok( $g->delete_vertex_attribute_by_id("a", "hot", "color" ) );

ok( !$g->has_vertex_attribute_by_id("a", "hot", "color" ) );
ok(  $g->has_vertex_attributes_by_id("a", "hot") );
is(  $g->get_vertex_attribute_by_id("a", "hot", "taste"),  "rhubarb" );

ok(  $g->delete_vertex_attributes_by_id("a", "hot") );
ok( !$g->has_vertex_attributes_by_id("a", "hot") );
is(  $g->get_vertex_attribute_by_id("a", "hot", "taste"),  undef );

ok( !$g->delete_vertex_attribute_by_id("a", "hot", "taste" ) );
ok( $g->delete_vertex_attributes_by_id("a", "hot") );

$attr = $g->get_vertex_attributes_by_id("a", "hot");
@name = $g->get_vertex_attribute_names_by_id("a", "hot");
@val  = $g->get_vertex_attribute_values_by_id("a", "hot");

is_deeply $attr, undef;
is_deeply \@name, [];
is_deeply \@val, [];

is( $g->vertices, 1 ); # Deleting attributes does not delete vertex

$g->add_weighted_vertex_by_id("b", "cool", 42);

ok( $g->has_vertex_by_id("b", "cool") );
is( $g->get_vertex_weight_by_id("b", "cool"),  42 );

is( $g->vertices, 2 );

$g->add_weighted_vertices_by_id("b", 43, "c", 44, "cool");
is( $g->get_vertex_weight_by_id("b", "cool"),  43 );
is( $g->get_vertex_weight_by_id("c", "cool" ),  44 );

is( $g->vertices, 3 );

ok($g->set_vertex_attributes_by_id('a', 'hot',
		             { 'color' => 'pearl', 'weight' => 'heavy' }));
$attr = $g->get_vertex_attributes_by_id('a', 'hot');
is_deeply $attr, { color => "pearl", weight => 'heavy' };

ok( $g->set_vertex_weight_by_id("a", "hot", 42));
is( $g->get_vertex_weight_by_id("a", "hot"), 42);
ok( $g->has_vertex_weight_by_id("a", "hot"));
ok(!$g->has_vertex_weight_by_id("x", "hot"));
ok( $g->delete_vertex_weight_by_id("a", "hot"));
ok(!$g->has_vertex_weight_by_id("a", "hot"));
is( $g->get_vertex_weight_by_id("a", "hot"), undef);

ok( $g->set_vertex_attribute_by_id("a", 0, "zero", "absolute") );
my $got = [ sort $g->vertices ];
is_deeply($got, [qw(a a b c)]) or diag explain $got;

my $h = Graph->new(multivertexed => 1);

eval { $h->set_vertex_attribute("foo", "color", "gold") };
like($@, qr/expected non-multivertexed/);

$h->ingest($g);
$got = ($h->as_hashes)[0];
is_deeply($got, {
    a => { hot => { color => 'pearl' }, 0 => { "zero" => "absolute" } },
    b => { cool => { weight => 43 } },
    c => { cool => { weight => 44 } }
}) or diag explain $got;
