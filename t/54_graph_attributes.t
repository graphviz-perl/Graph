use strict; use warnings;
use Test::More tests => 54;

use Graph;
my $g = Graph->new;

ok( !$g->has_graph_attributes() );
ok( !$g->has_graph_attributes() );

ok( $g->set_graph_attribute("color", "red") );

ok(  $g->has_graph_attribute("color") );
ok(  $g->has_graph_attribute("color") );

ok( $g->has_graph_attributes() );
ok( $g->has_graph_attributes() );

is( $g->get_graph_attribute("color"),  "red" );
is( $g->get_graph_attribute("color"),  "red" );

is( $g->get_graph_attribute("colour"), undef );
is( $g->get_graph_attribute("colour"), undef );

ok( $g->set_graph_attribute("color", "green") );

ok( $g->has_graph_attributes() );
ok( $g->has_graph_attributes() );

is( $g->get_graph_attribute("color"),  "green" );
is( $g->get_graph_attribute("color"),  "green" );

my $attr = $g->get_graph_attributes();
my @name = $g->get_graph_attribute_names();
my @val  = $g->get_graph_attribute_values();

is( scalar keys %$attr, 1 );
is( scalar @name,       1 );
is( scalar @val,        1 );

is( $attr->{color}, "green" );
is( $name[0],       "color" );
is( $val[0],        "green" );

ok( $g->set_graph_attribute("taste", "rhubarb") );

ok( $g->has_graph_attributes() );
ok( $g->has_graph_attributes() );

is( $g->get_graph_attribute("taste"),  "rhubarb" );
is( $g->get_graph_attribute("taste"),  "rhubarb" );

is( $g->get_graph_attribute("color"),  "green" );
is( $g->get_graph_attribute("taste"),  "rhubarb" );

$attr = $g->get_graph_attributes();
@name = sort $g->get_graph_attribute_names();
@val  = sort $g->get_graph_attribute_values();

is( scalar keys %$attr, 2 );
is( scalar @name,       2 );
is( scalar @val,        2 );

is( $attr->{color}, "green" );
is( $attr->{taste}, "rhubarb" );
is( $name[0],       "color" );
is( $val[0],        "green" );
is( $name[1],       "taste" );
is( $val[1],        "rhubarb" );

ok( $g->delete_graph_attribute("color" ) );

ok( !$g->has_graph_attribute("color" ) );
ok(  $g->has_graph_attributes() );
is(  $g->get_graph_attribute("taste"),  "rhubarb" );

ok(  $g->delete_graph_attributes() );
ok( !$g->has_graph_attributes() );
is(  $g->get_graph_attribute("taste"),  undef );

ok( !$g->delete_graph_attribute("taste" ) );
ok( !$g->delete_graph_attributes() );

$attr = $g->get_graph_attributes();
@name = $g->get_graph_attribute_names();
@val  = $g->get_graph_attribute_values();

is( scalar keys %$attr, 0 );
is( scalar @name,       0 );
is( scalar @val,        0 );

ok($g->set_graph_attributes({ 'color' => 'pearl', 'weight' => 'heavy' }));
$attr = $g->get_graph_attributes();
is(scalar keys %$attr, 2);
is($attr->{color},  'pearl');
is($attr->{weight}, 'heavy');
