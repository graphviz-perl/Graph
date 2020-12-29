use strict; use warnings;
use Test::More tests => 187;

use Graph;
use Graph::Directed;
use Graph::Undirected;

my @ARGS;
{
no warnings qw(qw); # the commas
@ARGS = (
    [qw(graph graphs 0)],
    [qw(vertex vertices 1 a a,b a a,b,c)],
    [qw(edge edges 2 a-a a-a,b-b a-a,b a-a,b-b,c-c a-a,b-b,b-c,c-c)],
);
}
for my $t (@ARGS) {
    my ($what, $whats, $arity, $g1, $g2, $g3, $g4, $g5) = @$t;
    my @args = ('a') x $arity;
    my @args2 = ('b') x $arity;
    my @args3 = ('c') x $arity;
    my @anti_args = ("x") x $arity;
    my (
	$add_e, $del_e, $has_e,
	$add_w, $has_w, $get_w, $set_w, $del_w,
	$names, $values,
	$has, $get, $set, $del,
	$hass, $gets, $sets, $dels,
    ) = map sprintf($_, $what), qw(
	add_%s delete_%s has_%s
	add_weighted_%s has_%s_weight get_%s_weight set_%s_weight delete_%s_weight
	get_%s_attribute_names get_%s_attribute_values
	has_%s_attribute get_%s_attribute set_%s_attribute delete_%s_attribute
	has_%s_attributes get_%s_attributes set_%s_attributes delete_%s_attributes
    );
    my (
	$list_e, $add_ws,
    ) = map sprintf($_, $whats), qw(
	%s add_weighted_%s
    );

    my $g_d = Graph::Directed->new;
    my $g_u = Graph::Undirected->new;
    $_->add_edge(1, 1) for $g_d, $g_u;
    $g_d->$set((1) x $arity, 'color', 'electric blue');
    $g_u->$set((1) x $arity, 'color', 'firetruck red');
    is $g_d, '1-1';
    is $g_u, '1=1';

    my $g = Graph->new;

    $g->$add_e(@args) if $arity;

    ok( !$g->$hass(@args) ) for 1..2;

    ok( $g->$set(@args, "color", "red") );

    ok( $g->$has(@args, "color") ) for 1..2;

    ok( $g->$hass(@args) ) for 1..2;

    is( $g->$get(@args, "color"),  "red" ) for 1..2;

    is( $g->$get(@args, "colour"), undef ) for 1..2;

    ok( $g->$set(@args, "color", "green") );

    ok( $g->$hass(@args) ) for 1..2;

    is( $g->$get(@args, "color"),  "green" ) for 1..2;

    is_deeply $g->$gets(@args), { color => "green" };
    is_deeply [ $g->$gets(@anti_args) ], [ undef ] if $arity;
    is_deeply [ $g->$names(@args) ], [ 'color' ];
    is_deeply [ $g->$values(@args) ], [ 'green' ];

    ok( $g->$set(@args, "taste", "rhubarb") );

    ok( $g->$hass(@args) ) for 1..2;

    is( $g->$get(@args, "taste"),  "rhubarb" ) for 1..2;
    is( $g->$get(@args, "color"),  "green" );
    is( $g->$get(@args, "taste"),  "rhubarb" );

    is_deeply $g->$gets(@args), { color => "green", taste => "rhubarb" };
    is_deeply [ sort $g->$names(@args) ], [ qw(color taste) ];
    is_deeply [ sort $g->$values(@args) ], [ qw(green rhubarb) ];

    ok( $g->$del(@args, "color" ) );

    ok( !$g->$has(@args, "color" ) );
    ok(  $g->$hass(@args) );
    is(  $g->$get(@args, "taste"),  "rhubarb" );

    ok(  $g->$dels(@args) );
    ok( !$g->$hass(@args) );
    is(  $g->$get(@args, "taste"),  undef );

    ok( !$g->$del(@args, "taste" ) );
    ok( !$g->$dels(@args) );

    is_deeply $g->$gets(@args), undef;
    is_deeply [ $g->$names(@args) ], [];
    is_deeply [ $g->$values(@args) ], [];

    ok($g->$sets(@args, { 'color' => 'pearl', 'weight' => 'heavy' }));
    is_deeply $g->$gets(@args), { 'color' => 'pearl', 'weight' => 'heavy' };

    next if !$arity;

    ok( $g->$del_e(@args2) );
    is $g, $g1;
    ok(!$g->$has_e(@args2));
    $g->$add_w(@args2, 42);
    is $g, $g2;
    ok( $g->$has_e(@args2));
    is( $g->$get_w(@args2),  42 );
    is( $g->$get(@args2, 'weight'),  42 );

    is( $g->$list_e, 2 );

    ok( $g->$del_e(@args2) );
    ok( $g->$del_e(@args3) );
    is $g, $g3;
    $g->$add_ws(@args2, 43, @args3, 44);
    is $g, $g4;
    is( $g->$get_w(@args2),  43 );
    is( $g->$get_w(@args3),  44 );

    is( $g->$list_e, 3 );

    if ($arity > 1) {
	ok( $g->$del_e(@args2) );
	ok( $g->$del_e($args2[1], $args3[0]) );
	$g->add_weighted_path($args2[0], 45, $args2[1], 46, $args3[0]);
	is $g, $g5;
	is( $g->get_edge_weight(@args2),  45 );
	is( $g->get_edge_weight($args2[1], $args3[0]),  46 );

	is( $g->$list_e, 4 );
    }

    ok( $g->$set_w(@args, 42));
    is( $g->$get_w(@args), 42);
    ok( $g->$has_w(@args));
    ok(!$g->$has_w(@anti_args));
    ok( $g->$del_w(@args));
    ok(!$g->$has_w(@args));
    is( $g->$get_w(@args), undef);
}
