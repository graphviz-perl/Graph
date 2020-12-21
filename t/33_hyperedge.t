use strict; use warnings;
use Test::More;

use Graph;
my $g = Graph->new(hyperedged => 1, directed => 0);

$g->add_edge();
$g->add_edge("a");
$g->add_edge("b", "c");
$g->add_edge("d", "e", "f");

is( $g->edges, 4 );

my @e = $g->edges;

is ( "@{[ sort map { qq'[@$_]' } @e ]}", "[] [a] [b c] [d e f]" );

ok( $g->has_edge() );
ok( $g->has_edge("a") );
ok( $g->has_edge("b", "c") );
ok( $g->has_edge("d", "e", "f") );
ok( $g->any_edge("d", "e", "f") );
ok( $g->any_edge("e", "f") );
ok( $g->any_edge("f", "e") );
ok( !$g->any_edge("a", "e") );

ok( ! $g->has_edge("b") );
ok( ! $g->has_edge("c") );
ok( ! $g->has_edge("d", "e") );

$g->add_edge("d", "e", "g");

is $g, ",a,b=c,d=e=f,d=e=g";

is( $g->delete_edge("d", "e", "f"), $g );
is $g, ",a,b=c,d=e=g,f";

ok( ! $g->has_edge("d", "e", "f") );
ok(   $g->has_edge("d", "e", "g") );

is( $g->delete_edge("d", "e", "f"), $g );

is ( "@{[ sort map { qq'[@$_]' } $g->edges ]}", "[] [a] [b c] [d e g]" );

$g = Graph->new(hyperedged => 1, directed => 1);

$g->set_edge_attributes([qw(a b c)], [qw(f g)],
                           { color => 'pearl', weight => 'heavy' });
$g->add_weighted_edge([qw(a b c)], [qw(f h)], 123);
$g->add_weighted_path(["c"], 45, ["d"], 46, ["e"]);

ok !$g->has_edge([qw(a c)], [qw(f g)]);
ok( $g->any_edge("c", "g") );
ok( $g->any_edge("c", "d") );
ok( !$g->any_edge("b", "d") );
ok $g->has_edge([qw(a b c)], [qw(f g)]) or diag explain $g;
ok $g->has_edge([qw(b a c)], [qw(f g)]);
ok $g->has_edge([qw(a b c)], [qw(f h)]) or diag explain $g;
ok !$g->has_edge([qw(f h)], [qw(a b c)]) or diag explain $g;
ok $g->has_edge([qw(c)], [qw(d)]) or diag explain $g;
ok $g->has_edge([qw(d)], [qw(e)]) or diag explain $g;
is_deeply [ $g->edges_to('e') ], [ [['d'], ['e']] ];
is_deeply [ $g->edges_from('d') ], [ [[qw(d)], [qw(e)]] ];
is_deeply [ $g->edges_at('e') ], [ [[qw(d)], [qw(e)]] ] or diag explain [ $g->edges_at('e') ];
is_deeply [ sort $g->successors('c') ], [qw(d f g h)];
is_deeply [ $g->predecessors('e') ], [qw(d)];
is $g, "[a,b,c]-[f,g],[a,b,c]-[f,h],[c]-[d],[d]-[e]";

done_testing;
