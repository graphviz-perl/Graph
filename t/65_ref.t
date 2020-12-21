use strict; use warnings;
use Test::More tests => 993;

use Graph;
use Graph::AdjacencyMap::Light;
use Graph::AdjacencyMap::Heavy;
use Graph::AdjacencyMap::Vertex;

sub _REF () { Graph::AdjacencyMap::Heavy::_REF }
sub _UNIQ () { Graph::AdjacencyMap::Heavy::_UNIQ }
sub _MULTI () { Graph::AdjacencyMap::Vertex::_MULTI }
sub _UNORD () { Graph::AdjacencyMap::Vertex::_UNORD }
sub _GEN_ID () { Graph::AdjacencyMap::_GEN_ID }

use Math::Complex;

my $t = [1, 2];
my $u = bless { 3, 4 }, "Ubu";
my $v = cplx(3, 4);
my $z = cplx(4, 5);
my @MAP_TESTS = (
    [ 'Graph::AdjacencyMap::Light', [0, 1, Graph->new], ['a'] ],
    [ 'Graph::AdjacencyMap::Heavy', [_REF, 1], [$t] ],
    [ 'Graph::AdjacencyMap::Heavy', [_REF, 2], [$u, $v] ],
    [ 'Graph::AdjacencyMap::Heavy', [_UNIQ, 2], [qw(a b)] ],
    [ 'Graph::AdjacencyMap::Heavy', [0, 2], [qw(a b)] ],
    [ 'Graph::AdjacencyMap::Heavy', [_MULTI|_UNORD, 2], [qw(a b c)] ],
    [ 'Graph::AdjacencyMap::Heavy', [_MULTI, 2], [qw(a b c)] ],
    [ 'Graph::AdjacencyMap::Vertex', [_MULTI|_UNORD, 2], [qw(a b c)] ],
);
my @METHOD_MAP = (
    { has => 'has_path', del => 'del_path', set => 'set_path' },
    { has => 'has_path_by_multi_id', del => 'del_path_by_multi_id', set => 'set_path_by_multi_id' },
);

sub test_adjmap {
    my ($class, $args, $path) = @_;
    my $m = $class->_new(@$args);
    my $is_multi = $args->[0] & _MULTI ? 1 : 0;
    my $map = $METHOD_MAP[ $is_multi ];
    my $path_shallow = [ $is_multi ? @$path[0..$#$path-1] : @$path ];
    my $got = [ $m->paths_non_existing([ $path_shallow ]) ];
    ok( !$m->${ \$map->{has} }(@$path) );
    is_deeply $got, [ $path_shallow ] or diag explain $got;
    $got = [ $m->${ \$map->{set} }(@$path) ];
    is_deeply( $got, [ $is_multi ? $path->[-1] : 0 ] ) or diag explain $got;
    ok( $m->${ \$map->{has} }(@$path) );
    ok( $m->${ \$map->{del} }(@$path) );
    ok( !$m->${ \$map->{has} }(@$path) );
    $got = [ $m->${ \$map->{set} }(@$path) ];
    is_deeply( $got, [ $is_multi ? $path->[-1] : 1 ] ) or diag explain $got;
    $got = [ $m->paths ];
    is_deeply $got, [ $path_shallow ] or diag explain $got;
    ok( $m->${ \$map->{has} }(@$path) );
    is( $m->_set_path_attr(@$path, 'say', 'hi'), 'hi' );
    is_deeply [ $m->_get_path_attr_names(@$path) ], [ 'say' ];
    is_deeply [ $m->_get_path_attr_values(@$path) ], [ 'hi' ];
    $got = [ $m->get_ids_by_paths([ $path_shallow ]) ]; # no check as Light restarts
    my @path_back = $m->get_paths_by_ids([ map [$_], @$got ]);
    is_deeply( $path_back[0][0], $path_shallow ) or diag explain \@path_back;
    if ($is_multi) {
	is $m->${ \$map->{set} }(@$path_shallow, _GEN_ID), 0;
	ok( $m->set_path_by_multi_id(@$path_shallow, 'hello') );
	$got = [ sort $m->get_multi_ids(@$path_shallow) ];
	is_deeply $got, [ qw(0 c hello) ] or diag explain $got;
    }
}

test_adjmap(@$_) for @MAP_TESTS;

my $g = Graph->new(refvertexed => 1);

$g->add_vertex($v);
$g->add_edge($v, $z);

my @V = sort { $a->sqrt <=> $b->sqrt } $g->vertices;

is($V[0]->Re, 3);
is($V[0]->Im, 4);
is($V[1]->Re, 4);
is($V[1]->Im, 5);

ok($g->has_vertex($v));
ok($g->has_vertex($z));
ok($g->has_edge($v, $z));

$v->Re(7);
$z->Im(8);

ok($g->has_vertex($v));
ok($g->has_vertex($z));

@V = sort { $a->sqrt <=> $b->sqrt } $g->vertices;

is($V[0]->Re, 4);
is($V[0]->Im, 8);
is($V[1]->Re, 7);
is($V[1]->Im, 4);

my $x = cplx(1,2);
my $y = cplx(3,4);
$g = Graph->new(refvertexed => 1);
$g->add_edge($x,$y);
my @e = $g->edges;
is("@{$e[0]}", "1+2i 3+4i");
$x->Im(5);
is("@{$e[0]}", "1+5i 3+4i");
$e[0]->[1]->Im(6);
is("$y", "3+6i");

use vars qw($foo $bar);

my $o0;
my $o1;

my $o1a = bless \$o0, 'S';
my $o1b = bless \$o1, 'S';
{ package S; use overload '""' => sub { "s" } }

my $o2a = bless [], 'A';
my $o2b = bless [], 'A';
{ package A; use overload '""' => sub { "a" } }

my $o3a = bless {}, 'H';
my $o3b = bless {}, 'H';
{ package H; use overload '""' => sub { "h" } }

my $o4a = bless sub {}, 'C';
my $o4b = bless sub {}, 'C';
{ package C; use overload '""' => sub { "c" } }

my $o5a = bless \*STDIN{IO}, 'I';
my $o5b = bless \*STDOUT{IO}, 'I';
{ package I; use overload '""' => sub { "i" } }

my $o6a = bless \*foo, 'G';
my $o6b = bless \*bar, 'G';
{ package G; use overload '""' => sub { "g" } }

for my $i ($o1a, $o2a, $o3a, $o4a, $o5a, $o6a) {
    for my $j ($o1b, $o2b, $o3b, $o4b, $o5b, $o6b) {
	print "# i = $i, j = $j\n";

	my $g1 = Graph->new(refvertexed => 1, directed => 1);

	ok( $g1->add_edge($i, $j));
	print "# g1 = $g1\n";
	ok( $g1->has_vertex($i));
	ok( $g1->has_vertex($j));
	ok( $g1->has_edge($i, $j));
	ok( $g1->delete_vertex($i));
	print "# g1 = $g1\n";
	ok(!$g1->has_vertex($i));
	ok( $g1->has_vertex($j));
	ok(!$g1->has_edge($i, $j));
	ok($g1->delete_vertex($j));
	print "# g1 = $g1\n";
	ok(!$g1->has_vertex($i));
	ok(!$g1->has_vertex($j));
	ok(!$g1->has_edge($i, $j));

	my $g2 = Graph->new(refvertexed => 1, directed => 0);

	ok( $g2->add_edge($i, $j));
	print "# g2 = $g2\n";
	ok( $g2->has_vertex($i));
	ok( $g2->has_vertex($j));
	ok( $g2->has_edge($i, $j));
	ok( $g2->delete_vertex($i));
	print "# g2 = $g2\n";
	ok(!$g2->has_vertex($i));
	ok( $g2->has_vertex($j));
	ok(!$g2->has_edge($i, $j));
	ok($g2->delete_vertex($j));
	print "# g2 = $g2\n";
	ok(!$g2->has_vertex($i));
	ok(!$g2->has_vertex($j));
	ok(!$g2->has_edge($i, $j));
    }
}

