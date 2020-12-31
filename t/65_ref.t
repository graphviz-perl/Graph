use strict; use warnings;
use Test::More;

use Graph;
use Graph::AdjacencyMap qw(:flags);
use Graph::AdjacencyMap::Light;

use Math::Complex;
use List::Util qw(uniq);

my $t = [1, 2];
my $u = bless { 3, 4 }, "Ubu";
my $v = cplx(3, 4);
my $z = cplx(4, 5);
my @MAP_TESTS = (
    [ 'Graph::AdjacencyMap::Light', [0, 1], [['a']] ],
    [ 'Graph::AdjacencyMap::Light', [0, 2], [[qw(a b)]] ],
    [ 'Graph::AdjacencyMap', [_REF, 1], [[$t]] ],
    [ 'Graph::AdjacencyMap', [_REF, 2], [[$u, $v]] ],
    [ 'Graph::AdjacencyMap', [_UNIQ, 2], [[qw(a b)]] ],
    [ 'Graph::AdjacencyMap', [_UNIQ, undef], [[qw(a a b)]] ],
    [ 'Graph::AdjacencyMap', [0, undef], [[qw()]] ],
    [ 'Graph::AdjacencyMap', [0, undef], [[qw(a b c d)]] ],
    [ 'Graph::AdjacencyMap', [0, 2], [[qw(a b)]] ],
    [ 'Graph::AdjacencyMap', [_MULTI, 1], [['a'], 'b'] ],
    [ 'Graph::AdjacencyMap', [_MULTI, 2], [[qw(a b)], 'c'] ],
    [ 'Graph::AdjacencyMap', [_COUNT, 1], [[qw(a)]] ],
    [ 'Graph::AdjacencyMap', [_COUNT, 2], [[qw(a b)]] ],
);
my @METHOD_MAP = (
    { has => 'has_path', del => 'del_path', set => 'set_path' },
    { has => 'has_path_by_multi_id', del => 'del_path_by_multi_id', set => 'set_path_by_multi_id' },
);

sub test_adjmap {
    my ($class, $args, $path_maybe_id) = @_;
    my ($path, $maybe_id) = @$path_maybe_id;
    my $m = $class->_new(@$args);
    my $is_multi = $m->_is_MULTI ? 1 : 0;
    my $maybe_count = $m->_is_COUNT ? 2 : 1;
    my $map = $METHOD_MAP[ $is_multi ];
    my $path_expected = [ $m->_is_UNIQ ? uniq @$path : @$path ];
    my $label = "$class(@{[Graph::AdjacencyMap::_stringify_fields($args->[0])]}, @{[$m->_dumper($args->[1])]})";
    my $got = [ $m->paths_non_existing([ $path_expected ]) ];
    is_deeply $got, [ $path_expected ], $label or diag explain $got;
    ok( !$m->has_paths, $label );
    is( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
    $got = [ $m->${ \$map->{set} }(@$path_maybe_id) ];
    is_deeply( $got, [ $is_multi ? $maybe_id : 0 ], $label ) or diag explain $got;
    is $m->_set_path_attr(@$path_maybe_id, 'say', 'hi'), 'hi', $label;
    ok $m->_has_path_attrs(@$path_maybe_id), $label;
    ok $m->_del_path_attrs(@$path_maybe_id);
    ok( $m->has_paths, $label );
    isnt( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
    $m->${ \$map->{set} }(@$path_maybe_id); # second time
    is( $m->_get_path_count($path_expected), $maybe_count, $label );
    ok( $m->${ \$map->{del} }(@$path_maybe_id), $label ) for 1..$maybe_count;
    is( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
    is( $m->_get_path_count($path_expected), 0, $label );
    $got = [ $m->${ \$map->{set} }(@$path_maybe_id) ];
    is_deeply( $got, [ $is_multi ? $maybe_id : 1 ], $label ) or diag explain $got;
    is( $m->_get_path_count($path_expected), 1, $label );
    $got = [ $m->paths ];
    is_deeply $got, [ $path_expected ], $label or diag explain $got;
    isnt( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
    $got = [ $m->get_ids_by_paths([ $path_expected ]) ];
    is_deeply $got, [ 1 ], $label or diag explain $got;
    my @path_back = $m->get_paths_by_ids([ map [$_], @$got ]);
    is_deeply( $path_back[0][0], $path_expected, $label ) or diag explain \@path_back;
    eval { $m->stringify };
    is $@, '', $label;
    if (@$path_expected == 1) {
	ok $m->rename_path(@$path_expected, 'newname', $label);
	is( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
	isnt( $m->${ \$map->{has} }(['newname'], $is_multi ? $maybe_id : ()), undef, $label );
	ok $m->rename_path('newname', @$path_expected), $label;
	isnt( $m->${ \$map->{has} }(@$path_maybe_id), undef, $label );
	is( $m->${ \$map->{has} }(['newname'], $is_multi ? $maybe_id : ()), undef, $label );
    }
    ok( !$m->_has_path_attrs(@$path_maybe_id), $label );
    is( $m->_set_path_attr(@$path_maybe_id, 'say', 'hi'), 'hi', $label );
    ok( $m->_has_path_attr(@$path_maybe_id, 'say'), $label );
    ok( $m->_has_path_attrs(@$path_maybe_id), $label );
    is_deeply [ $m->_get_path_attr_names(@$path_maybe_id) ], [ 'say' ], $label;
    is_deeply [ $m->_get_path_attr_values(@$path_maybe_id) ], [ 'hi' ], $label;
    if (@$path_expected == 1) {
	my @new_path_full = @$path_maybe_id;
	$new_path_full[0] = ['newname'];
	ok $m->rename_path(@$path_expected, 'newname'), $label;
	is_deeply [ $m->_get_path_attr_names(@new_path_full) ], [ 'say' ], $label;
	ok $m->rename_path('newname', @$path_expected), $label;
	is_deeply [ $m->_get_path_attr_names(@$path_maybe_id) ], [ 'say' ], $label;
    }
    $got = $m->_get_path_attrs(@$path_maybe_id);
    is_deeply $got, { say => 'hi' }, $label or diag explain $got;
    $got = { %$got, extra => 'hello' };
    $got = $m->_set_path_attrs(@$path_maybe_id, $got);
    is_deeply [ sort $m->_get_path_attr_names(@$path_maybe_id) ], [ qw(extra say) ], $label;
    is_deeply [ $m->_get_path_attr(@$path_maybe_id, 'extra') ], [ qw(hello) ], $label;
    $m->_del_path_attr(@$path_maybe_id, 'extra');
    is_deeply [ $m->_get_path_attr_names(@$path_maybe_id) ], [ qw(say) ], $label;
    $got = [ $m->_get_path_attrs(@$path_maybe_id) ];
    is_deeply( $got, [ { say => 'hi' } ] ), $label or diag explain $got;
    $m->_del_path_attr(@$path_maybe_id, 'say');
    is_deeply [ $m->_get_path_attr_names(@$path_maybe_id) ], [ ], $label;
    is( $m->_get_path_count($path_expected), 1, $label );
    $m->_set_path_attr(@$path_maybe_id, 'say', 'hi');
    ok $m->_del_path_attrs(@$path_maybe_id);
    ok( !$m->_has_path_attr(@$path_maybe_id, 'say'), $label );
    is( $m->_get_path_count($path_expected), 1, $label );
    if ($is_multi) {
	is $m->${ \$map->{set} }($path_expected, _GEN_ID), 0, $label;
	ok( $m->set_path_by_multi_id($path_expected, 'hello'), $label );
	$got = [ sort $m->get_multi_ids($path_expected) ];
	is_deeply $got, [ sort $path_maybe_id->[-1], qw(0 hello) ], $label or diag explain $got;
    }
    $m->_set_path_attr(@$path_maybe_id, 'say', 'hi');
    $m->${ \$map->{del} }(@$path_maybe_id);
    ok( !$m->_has_path_attr(@$path_maybe_id, 'say'), $label );
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
is("@{$e[0] || []}", "1+2i 3+4i");
$x->Im(5);
is("@{$e[0] || []}", "1+5i 3+4i");
SKIP: {
skip 'no object to invoke', 1 unless @e and ref $e[0]->[1];
$e[0]->[1]->Im(6);
is("$y", "3+6i");
}

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

done_testing;
