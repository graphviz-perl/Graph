use strict; use warnings;
use Test::More;

use Graph::Directed;
use Graph::Undirected;

my $g0 = Graph::Directed->new;
$g0->add_edge(qw(a b));
$g0->add_edge(qw(a c));
$g0->add_edge(qw(c d));
ok(!$g0->is_transitive);

my $t0 = Graph::TransitiveClosure->new($g0->deep_copy);
is $t0, "a-a,a-b,a-c,a-d,b-b,c-c,c-d,d-d";
ok( $t0->is_transitive);

my $r0 = Graph::TransitiveClosure->new($g0->deep_copy, reflexive => 0);
is $r0, "a-b,a-c,a-d,c-d";
ok !$r0->transitive_closure_matrix->is_transitive(qw(a a)), 'r0 !is_transitive a a';
ok $r0->transitive_closure_matrix->is_transitive(qw(a c)), 'r0 is_transitive a c';
ok !$r0->transitive_closure_matrix->is_transitive(qw(d a)), 'r0 !is_transitive d a';
ok( $r0->is_transitive);

my $r1 = Graph::TransitiveClosure->new($g0->deep_copy, reflexive => 1);
is $r1, "a-a,a-b,a-c,a-d,b-b,c-c,c-d,d-d";
ok $r1->transitive_closure_matrix->is_transitive(qw(a a)), 'r1 is_transitive a a';
ok $r1->transitive_closure_matrix->is_transitive(qw(a c)), 'r1 is_transitive a c';
ok !$r1->transitive_closure_matrix->is_transitive(qw(d a)), 'r1 !is_transitive d a';
ok( $r1->is_transitive);

my $g1 = Graph::Undirected->new;
$g1->add_edge(qw(a b));
$g1->add_edge(qw(a c));
$g1->add_edge(qw(c d));
ok(!$g1->is_transitive);

my $t1 = Graph::TransitiveClosure->new($g1->deep_copy);
is $t1, "a=a,a=b,a=c,a=d,b=b,b=c,b=d,c=c,c=d,d=d";
is("@{[$t1->path_vertices(qw(a d))]}", "a c d");
is($t1->path_length(qw(a b)), 1);
ok( $t1->is_transitive);

my $g2 = Graph->new;
$g2->add_weighted_edge(qw(a b 3));
$g2->add_weighted_edge(qw(b c 1));
ok(!$g2->is_transitive);

my $t2 = Graph::TransitiveClosure->new($g2->deep_copy, path => 1);

is($t2->path_length(qw(a a)), 0);
is($t2->path_length(qw(a b)), 3);
is($t2->path_length(qw(a c)), 4);
is($t2->path_length(qw(b a)), undef);
is($t2->path_length(qw(b b)), 0);
is($t2->path_length(qw(b c)), 1);
is($t2->path_length(qw(c a)), undef);
is($t2->path_length(qw(c b)), undef);
is($t2->path_length(qw(c c)), 0);

is("@{[$t2->path_vertices(qw(a a))]}", "");
is("@{[$t2->path_vertices(qw(a b))]}", "a b");
is("@{[$t2->path_vertices(qw(a c))]}", "a b c");
is("@{[$t2->path_vertices(qw(b a))]}", "");
is("@{[$t2->path_vertices(qw(b b))]}", "");
is("@{[$t2->path_vertices(qw(b c))]}", "b c");
is("@{[$t2->path_vertices(qw(c a))]}", "");
is("@{[$t2->path_vertices(qw(c b))]}", "");
is("@{[$t2->path_vertices(qw(c c))]}", "");

ok( $t2->is_transitive);

my $g3 = Graph->new;
$g3->add_edge(qw(a b));
$g3->add_edge(qw(b c));

ok(!$g3->deep_copy->is_transitive);

my $t3 = Graph::TransitiveClosure->new($g3->deep_copy, path => 1);

is($t3->path_length(qw(a a)), 0);
is($t3->path_length(qw(a b)), 1);
is($t3->path_length(qw(a c)), 2);
is($t3->path_length(qw(b a)), undef);
is($t3->path_length(qw(b b)), 0);
is($t3->path_length(qw(b c)), 1);
is($t3->path_length(qw(c a)), undef);
is($t3->path_length(qw(c b)), undef);
is($t3->path_length(qw(c c)), 0);

is("@{[$t3->path_vertices(qw(a a))]}", "");
is("@{[$t3->path_vertices(qw(a b))]}", "a b");
is("@{[$t3->path_vertices(qw(a c))]}", "a b c");
is("@{[$t3->path_vertices(qw(b a))]}", "");
is("@{[$t3->path_vertices(qw(b b))]}", "");
is("@{[$t3->path_vertices(qw(b c))]}", "b c");
is("@{[$t3->path_vertices(qw(c a))]}", "");
is("@{[$t3->path_vertices(qw(c b))]}", "");
is("@{[$t3->path_vertices(qw(c c))]}", "");

is($t3->path_successor(qw(a a)), undef);
is($t3->path_successor(qw(a b)), "b");
is($t3->path_successor(qw(a c)), "b");
is($t3->path_successor(qw(b a)), undef);
is($t3->path_successor(qw(b b)), undef);
is($t3->path_successor(qw(b c)), "c");
is($t3->path_successor(qw(c a)), undef);
is($t3->path_successor(qw(c b)), undef);
is($t3->path_successor(qw(c c)), undef);

ok( $t3->is_transitive);

is($g3->path_length(qw(a a)), 0);
is($g3->path_length(qw(a b)), 1);
is($g3->path_length(qw(a c)), 2);
is($g3->path_length(qw(b a)), undef);
is($g3->path_length(qw(b b)), 0);
is($g3->path_length(qw(b c)), 1);
is($g3->path_length(qw(c a)), undef);
is($g3->path_length(qw(c b)), undef);
is($g3->path_length(qw(c c)), 0);

is("@{[$g3->path_vertices(qw(a a))]}", "");
is("@{[$g3->path_vertices(qw(a b))]}", "a b");
is("@{[$g3->path_vertices(qw(a c))]}", "a b c");
is("@{[$g3->path_vertices(qw(b a))]}", "");
is("@{[$g3->path_vertices(qw(b b))]}", "");
is("@{[$g3->path_vertices(qw(b c))]}", "b c");
is("@{[$g3->path_vertices(qw(c a))]}", "");
is("@{[$g3->path_vertices(qw(c b))]}", "");
is("@{[$g3->path_vertices(qw(c c))]}", "");

is($g3->path_successor(qw(a a)), undef);
is($g3->path_successor(qw(a b)), "b");
is($g3->path_successor(qw(a c)), "b");
is($g3->path_successor(qw(b a)), undef);
is($g3->path_successor(qw(b b)), undef);
is($g3->path_successor(qw(b c)), "c");
is($g3->path_successor(qw(c a)), undef);
is($g3->path_successor(qw(c b)), undef);
is($g3->path_successor(qw(c c)), undef);

{
    # Found by Nathan Goodman.
    is($t3->path_vertices("a", "b"), 2);
    is($t3->path_vertices("a", "b"), 2); # Crashed or hung, depending.
}

{
    my $g4 = Graph::Directed->new;
    $g4->set_edge_attribute("a", "b", "distance", 2);
    $g4->set_edge_attribute("b", "c", "distance", 3);
    my $t4 = Graph::TransitiveClosure->new($g4,
					   attribute_name => 'distance',
					   path_length => 1);
    is($t4->path_length("a", "c"), 5);
}

{
    # Found by Nathan Goodman.
    use Graph::Directed;
    my $graph = new Graph::Directed;

    $graph->add_weighted_edge('a', 'b', 1);
    $graph->add_weighted_edge('b', 'a', 1);

    my $tc = new Graph::TransitiveClosure($graph,
					  path_length => 1,
					  path_vertices => 1);

    is($tc->path_length('a','a'),   0);
    is($tc->path_vertices('a','a'), 0);

    is($tc->path_length('b','b'),   0);
    is($tc->path_vertices('b','b'), 0);

    # Some extra ones.

    is($tc->path_length('a','b'),   1);
    is($tc->path_vertices('a','b'), 2);

    is($tc->path_length('b','a'),   1);
    is($tc->path_vertices('b','a'), 2);

    ok($tc->is_reachable('a', 'a'));
    ok($tc->is_reachable('a', 'b'));
    ok($tc->is_reachable('b', 'a'));
    ok($tc->is_reachable('b', 'b'));
}

{
    use Graph::Directed;
    my $graph = new Graph::Directed;

    $graph->add_edge('a', 'b');
    $graph->add_edge('b', 'a');

    my $tc = new Graph::TransitiveClosure($graph,
					  path_length => 1,
					  path_vertices => 1);

    is($tc->path_length('a','a'),   0);
    is($tc->path_vertices('a','a'), 0);

    is($tc->path_length('b','b'),   0);
    is($tc->path_vertices('b','b'), 0);

    is($tc->path_length('a','b'),   1);
    is($tc->path_vertices('a','b'), 2);

    is($tc->path_length('b', 'a'),  1);
    is($tc->path_vertices('b','a'), 2);

    ok($tc->is_reachable('a', 'a'));
    ok($tc->is_reachable('a', 'b'));
    ok($tc->is_reachable('b', 'a'));
    ok($tc->is_reachable('b', 'b'));
}

{
    # More Nathan Goodman.
    use Graph::Directed;
    my $graph = new Graph::Directed;

    $graph->add_weighted_edge('a', 'a', 1);
    my $tc = new Graph::TransitiveClosure($graph,
					  path_length => 1,
					  path_vertices => 1);

    ok($tc->is_reachable('a', 'a'));
    is($tc->path_length('a', 'a'), 0);
    is($tc->path_vertices('a', 'a'), 0);

    # More extra.
    is($tc->path_length('b','b'),   undef);
    is($tc->path_vertices('b','b'), undef);

    is($tc->path_length('a','b'),   undef);
    is($tc->path_vertices('a','b'), undef);

    is($tc->path_length('b', 'a'),  undef);
    is($tc->path_vertices('b','a'), undef);

    is($tc->is_reachable('a', 'b'), undef);
    is($tc->is_reachable('b', 'a'), undef);
    is($tc->is_reachable('b', 'b'), undef);
}

# TransitiveClosure_Floyd_Warshall is just an alias for TransitiveClosure.

my $t0tcfw = $g0->deep_copy->TransitiveClosure_Floyd_Warshall;

is($t0, $t0tcfw);

my $t3apspfw = $g3->deep_copy->APSP_Floyd_Warshall;

is($t3, $t3apspfw);

is($t3apspfw->path_length(qw(a a)), 0);
is($t3apspfw->path_length(qw(a b)), 1);
is($t3apspfw->path_length(qw(a c)), 2);
is($t3apspfw->path_length(qw(b a)), undef);
is($t3apspfw->path_length(qw(b b)), 0);
is($t3apspfw->path_length(qw(b c)), 1);
is($t3apspfw->path_length(qw(c a)), undef);
is($t3apspfw->path_length(qw(c b)), undef);
is($t3apspfw->path_length(qw(c c)), 0);

is("@{[$t3apspfw->path_vertices(qw(a a))]}", "");
is("@{[$t3apspfw->path_vertices(qw(a b))]}", "a b");
is("@{[$t3apspfw->path_vertices(qw(a c))]}", "a b c");
is("@{[$t3apspfw->path_vertices(qw(b a))]}", "");
is("@{[$t3apspfw->path_vertices(qw(b b))]}", "");
is("@{[$t3apspfw->path_vertices(qw(b c))]}", "b c");
is("@{[$t3apspfw->path_vertices(qw(c a))]}", "");
is("@{[$t3apspfw->path_vertices(qw(c b))]}", "");
is("@{[$t3apspfw->path_vertices(qw(c c))]}", "");

is($t3apspfw->path_successor(qw(a a)), undef);
is($t3apspfw->path_successor(qw(a b)), "b");
is($t3apspfw->path_successor(qw(a c)), "b");
is($t3apspfw->path_successor(qw(b a)), undef);
is($t3apspfw->path_successor(qw(b b)), undef);
is($t3apspfw->path_successor(qw(b c)), "c");
is($t3apspfw->path_successor(qw(c a)), undef);
is($t3apspfw->path_successor(qw(c b)), undef);
is($t3apspfw->path_successor(qw(c c)), undef);

{
    # From Andras Salamon
    use Graph;
    my $g = Graph->new;
    $g->add_edges(qw(a b b c a d d e b f));
    my $t = $g->TransitiveClosure_Floyd_Warshall; # the calling convention
    ok( $t->is_reachable('a', 'f'));
    ok(!$t->is_reachable('c', 'f'));
}

{
    # From Andras Salamon
    my $g = Graph->new;
    $g->add_edges( qw( a b b c ) );
    $g->add_vertex( 'd' );

    my $t0 = $g->deep_copy->TransitiveClosure_Floyd_Warshall(reflexive => 0);
    ok( $t0->has_vertex( 'a' ) );
    ok(!$t0->has_vertex( 'd' ) );

    my $t1 = $g->deep_copy->TransitiveClosure_Floyd_Warshall(reflexive => 1);
    ok( $t1->has_vertex( 'a' ) );
    ok( $t1->has_vertex( 'd' ) );

    # test caching of TCM
    my $t0c = $g->TransitiveClosure_Floyd_Warshall(reflexive => 0);
    ok( $t0c->has_vertex( 'a' ) );
    ok(!$t0c->has_vertex( 'd' ) );

    my $t1c = $g->TransitiveClosure_Floyd_Warshall(reflexive => 1);
    ok( $t1c->has_vertex( 'a' ) );
    ok( $t1c->has_vertex( 'd' ) );
}

{
    # From Andras Salamon
    use Graph::Directed;
    my $g = new Graph::Directed;
    $g->add_edges( qw(a b b c) );
    is($g->APSP_Floyd_Warshall, 'a-a,a-b,a-c,b-b,b-c,c-c');
}

{
    # From Nathan Goodman.
    my $graph=new Graph::Directed;
    $graph->add_weighted_edge(0,1,1);
    $graph->add_weighted_edge(1,2,1);

    my $tc1=new Graph::TransitiveClosure($graph->deep_copy);

    is ("@{[sort $tc1->path_vertices(0,1)]}", "0 1");
    is ("@{[sort $tc1->path_vertices(0,2)]}", "0 1 2");
    is ("@{[sort $tc1->path_vertices(1,2)]}", "1 2");

    my $tc2=new Graph::TransitiveClosure($graph->deep_copy,path_length=>1,path_vertices=>1);

    is ("@{[sort $tc2->path_vertices(0,1)]}", "0 1");
    is ("@{[sort $tc2->path_vertices(0,2)]}", "0 1 2");
    is ("@{[sort $tc2->path_vertices(1,2)]}", "1 2");
}

{
    # From Jon Freeman.
    my @example = ( [ 1, 3, -2 ],
                    [ 3, 4, 2 ],
                    [ 4, 2, -1 ],
                    [ 2, 1, 4 ],
                    [ 2, 3, 3 ] );
    my $g = Graph::Directed->new;
    $g->add_weighted_edge(@$_) for @example;
    my $apsp = $g->APSP_Floyd_Warshall();
    # The output from APSP_Floyd_Warshall was non-deterministically
    # incorrect for two of the possible vertex pairs due to an "|| 1"
    # instead of defined-or across the 1-3-4 path which had "distance" 2+-2=0
    my @bad_edges = ( [1, 2, -1, [1,3,4,2]], [2, 4, 4, [2,1,3,4]] );
    foreach my $e (@bad_edges) {
        my ($u, $v, $length, $path) = @$e;
        my @spvs = $apsp->path_vertices($u, $v);
        is_deeply \@spvs, $path, "APSP $u $v" or diag explain \@spvs;
        is $apsp->path_length($u, $v), $length, "length $u $v";
    }
}

{
    my @example = ( [ 1, 2 ],
                    [ 1, 3 ],
                    [ 1, 4 ], # direct link to two away
                    [ 3, 4 ] );
    my $g = Graph::Directed->new;
    $g->add_edge(@$_) for @example;
    my $path_counts = $g->APSP_Floyd_Warshall(path_count => 1);
    my @counts = (
	[ 1, 2, 1 ],
	[ 1, 3, 1 ],
	[ 1, 4, 2 ],
	[ 2, 1, 0 ],
	[ 2, 3, 0 ],
	[ 2, 4, 0 ],
	[ 3, 1, 0 ],
	[ 3, 2, 0 ],
	[ 3, 4, 1 ],
	[ 4, 1, 0 ],
	[ 4, 2, 0 ],
	[ 4, 3, 0 ],
    );
    foreach my $e (@counts) {
        my ($u, $v, $count) = @$e;
        is $path_counts->path_length($u, $v), $count, "count $u $v";
    }
}

{
    my @example = ( [ 1, 2 ],
                    [ 1, 3 ],
                    [ 1, 4 ], # direct link to two away
                    [ 1, 1 ], # self-loop
                    [ 3, 4 ] );
    my $g = Graph::Directed->new;
    $g->add_edge(@$_) for @example;
    my $tcg = $g->transitive_closure;
    is $tcg->transitive_closure_matrix->[1]->stringify, <<'EOF';
 to:    1    2    3    4
   1    0    1    1    1
   2         0          
   3              0    1
   4                   0
EOF
    my @paths = (
	[ 1, 2, [[1,2]] ],
	[ 1, 3, [[1,3]] ],
	[ 1, 4, [[1,3,4], [1,4]] ],
	[ 2, 1, [] ],
	[ 2, 3, [] ],
	[ 2, 4, [] ],
	[ 3, 1, [] ],
	[ 3, 2, [] ],
	[ 3, 4, [[3,4]] ],
	[ 4, 1, [] ],
	[ 4, 2, [] ],
	[ 4, 3, [] ],
    );
    foreach my $t (@paths) {
        my ($u, $v, $paths) = @$t;
        my $got = [ sort { $a->[1] <=> $b->[1] } $g->all_paths($u, $v) ];
        is_deeply $got, $paths, "paths $u $v" or diag explain $got;
    }
}

{
    my @example = ( [ 1, 2, [[qw(a weight 1)], [qw(b weight 2)]] ],
		    [ 1, 3, [[qw(c other 1)], [qw(b weight 2)]] ],
		    [ 1, 4, [[qw(d weight 4)], [qw(b weight 5)]] ], # direct link to two away
		    [ 3, 4, [[qw(d weight 3)], [qw(3 weight 2)]] ] );
    my $g = Graph::Directed->new(multiedged => 1);
    for my $t (@example) {
	my ($u, $v, $e) = @$t;
	$g->set_edge_attribute_by_id($u, $v, @$_) for @$e;
    }
    my $tcg = $g->transitive_closure;
    my @paths = (
	[ 1, 2, 1, [[1,2]] ],
	[ 1, 3, 2, [[1,3]] ],
	[ 1, 4, 4, [[1,3,4], [1,4]] ],
	[ 2, 1, undef, [] ],
	[ 2, 3, undef, [] ],
	[ 2, 4, undef, [] ],
	[ 3, 1, undef, [] ],
	[ 3, 2, undef, [] ],
	[ 3, 4, 2, [[3,4]] ],
	[ 4, 1, undef, [] ],
	[ 4, 2, undef, [] ],
	[ 4, 3, undef, [] ],
    );
    foreach my $t (@paths) {
	my ($u, $v, $dist, $paths) = @$t;
	my $got = [ sort { $a->[1] <=> $b->[1] } $g->all_paths($u, $v) ];
	is_deeply $got, $paths, "paths $u $v" or diag explain $got;
	is $tcg->path_length($u, $v), $dist, "dist $u $v";
    }
}

{
    my $g = Graph::Undirected->new;
    $g->add_path(qw(A B C A));
    my $tcg = $g->transitive_closure;
    my @paths = sort { @$a <=> @$b } $tcg->all_paths("A", "C");
    is_deeply \@paths, [ [qw(A C)], [qw(A B C)] ], "no infinite loop";
}

{
    # 9 4 8 are a cycle, plus longer cycle 9 4 8 7
    # other paths: 1-7, 6-2-3
    my @example = (
	[9, 4],
	[2, 3],
	[7, 9],
	[8, 7],
	[6, 2],
	[4, 8],
	[8, 9],
	[1, 7],
    );
    my $g = Graph::Directed->new;
    $g->add_weighted_edge(@$_, 1) for @example;
    my $tcg = $g->transitive_closure;
    is $tcg->transitive_closure_matrix->[0]->stringify, <<'EOF', 'adjacency';
 to:    1    2    3    4    6    7    8    9
   1    1    0    0    1    0    1    1    1
   2    0    1    1    0    0    0    0    0
   3    0    0    1    0    0    0    0    0
   4    0    0    0    1    0    1    1    1
   6    0    1    1    0    1    0    0    0
   7    0    0    0    1    0    1    1    1
   8    0    0    0    1    0    1    1    1
   9    0    0    0    1    0    1    1    1
EOF
    is $tcg->transitive_closure_matrix->[1]->stringify, <<'EOF', 'distances';
 to:    1    2    3    4    6    7    8    9
   1    0              3         1    4    2
   2         0    1                         
   3              0                         
   4                   0         2    1    2
   6         1    2         0               
   7                   2         0    3    1
   8                   2         1    0    1
   9                   1         3    2    0
EOF
    ok $tcg->is_reachable(7, 8), '7-8 reachable when on cycle';
}

{
  my $g = Graph::Directed->new(edges => [
    [qw(A C)], [qw(A NOTA)], [qw(B A)], [qw(B C)], [qw(B NOTA)],
  ]);
  $g->delete_vertex('C');
  my $tc = $g->transitive_closure;
  is $tc, 'A-A,A-NOTA,B-A,B-B,B-NOTA,NOTA-NOTA';
  $tc->delete_edge($_,$_) for qw(A B C N);
  is $tc, 'A-NOTA,B-A,B-NOTA,NOTA-NOTA';
}

done_testing;
