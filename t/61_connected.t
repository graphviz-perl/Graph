use strict; use warnings;
use Test::More tests => 264;

my %undirected_map = map +($_ => $_), qw(
    is_connected
    connected_components
    connected_component_by_vertex
    connected_component_by_index
    same_connected_components
    connected_graph
);
my %directed_map = map { (my $v=$_)=~s/connected/weakly_$&/;($_=>$v) } keys %undirected_map;
my %mapping = ('Graph::Undirected' => \%undirected_map, 'Graph::Directed', \%directed_map);

use Graph::Undirected;
use Graph::Directed;

test_graph(@$_) for (
    ['Graph::Undirected', {}],
    ['Graph::Undirected', {unionfind => 1}],
    ['Graph::Undirected', {unionfind => 1, multiedged => 1}],
    ['Graph::Directed', {}],
);

sub test_graph {
    my ($class, $args) = @_;
    my $g0 = $class->new(%$args);
    my $methmap = $mapping{$class};
    ok(!$g0->${ \$methmap->{is_connected} });
    is( $g0->${ \$methmap->{connected_components} }, 0);
    is( $g0->${ \$methmap->{connected_component_by_vertex} }('a'), undef);
    is( $g0->${ \$methmap->{connected_component_by_index} }(0), undef );
    ok(!$g0->${ \$methmap->{same_connected_components} }('a', 'b'));
    is($g0->${ \$methmap->{connected_graph} }, '');

    $g0->add_vertex('a');

    ok( $g0->${ \$methmap->{is_connected} });
    is( $g0->${ \$methmap->{connected_components} }(), 1);
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }('a'), undef);
    is( "@{[ $g0->${ \$methmap->{connected_component_by_index} }(0) ]}", 'a' );
    ok(!$g0->${ \$methmap->{same_connected_components} }('a', 'b'));
    is($g0->${ \$methmap->{connected_graph} }, 'a');

    $g0->add_vertex('b');

    ok(!$g0->${ \$methmap->{is_connected} });
    is( $g0->${ \$methmap->{connected_components} }(), 2);
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }($_), undef) for qw(a b);
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }('a'),
	 $g0->${ \$methmap->{connected_component_by_vertex} }('b'));
    my @c0 = map [ $g0->${ \$methmap->{connected_component_by_index} }(0) ], (1..3);
    is( @$_, 1 ) for @c0;
    is( "@{$c0[0]}", "@{$c0[$_]}" ) for 1, 2;
    my @c1 = map [ $g0->${ \$methmap->{connected_component_by_index} }(1) ], (1..3);
    is( @$_, 1 ) for @c1;
    is( "@{$c1[0]}", "@{$c1[$_]}" ) for 1, 2;
    isnt( "@{$c0[0]}", "@{$c1[0]}" );
    ok( ("@{$c0[0]}" eq "a" && "@{$c1[0]}" eq "b") ||
	("@{$c0[0]}" eq "b" && "@{$c1[0]}" eq "a") );
    ok(!$g0->${ \$methmap->{same_connected_components} }('a', 'b'));
    is($g0->${ \$methmap->{connected_graph} }, 'a,b');

    $g0->add_edge(qw(a b));

    ok( $g0->${ \$methmap->{is_connected} });
    is( $g0->${ \$methmap->{connected_components} }(), 1);
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }($_), undef) for qw(a b);
    is($g0->${ \$methmap->{connected_component_by_vertex} }('a'), $g0->${ \$methmap->{connected_component_by_vertex} }('b'));
    @c0 = map [ $g0->${ \$methmap->{connected_component_by_index} }(0) ], (1..3);
    is( @$_, 2 ) for @c0;
    is( "@{$c0[0]}", "@{$c0[$_]}" ) for 1, 2;
    @c1 = map [ $g0->${ \$methmap->{connected_component_by_index} }(1) ], (1..3);
    is( @$_, 0 ) for @c1;
    is( "@{[ sort @{$c0[0]} ]}", "a b" );
    ok( $g0->${ \$methmap->{same_connected_components} }('a', 'b'));
    is($g0->${ \$methmap->{connected_graph} }, 'a+b');

    $g0->add_edge(qw(c d));

    ok(!$g0->${ \$methmap->{is_connected} });
    is( $g0->${ \$methmap->{connected_components} }(), 2);
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }($_), undef) for qw(a b c d);
    is($g0->${ \$methmap->{connected_component_by_vertex} }($_->[0]), $g0->${ \$methmap->{connected_component_by_vertex} }($_->[1])) for [qw(a b)], [qw(c d)];
    isnt($g0->${ \$methmap->{connected_component_by_vertex} }('a'), $g0->${ \$methmap->{connected_component_by_vertex} }('d'));
    ok( $g0->${ \$methmap->{same_connected_components} }(@$_)) for [qw(a b)], [qw(c d)];
    ok(!$g0->${ \$methmap->{same_connected_components} }('a', 'c'));
    my $g0c = $g0->${ \$methmap->{connected_graph} };
    is($g0c, 'a+b,c+d');
    is("@{[sort @{ $g0c->get_vertex_attribute('a+b', 'subvertices') }]}", "a b");
    is("@{[sort @{ $g0c->get_vertex_attribute('c+d', 'subvertices') }]}", "c d");
    is($g0c->get_vertex_attribute('b+a', 'subvertices'), undef);
}

my $g4 = Graph::Directed->new;

eval { $g4->is_connected };
like($@, qr/Graph::is_connected: expected undirected graph, got directed/);

eval { $g4->connected_components };
like($@, qr/Graph::connected_components: expected undirected graph, got directed/);

eval { $g4->connected_component_by_vertex };
like($@, qr/Graph::connected_component_by_vertex: expected undirected graph, got directed/);

eval { $g4->connected_component_by_index };
like($@, qr/Graph::connected_component_by_index: expected undirected graph, got directed/);

eval { $g4->same_connected_components };
like($@, qr/Graph::same_connected_components: expected undirected graph, got directed/);

eval { $g4->connected_graph };
like($@, qr/Graph::connected_graph: expected undirected graph, got directed/);

my $g5 = Graph::Undirected->new;

eval { $g5->is_weakly_connected };
like($@, qr/Graph::is_weakly_connected: expected directed graph, got undirected/);

eval { $g5->weakly_connected_components };
like($@, qr/Graph::weakly_connected_components: expected directed graph, got undirected/);

eval { $g5->weakly_connected_component_by_vertex };
like($@, qr/Graph::weakly_connected_component_by_vertex: expected directed graph, got undirected/);

eval { $g5->weakly_connected_component_by_index };
like($@, qr/Graph::weakly_connected_component_by_index: expected directed graph, got undirected/);

eval { $g5->same_weakly_connected_components };
like($@, qr/Graph::same_weakly_connected_components: expected directed graph, got undirected/);

eval { $g5->weakly_connected_graph };
like($@, qr/Graph::weakly_connected_graph: expected directed graph, got undirected/);
