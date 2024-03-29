use strict; use warnings;
use Graph;

use Test::More;

my $N = 5;

sub prettyn {
    join('; ',
	 map { qq[@$_] }
	      sort { my @a = @$a; my @b = @$b;
		     my $c = @b <=> @a; return $c if $c;
		     while (@a && @b) {
			 $c = (shift @a) <=> (shift @b); return $c if $c;
		     }
		     return @a - @b }
	           map { [ sort { $a <=> $b } @$_ ] }
	 @{ $_[0] });
}

sub prettya {
    join('; ',
	 map { qq[@$_] }
	      sort { my @a = @$a; my @b = @$b;
		     my $c = @b <=> @a; return $c if $c;
		     while (@a && @b) {
			 $c = (shift @a) cmp (shift @b); return $c if $c;
		     }
		     return @a - @b }
	           map { [ sort @$_ ] }
	 @{ $_[0] });
}

my $g0a = Graph->new(undirected => 1);

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0a->biconnectivity;
    is("@{[sort { $a <=> $b } @$ap? @$ap : ()]}", "");
    is("@{[prettyn($bc)]}", "");
    is("@{[prettyn($br)]}", "");
}

ok(!$g0a->is_biconnected);
is( $g0a->is_edge_connected, undef);
is( $g0a->is_edge_separable, undef);
is("@{[sort { $a <=> $b } $g0a->articulation_points]}", "");
is("@{[prettyn([$g0a->biconnected_components])]}", "");
is("@{[prettyn([$g0a->bridges])]}", "");

my $g0b = Graph->new(undirected => 1);
$g0b->add_vertex("a");

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0b->biconnectivity;
    is("@{[sort { $a <=> $b } @$ap? @$ap : ()]}", "");
    is("@{[prettyn($bc)]}", "");
    is("@{[prettyn($br)]}", "");
}

ok(!$g0b->is_biconnected);
is( $g0b->is_edge_connected, undef);
is( $g0b->is_edge_separable, undef);
is("@{[sort { $a <=> $b } $g0b->articulation_points]}", "");
is("@{[prettyn([$g0b->biconnected_components])]}", "");
is("@{[prettyn([$g0b->bridges])]}", "");

my $g0c = Graph->new(undirected => 1);
$g0c->add_edge(qw(a b));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0c->biconnectivity;
    is(@$ap, 0);
    is("@{[sort { $a <=> $b } @$ap? @$ap : ()]}", "");
    is("@{[prettya($bc)]}", "a b");
    is("@{[prettya($br)]}", "a b");
}

ok(!$g0c->is_biconnected);
is( $g0c->is_edge_connected, undef);
is( $g0c->is_edge_separable, undef);
is("@{[sort { $a <=> $b } $g0c->articulation_points]}", "");
is("@{[prettya([$g0c->biconnected_components])]}", "a b");
is("@{[prettya([$g0c->bridges])]}", "a b");

my $g0d = Graph->new(undirected => 1);
$g0d->add_edge(qw(a b));
$g0d->add_edge(qw(b c));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0d->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b");
    is("@{[prettya($bc)]}", "a b; b c");
    is("@{[prettya($br)]}", "a b; b c");
}

ok(!$g0d->is_biconnected);
ok(!$g0d->is_edge_connected);
ok( $g0d->is_edge_separable);
is("@{[sort $g0d->articulation_points]}", "b");
is("@{[prettya([$g0d->biconnected_components])]}", "a b; b c");
is("@{[prettya([$g0d->bridges])]}", "a b; b c");

my $g0e = Graph->new(undirected => 1);
$g0e->add_edge(qw(a b));
$g0e->add_edge(qw(b c));
$g0e->add_edge(qw(c d));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0e->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b c");
    is("@{[prettya($bc)]}", "a b; b c; c d");
    is("@{[prettya($br)]}", "a b; b c; c d");
}

ok(!$g0e->is_biconnected);
ok(!$g0e->is_edge_connected);
ok( $g0e->is_edge_separable);
is("@{[sort $g0e->articulation_points]}", "b c");
is("@{[prettya([$g0e->biconnected_components])]}", "a b; b c; c d");
is("@{[prettya([$g0e->bridges])]}", "a b; b c; c d");

my $g0f = Graph->new(undirected => 1);

$g0f->add_cycle(qw(a b c));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0f->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "");
    is("@{[prettya($bc)]}", "a b c");
    is("@{[prettya($br)]}", "");
}

ok( $g0f->is_biconnected);
ok( $g0f->is_edge_connected);
ok(!$g0f->is_edge_separable);
is("@{[sort $g0f->articulation_points]}", "");
is("@{[prettya([$g0f->biconnected_components])]}", "a b c");
is("@{[prettya([$g0f->bridges])]}", "");

my $g0g = Graph->new(undirected => 1);

$g0g->add_cycle(qw(a b c d));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0g->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "");
    is("@{[prettya($bc)]}", "a b c d");
    is("@{[prettya($br)]}", "");
}

ok( $g0g->is_biconnected);
ok( $g0g->is_edge_connected);
ok(!$g0g->is_edge_separable);
is("@{[sort $g0g->articulation_points]}", "");
is("@{[prettya([$g0g->biconnected_components])]}", "a b c d");
is("@{[prettya([$g0g->bridges])]}", "");

my $g0h = Graph->new(undirected => 1);

$g0h->add_cycle(qw(a b c));
$g0h->add_edge(qw(b d));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0h->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b");
    is("@{[prettya($bc)]}", "a b c; b d");
    is("@{[prettya($br)]}", "b d");
}

ok(!$g0h->is_biconnected);
ok(!$g0h->is_edge_connected);
ok( $g0h->is_edge_separable);
is("@{[sort $g0h->articulation_points]}", "b");
is("@{[prettya([$g0h->biconnected_components])]}", "a b c; b d");
is("@{[prettya([$g0h->bridges])]}", "b d");

my $g0i = Graph->new(undirected => 1);

$g0i->add_cycle(qw(a b c));
$g0i->add_edge(qw(b d));
$g0i->add_edge(qw(d e));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0i->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b d");
    is("@{[prettya($bc)]}", "a b c; b d; d e");
    is("@{[prettya($br)]}", "b d; d e");
}

ok(!$g0i->is_biconnected);
ok(!$g0i->is_edge_connected);
ok( $g0i->is_edge_separable);
is("@{[sort $g0i->articulation_points]}", "b d");
is("@{[prettya([$g0i->biconnected_components])]}", "a b c; b d; d e");
is("@{[prettya([$g0i->bridges])]}", "b d; d e");

my $g0j = Graph->new(undirected => 1);

$g0j->add_cycle(qw(a b c));
$g0j->add_cycle(qw(b d e));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0j->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b");
    is("@{[prettya($bc)]}", "a b c; b d e");
    is("@{[prettya($br)]}", "");
}

ok(!$g0j->is_biconnected);
ok( $g0j->is_edge_connected);
ok(!$g0j->is_edge_separable);
is("@{[sort $g0j->articulation_points]}", "b");
is("@{[prettya([$g0j->biconnected_components])]}", "a b c; b d e");
is("@{[prettya([$g0j->bridges])]}", "");

my $g0k = Graph->new(undirected => 1);

$g0k->add_cycle(qw(a b c));
$g0k->add_cycle(qw(d e f));
$g0k->add_edge(qw(b d));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0k->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b d");
    is("@{[prettya($bc)]}", "a b c; d e f; b d");
    is("@{[prettya($br)]}", "b d");
}

ok(!$g0k->is_biconnected);
ok(!$g0k->is_edge_connected);
ok( $g0k->is_edge_separable);
is("@{[sort $g0k->articulation_points]}", "b d");
is("@{[prettya([$g0k->biconnected_components])]}", "a b c; d e f; b d");
is("@{[prettya([$g0k->bridges])]}", "b d");

my $g0l = Graph->new(undirected => 1);

$g0l->add_cycle(qw(a b c));
$g0l->add_cycle(qw(d e f));
$g0l->add_cycle(qw(g h i));
$g0l->add_edge(qw(b d));
$g0l->add_edge(qw(d g));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0l->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b d g");
    is("@{[prettya($bc)]}", "a b c; d e f; g h i; b d; d g");
    is("@{[prettya($br)]}", "b d; d g");
}

ok(!$g0l->is_biconnected);
ok(!$g0l->is_edge_connected);
ok( $g0l->is_edge_separable);
is("@{[sort $g0l->articulation_points]}", "b d g");
is("@{[prettya([$g0l->biconnected_components])]}",
   "a b c; d e f; g h i; b d; d g");
is("@{[prettya([$g0l->bridges])]}", "b d; d g");

my $g0m = Graph->new(undirected => 1);

$g0m->add_cycle(qw(a b c));
$g0m->add_cycle(qw(b d e));
$g0m->add_cycle(qw(b h i));

for (0..$N-1) {
    my ($ap, $bc, $br) = $g0m->biconnectivity;
    is("@{[sort @$ap? @$ap : ()]}", "b");
    is("@{[prettya($bc)]}", "a b c; b d e; b h i");
    is("@{[prettya($br)]}", "");
}

ok(!$g0m->is_biconnected);
ok( $g0m->is_edge_connected);
ok(!$g0m->is_edge_separable);
is("@{[sort $g0m->articulation_points]}", "b");
is("@{[prettya([$g0m->biconnected_components])]}", "a b c; b d e; b h i");
is("@{[prettya([$g0m->bridges])]}", "");

is("@{[sort $g0m->cut_vertices]}", "b");

my $g1 = Graph->new(undirected => 1);

$g1->add_cycle(qw(0 1 2 6));
$g1->add_cycle(qw(7 8 10));
$g1->add_cycle(qw(3 4 5));
$g1->add_cycle(qw(4 9 11));
$g1->add_edge(qw(11 12));
$g1->add_edge(qw(0 5));
$g1->add_edge(qw(6 7));

for (0..2*$N-1) {
    my ($ap, $bc, $br) = $g1->biconnectivity;
    is("@{[sort { $a <=> $b } @$ap]}", "0 4 5 6 7 11");
    is("@{[prettyn($bc)]}", "0 1 2 6; 3 4 5; 4 9 11; 7 8 10; 0 5; 6 7; 11 12");
    is("@{[prettyn($br)]}", "0 5; 6 7; 11 12");
}

my $g2 = Graph->new(undirected => 1);

$g2->add_cycle(qw(a b c));
$g2->add_cycle(qw(d e f));
$g2->add_cycle(qw(f g h));
$g2->add_edge(qw(c d));
$g2->add_edge(qw(h i));
$g2->add_edge(qw(i j));
$g2->add_edge(qw(j k));

for (0..2*$N-1) {
    my ($ap, $bc, $br) = $g2->biconnectivity;
    is("@{[sort @$ap]}", "c d f h i j");
    is("@{[prettya($bc)]}", "a b c; d e f; f g h; c d; h i; i j; j k");
    is("@{[prettya($br)]}", "c d; h i; i j; j k");
}

my $g3 = Graph->new(undirected => 1);

$g3->add_path(qw(s a e i k j i));
$g3->add_path(qw(s b a f e));
$g3->add_path(qw(b f));
$g3->add_path(qw(s c g d h l));
$g3->add_path(qw(s d));
$g3->add_path(qw(c h));

for (0..2*$N-1) {
    my ($ap, $bc, $br) = $g3->biconnectivity;
    is("@{[sort @$ap]}", "e h i s");
    is("@{[prettya($bc)]}", "a b e f s; c d g h s; i j k; e i; h l");
    is("@{[prettya($br)]}", "e i; h l");
}

is( $g3->biconnected_components, 5 );

my @c0 = map $g3->biconnected_component_by_index(0), 1..5;
my @c1 = map $g3->biconnected_component_by_index(1), 1..5;
my @c2 = map $g3->biconnected_component_by_index(2), 1..5;

is( "@{$c0[0]}", "@{$c0[$_]}" ) for 1..4;
is( "@{$c1[0]}", "@{$c1[$_]}" ) for 1..4;
is( "@{$c2[0]}", "@{$c2[$_]}" ) for 1..4;

isnt( "@{$c0[0]}", "@{$c1[0]}" );
isnt( "@{$c0[0]}", "@{$c2[0]}" );

is( $g3->biconnected_component_by_index(5), undef );

my $g3c = $g3->biconnected_graph();

is( $g3c, "a+b+e+f+s=c+d+g+h+s,a+b+e+f+s=e+i,c+d+g+h+s=h+l,e+i=i+j+k");

ok( $g3->same_biconnected_components('a', 'b') );
is( $g3->biconnected_component_by_vertex('a'), $g3->biconnected_component_by_vertex('b') );
ok( $g3->same_biconnected_components('a', 'b', 'e') );
ok(!$g3->same_biconnected_components('a', 'c') );
ok(!$g3->same_biconnected_components('a', 'b', 'c') );

is("@{[sort @{ $g3c->get_vertex_attribute('a+b+e+f+s', 'subvertices') }]}", "a b e f s");
is("@{[sort @{ $g3c->get_vertex_attribute('i+j+k', 'subvertices') }]}", "i j k");
is($g3c->get_vertex_attribute('i+k+j', 'subvertices'), undef);

my $g4 = Graph->new(undirected => 1, edges => [[qw(a b)], [qw(a c)], [qw(a d)]]);
# edges coloured by what comp they're in - >1 colour on vertex = in >1 comp
my @component_colours = do { no warnings 'qw'; qw(#FF0000 #00FF00 #0000FF #FFFF00 #FF00FF #00FFFF) };
sub _bicon_graphvizify {
    my ($g) = @_;
    my ($gc, @bc) = ($g->copy, $g->biconnected_components);
    require Set::Object;
    for my $ci (0..$#bc) {
	for my $v (@{ $bc[$ci] }) {
	    $gc->set_vertex_attribute($v, 'components', (my $bcs = $gc->get_vertex_attribute($v, 'components') || Set::Object->new));
	    $bcs->insert($ci);
	}
    }
    $gc->set_graph_attribute(graphviz => { groups => [
	map +{
	    attributes => {
		subgraph => { pencolor => $component_colours[ $_ % @component_colours ] },
		name => "cluster_$_",
	    },
	    nodes => $bc[$_],
	}, 0..$#bc
    ]});
    for my $u ($g->vertices) {
	my $comps_u = $gc->get_vertex_attribute($u, 'components');
	for my $v ($g->successors($u)) {
	    my $comps_v = $gc->get_vertex_attribute($v, 'components');
	    my $colour = $component_colours[ ($comps_u->intersection($comps_v)->members)[0] % @component_colours ];
	    $gc->set_edge_attribute($u, $v, graphviz => { color => $colour });
	}
    }
    $gc;
}
#require GraphViz2;
#open my $fh, '>', 'bicon.dot';
#my $g_gv = _bicon_graphvizify($g3);
#print $fh GraphViz2->from_graph($g_gv)->dot_input;
ok !$g4->same_biconnected_components(qw(a b c));

eval { Graph->new->biconnectivity };
like($@, qr/Graph::biconnectivity: expected undirected graph, got directed/);

done_testing;
