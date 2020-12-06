use strict; use warnings;
use Test::More tests => 56;
	
use Graph;

for my $m (0, 1) {
    for my $r (0, 1) {
	    my $g = Graph->new(countvertexed => $m,
			       refvertexed   => $r);
	    note "countvertexed = $m, refvertexed = $r";
	    $g->add_vertex("a");
	    $g->add_vertex("a");
	    $g->add_vertex(my $b = []);
	    $g->add_vertex($b);
	    for (1, 2) {
		ok(  $g->has_vertices( ) );
		ok(  $g->has_vertex("a") );
		ok(  $g->has_vertex($b ) );
		ok( !$g->has_vertex("e") );
		is( $g->get_vertex_count("a"),      $m ? 2 : 1 );
		is( $g->get_vertex_count($b ),      $m ? 2 : 1 );
		is( $g->get_vertex_count("e"),      0 );
	    }
    }
}
