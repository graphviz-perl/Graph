use strict; use warnings;
use Test::More tests => 36;
	
use Graph;

my %EXPECT = (
    "0 0" => "a=b",
    "0 1" => "a=b,c=d=e",
    "1 0" => "a=b",
    "1 1" => "a=b,c=d=e",
);

for my $m (0, 1) {
    for my $h (0, 1) {
	my $g = Graph->new(countedged => $m,
			   hyperedged => $h, directed => 0);
	note "c = $m, h = $h\n";
	$g->add_edge("a", "b");
	$g->add_edge("a", "b");
	if ($g->hyperedged) {
	    $g->add_edge("c", "d", "e");
	    $g->add_edge("c", "d", "e");
	}
	for (1, 2) {
	    ok(  $g->has_vertices( ) );
	    is $g, $EXPECT{"$m $h"};
	}
	for (1, 2) {
	    is( $g->get_edge_count("a", "b"),      $m ? 2 : 1 );
	    if ($g->hyperedged) {
		is( $g->get_edge_count("c", "d", "e"), $m ? 2 : 1 );
	    }
	    is( $g->get_edge_count("e", "f"),      0 );
	}
    }
}
