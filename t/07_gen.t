use strict; use warnings;
use Test::More tests => 15;

use Graph;

my $g = Graph->new;

gen_changed( $g->[1] ); # [1] is the generational index
ok( $g->add_vertex('a') );
gen_changed( $g->[1] );
ok( $g->add_vertex('b') );
gen_changed( $g->[1] );
ok( $g->add_edge('a', 'b') );
gen_changed( $g->[1] );
ok( $g->delete_edge('a', 'b') );
gen_changed( $g->[1] );
ok( $g->add_edge('a', 'c') );
gen_changed( $g->[1] );
ok( $g->delete_vertex('a') );
gen_changed( $g->[1] );
ok( $g->delete_vertex('b') );
gen_changed( $g->[1] ); # delete vertex

my $gen_old;
sub gen_changed {
  isnt $_[0], $gen_old;
  $gen_old = $_[0],;
}
