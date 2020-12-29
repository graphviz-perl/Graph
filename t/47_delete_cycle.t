use strict; use warnings;
use Test::More tests => 2;

use Graph;
my $g = Graph->new;

$g->add_cycle("a", "b", "c", "d", "e");
$g->delete_cycle("a", "b", "c");
is $g, "c-d,d-e,e-a,b";

my $h = Graph->new(undirected => 1);
$h->add_cycle("a", "b", "c", "d", "e");
$h->delete_cycle("a", "b", "c");
is $h, "a=e,c=d,d=e,b";
