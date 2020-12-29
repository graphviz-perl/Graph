use strict; use warnings;
use Test::More tests => 2;

use Graph;

my $g = Graph->new;
$g->add_path("a", "b", "c", "d", "e");
$g->delete_path("a", "b", "c");
is $g, "c-d,d-e,a,b";

my $h = Graph->new(undirected => 1);
$h->add_path("a", "b", "c", "d", "e");
$h->delete_path("a", "b", "c");
is $h, "c=d,d=e,a,b";
