use strict; use warnings;
use Test::More tests => 2;

use Graph;

my $g = Graph->new;
$g->add_path("a", "b", "c"); # @todo: hyperedges
is $g, "a-b,b-c";

my $h = Graph->new(undirected => 1); # @todo: hyperedges
$h->add_path("a", "b", "c");
is $h, "a=b,b=c";
