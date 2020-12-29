use strict; use warnings;
use Test::More tests => 2;

use Graph;

my $g = Graph->new;
$g->add_cycle("a", "b", "c");
is $g, "a-b,b-c,c-a";

my $h = Graph->new(undirected => 1);
$h->add_cycle("a", "b", "c");
is $h, "a=b,a=c,b=c";
