use strict; use warnings;
use Test::More tests => 13;

use Graph;
my $g = Graph->new;

$g->add_cycle("a", "b", "c", "d", "e");
is $g, "a-b,b-c,c-d,d-e,e-a";

ok(   $g->has_cycle(qw(a b c d e)) );
ok( ! $g->has_cycle(qw(a c b d e)) );
ok( ! $g->has_cycle(qw(b a c d e)) );
ok(   $g->has_cycle(qw(e a b c d)) );
ok( ! $g->has_cycle(qw(e d a b c)) );

$g->delete_cycle("a", "b", "c");
is $g, "c-d,d-e,e-a,b";

my $h = Graph->new(undirected => 1);
$h->add_cycle("a", "b", "c", "d", "e");
is $h, "a=b,a=e,b=c,c=d,d=e";
ok(   $h->has_cycle(qw(a b c d e)) );
ok(   $h->has_cycle(qw(e a b c d)) );
ok( ! $h->has_cycle(qw(a b d c e)) );

$h->delete_cycle("a", "b", "c");
is $h, "a=e,c=d,d=e,b";

ok(!  $g->has_cycle());
