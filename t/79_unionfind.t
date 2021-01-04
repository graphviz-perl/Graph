use strict; use warnings;
use Test::More;

use Graph::UnionFind;

my $uf = Graph::UnionFind->new;

is_deeply [$uf->find('a')], [undef];
$uf->add('a');
is_deeply [$uf->find('a')], ['a'];
$uf->add('b');
is_deeply [$uf->find('a')], ['a'];
is_deeply [$uf->find('b')], ['b'];

$uf->union(['a', 'b']); # http://rt.cpan.org/NoAuth/Bug.html?id=2627

is_deeply [$uf->find('a')], ['b'];
is_deeply [$uf->find('b')], ['b'];

$uf->union(['c', 'd']);

is_deeply [$uf->find('c')], ['d'];
is_deeply [$uf->find('d')], ['d'];

is_deeply [$uf->find('e')], [undef];

ok( $uf->same('a', 'b'));
ok( $uf->same('b', 'a'));
ok( $uf->same('c', 'd'));
ok(!$uf->same('a', 'c'));

$uf->union(['a', 'd']);
ok( $uf->same('a', 'c'));

ok(!$uf->same('c', 'e'));

# rt.cpan.org #39805: UnionFind: Repeated adds clobbers graph component information
my $graph = Graph::UnionFind->new;
$graph->add('a');
$graph->union(['a','b']);

ok($graph->same('a', 'b'));
ok($graph->same('b', 'a'));

$graph->add('a');

ok($graph->same('a', 'b'));
ok($graph->same('b', 'a'));

done_testing;
