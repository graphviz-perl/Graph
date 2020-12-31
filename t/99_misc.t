use strict; use warnings;

use Test::More;

use Graph::Directed;
use Graph::Undirected;

my @E = ([qw(a b)], [qw(a c)], [qw(b d)], [qw(b e)], [qw(c f)], [qw(c g)]);

{
    my $gi0 = Graph->new;
    $gi0->set_edge_attribute(qw(a b), weight => 1);
    my $gi1 = Graph->new;
    $gi1->set_vertex_attribute('x', shape => 1);
    $gi1->set_edge_attribute(qw(x y), weight => 2);
    is_deeply [ $gi0->ingest($gi1)->as_hashes ], [
        { x => { shape => 1 }, map +($_ => {}), qw(a b y) },
        { a => { b => { weight => 1 } }, x => { y => { weight => 2 } } },
    ];
}

for ({}, {countvertexed => 1}, {multivertexed => 1}) {
  my $gr = Graph::Directed->new(%$_);
  $gr->add_edge(@$_) for @E;
  $gr->rename_vertex('b', 'b1');
  my $label = ref($gr->[ 2 ]) . ' {' . join('=>', %$_) . '}';
  is $gr, "a-b1,a-c,b1-d,b1-e,c-f,c-g", $label;
  $gr->rename_vertices(sub { uc $_[0] });
  is $gr, "A-B1,A-C,B1-D,B1-E,C-F,C-G", $label;
}

my $g2 = Graph->new;
is_deeply [ $g2->clustering_coefficient ], [],
  'clustering_coefficient with no vertices = empty list';

for my $p (qw(zero
	      one
	      two
	      three
	      four
	      five
	      six
	      seven
	      eight
	      nine
	      ten)) {
    $g2->add_path(split(//, $p));
}

my ($gamma, %clustering) = $g2->clustering_coefficient;

my $eps = 1e-6;

ok(abs($gamma - 0.402222222222222) <= $eps);
ok(abs($clustering{e} - 0.7) <= $eps);
ok(abs($clustering{t} - 1/3) <= $eps);
is($clustering{z}, 0.0);
is($clustering{r}, 1.0);

my %betweenness = $g2->betweenness;

ok(abs($betweenness{e} - 60.3333333333333) <= $eps);
ok(abs($betweenness{t} - 17.1666666666667) <= $eps);
is($betweenness{x}, 0.0);
is($betweenness{u}, 3.0);

{
my $w = '';
local $SIG{__WARN__} = sub { $w = shift };
my $g3 = Graph->new;
$g3->add_edge(0,1);
my @dummy = $g3->SP_Dijkstra(1,0);
is $w, '';
}

is_deeply [ sort(Graph::__fisher_yates_shuffle(1..3)) ], [ 1..3 ];

done_testing;
