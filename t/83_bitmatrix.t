use strict; use warnings;
use Test::More tests => 29;

use Graph;
use Graph::BitMatrix;

my $g = Graph->new;

$g->add_edge(qw(e a));
$g->add_edge(qw(a b));
$g->add_edge(qw(b c));
$g->add_edge(qw(b d));
$g->add_edge(qw(d d));
$g->delete_vertex('e');

my $m = Graph::BitMatrix->new($g);

ok(!$m->get(qw(a a)) );
ok( $m->get(qw(a b)) );
ok(!$m->get(qw(a c)) );
ok(!$m->get(qw(a d)) );

ok(!$m->get(qw(b a)) );
ok(!$m->get(qw(b b)) );
ok( $m->get(qw(b c)) );
ok( $m->get(qw(b d)) );

ok(!$m->get(qw(c a)) );
ok(!$m->get(qw(c b)) );
ok(!$m->get(qw(c c)) );
ok(!$m->get(qw(c d)) );

ok(!$m->get(qw(d a)) );
ok(!$m->get(qw(d b)) );
ok(!$m->get(qw(d c)) );
ok( $m->get(qw(d d)) );

$m->set(qw(c c));
ok( $m->get(qw(c c)) );

$m->unset(qw(c c));
ok(!$m->get(qw(c c)) );

is("@{[$m->get_row(qw(a a b c d))]}", "0 1 0 0");
is("@{[$m->get_row(qw(b a b c d))]}", "0 0 1 1");
is("@{[$m->get_row(qw(c a b c d))]}", "0 0 0 0");
is("@{[$m->get_row(qw(d a b c d))]}", "0 0 0 1");

is $m->stringify, <<'EOF';
 to:    a    b    c    d
   a    0    1    0    0
   b    0    0    1    1
   c    0    0    0    0
   d    0    0    0    1
EOF

is scalar Graph::BitMatrix->new($g, transpose => 1)->stringify, <<'EOF';
 to:    a    b    c    d
   a    0    0    0    0
   b    1    0    0    0
   c    0    1    0    0
   d    0    1    0    1
EOF

is( $m->get(qw(x x)), undef );

is("@{[sort $m->vertices]}", "a b c d");

$m->set_row(qw(b a c));
is("@{[$m->get_row(qw(b a b c d))]}", "1 0 1 1");
$m->unset_row(qw(b c d));
is("@{[$m->get_row(qw(b a b c d))]}", "1 0 0 0");

eval { Graph::BitMatrix->new($g, nonesuch => 1) };
like($@, qr/Graph::BitMatrix::new: Unknown option: 'nonesuch' /);
