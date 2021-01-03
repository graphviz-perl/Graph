use strict; use warnings;
use Graph;
use Graph::Directed;
use Graph::Undirected;
use Time::HiRes qw(time);
use Getopt::Long;

my %OPT = (seed => 42, test => 'apsp', fill => 0.50, V => 20, directed => 1);

my @TEST = qw(apsp mstp mstk sptd sptb cc bcc scc);
my %TEST; @TEST{@TEST} = ();

my @WTEST = qw(apsp mstp mstk sptd sptb bcc);
my %WTEST; @WTEST{@WTEST} = ();

my @UTEST = qw(mstk mstp cc);
my %UTEST; @UTEST{@UTEST} = ();

my @DTEST = qw(scc);
my %DTEST; @DTEST{@DTEST} = ();

sub usage {
    die <<__EOF__;
$0: Usage: $0 [--seed=n]
              [--test=@{[join('|', @TEST)]}]
              [--directed=d] [--fill=f] [V]
Default values:@{[ map qq{\n$_ = $OPT{$_}}, sort keys %OPT ]}
__EOF__
}

usage()
  unless
    GetOptions(
	       'seed=n'		=> \$OPT{seed},
	       'test=s'		=> \$OPT{test},
	       'directed=n'	=> \$OPT{directed},
	       'fill=f'		=> \$OPT{fill},
	      );
$OPT{V} = shift if @ARGV;
usage() if @ARGV;
usage() unless exists $TEST{$OPT{test}};

print "Running $OPT{test}...\n";

srand($OPT{seed});

if (exists $UTEST{$OPT{test}} && $OPT{directed}) {
    $OPT{directed} = 0;
    print "($OPT{test} needs undirected, fixed)\n";
} elsif (exists $DTEST{$OPT{test}} && !$OPT{directed}) {
    $OPT{directed} = 1;
    print "($OPT{test} needs directed, fixed)\n";
}

if ($OPT{fill} < 0.0 || $OPT{fill} > 1.0) {
    $OPT{fill} = 0.5;
    print "($OPT{fill} must be between 0.0 and 1.0, fixed to be 0.5)\n";
}

# Thanks to Devel::DProf and List::Util breakage.
# my $g = Graph->random_graph(vertices   => $OPT{V},
#                             directed   => $OPT{directed},
#                             edges_fill => $OPT{fill});
my $E = int(($OPT{V} * ($OPT{V} - 1) * $OPT{fill}) / ($OPT{directed} ? 1 : 2));
my $g = $OPT{directed} ? Graph::Directed->new() : Graph::Undirected->new();
my $e = $E;
my (%v1_v2, @edges);
#my $t0_edge = time();
while (1) {
    my $u = int(rand($OPT{V}));
    my $v = int(rand($OPT{V}));
    if ($u ne $v && !exists $v1_v2{$u}{$v}) {
        push @edges, [$u, $v];
        $v1_v2{$u}{$v} = undef;
	last unless --$e;
    }
}
print "($OPT{V} vertices, $E edges)\n";

if (exists $WTEST{$OPT{test}}) {
    push @$_, rand() for @edges;
    $g->add_weighted_edges(map @$_, @edges);
} else {
    $g->add_edges(@edges);
}
#my $t1_edge = time();
#printf "EDGE TIME real %.2f\n", $t1_edge - $t0_edge;

my $t0 = time();
my ($u0, $s0) = times();

if ($OPT{test} eq 'apsp') {
    my $apsp = $g->APSP_Floyd_Warshall;
} elsif ($OPT{test} eq 'mstk') {
    my $mst = $g->MST_Kruskal;
} elsif ($OPT{test} eq 'mstp') {
    my $mst = $g->MST_Prim;
} elsif ($OPT{test} eq 'sptd') {
    my $mst = $g->SPT_Dijkstra;
} elsif ($OPT{test} eq 'sptb') {
    my $mst = $g->SPT_Bellman_Ford;
} elsif ($OPT{test} eq 'cc') {
    my @cc = $g->connected_components;
} elsif ($OPT{test} eq 'bcc') {
    my @bcc = $g->biconnected_components;
} elsif ($OPT{test} eq 'scc') {
    my @scc = $g->strongly_connected_components;
} else {
    die "$0: Unknown test '$OPT{test}'";
}

my $t1 = time();
my ($u1, $s1) = times();

my $u = $u1 - $u0;
my $s = $s1 - $s0;
my $c = $u + $s;

printf "real %.2f user %.2f system %.2f cpu %.2f\n", $t1 - $t0, $u, $s, $c;

exit(0);
