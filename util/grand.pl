use strict; use warnings;
use Graph;
use Time::HiRes qw(time);
use Getopt::Long;

my %OPT = (seed => 42, test => 'apsp', fill => 0.50, V => 20, directed => 1, unionfind => 0);
my %TEST2METHOD = (
    apsp => 'APSP_Floyd_Warshall',
    mstk => 'MST_Kruskal',
    mstp => 'MST_Prim',
    sptd => 'SPT_Dijkstra',
    sptb => 'SPT_Bellman_Ford',
    cc => 'connected_components',
    bcc => 'biconnected_components',
    scc => 'strongly_connected_components',
    succ => sub { my $g = shift; $g->successors($_) for $g->vertices },
    ef => sub { my $g = shift; $g->edges_from($_) for $g->vertices },
    ea => sub { my $g = shift; $g->edges_at($_) for $g->vertices },
);
my %WTEST; @WTEST{qw(apsp mstp mstk sptd sptb)} = ();
my %UTEST; @UTEST{qw(mstk mstp cc bcc)} = ();
my %DTEST; @DTEST{qw(scc)} = ();

sub usage {
    die <<__EOF__;
$0: Usage: $0 [--seed=n]
              [--test=@{[join('|', sort keys %TEST2METHOD)]}]
              [--directed=d] [--fill=f] [V]
Default values:@{[ map qq{\n$_ = $OPT{$_}}, sort keys %OPT ]}
__EOF__
}

$| = 1;
usage() unless GetOptions(
    'seed=n'		=> \$OPT{seed},
    'test=s'		=> \$OPT{test},
    'directed=n'	=> \$OPT{directed},
    'fill=f'		=> \$OPT{fill},
    'uf=n'		=> \$OPT{unionfind},
);
$OPT{V} = shift if @ARGV;
usage() if @ARGV;
usage() unless $TEST2METHOD{$OPT{test}};

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
my $g = Graph->new(map +($_ => $OPT{$_}), qw(directed unionfind));
my $e = $E;
my (%v1_v2, @edges);
my $t0_edge = time();
while (1) {
    my $u = int(rand($OPT{V}));
    my $v = int(rand($OPT{V}));
    if ($u ne $v && !exists $v1_v2{$u}{$v}) {
        push @edges, [$u, $v];
        $v1_v2{$u}{$v} = undef;
	last unless --$e;
    }
}

if (exists $WTEST{$OPT{test}}) {
    push @$_, rand() for @edges;
    $g->add_weighted_edges(map @$_, @edges);
} else {
    $g->add_edges(@edges);
}
my $t1_edge = time();
printf "%d vertices, %d edges - set up %.2f\n", $OPT{V}, $E, $t1_edge - $t0_edge;

my $t0 = time();
my ($u0, $s0) = times();
() = $g->${ \$TEST2METHOD{$OPT{test}} };
my $t1 = time();
my ($u1, $s1) = times();

my $u = $u1 - $u0;
my $s = $s1 - $s0;
my $c = $u + $s;

printf "real %.2f user %.2f system %.2f cpu %.2f\n", $t1 - $t0, $u, $s, $c;

exit(0);
