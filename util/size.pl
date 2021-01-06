use lib 'lib';
use strict; use warnings;
use Graph;
use Devel::Size qw(size total_size);

my $N = 65536;

my $fmt = "%5s %8s %9s\n";
my $fmr = "%5d %8d %9.1f\n";

for ([0, 0], [0, 1], [1, 0], [1, 1]) {
    my ($countv, $counte) = @$_;
    my %args = (
	countvertexed => $countv,
	countedged => $counte,
    );
    printf $fmt, "V", "S", "S/N";
    my $g0 = Graph->new(%args);
    my $s0 = total_size($g0);
    printf $fmr, 0, $s0, 0;
    my $vr;
    for (my $n = 1; $n <= $N; $n *= 16) {
	my $g0 = Graph->new(%args);
	$g0->add_vertices(1..$n);
	my $s = total_size($g0);
	$vr = ($s - $s0) / $n;
	printf $fmr, $n, $s, $vr;
    }
    printf "Vertices(countvertexed=$countv) / MB = %8.1f\n", 1048576/$vr;

    printf $fmt, "E", "S", "S/N";
    my $g1 = Graph->new;
    printf $fmr, 0, $s0, 0;

    my $er;
    for (my $n = 1; $n <= $N; $n *= 16) {
	my $g1 = Graph->new(%args);
	$g1->add_edges(map [0, $_], 1..$n);
	my $s = total_size($g1);
	$er = ($s - $s0 - $n * $vr) / $n;
	printf $fmr, $n, $s, $er;
    }
    printf "Edges(countedged=$counte) / MB = %8.1f\n", 1048576/$er;
}
