use strict; use warnings;
use Test::More;

use Graph::Directed;
use Graph::Undirected;

sub test_graphs {
  my ($graphs, $methods, $label) = @_;
  for my $m (sort keys %$methods) {
    my $this_m = $methods->{$m};
    for my $k (sort keys %$this_m) {
      my $g = $graphs->{$k};
      my $gs = $g->stringify;
      for my $call ( @{ $this_m->{$k} } ) {
	my ($arg, $expected) = @$call;
	my @args = split ' ', $arg;
	is "@{[sort $g->$m(@args)]}", $expected, "$label $k($gs) $m (@args)";
      }
    }
  }
}

sub make_graphs {
    my ($spec, $class, $l) = @_;
    +{ map {
	my ($V, $E) = @{ $spec->{$_} };
	my $g = $class->new;
	$g->add_vertices(@$V);
	$g->add_edge(@$_) for @$E;
	($l.$_ => $g);
    } keys %$spec };
}

my %V_E = (
    0 => [ [], [] ],
    1 => [ [qw(a)], [] ],
    '2a' => [ [qw(a b)], [] ],
    '2b' => [ [], [[qw(a b)]] ],
    '2c' => [ [], [[qw(a b)], [qw(b a)]] ],
    3 => [ [], [[qw(a b)], [qw(a c)], [qw(b d)], [qw(b e)], [qw(c f)], [qw(c g)]] ],
    4 => [ [], [[qw(a b)], [qw(b a)], [qw(a a)]] ],
    5 => [ [], [[qw(a a)]] ],
);

{
    my $dg = make_graphs(\%V_E, 'Graph::Directed', 'd');
    is $dg->{$_->[0]}, $_->[1], $_->[0] for (
	[ d0 => "" ],
	[ d1 => "a" ],
	[ d2a => "a,b" ],
	[ d2b => "a-b" ],
	[ d2c => "a-b,b-a" ],
	[ d3 => "a-b,a-c,b-d,b-e,c-f,c-g" ],
	[ d4 => "a-a,a-b,b-a" ],
	[ d5 => "a-a" ],
    );
    test_graphs($dg, {
	all_successors => {
	    d0 => [ ['a', ""] ],
	    d1 => [ ['a', ""] ],
	    d2a => [ ['a', ""], ['b', ""] ],
	    d2b => [ ['a', "b"], ['b', ""] ],
	    d2c => [ ['a', "a b"], ['b', "a b"] ],
	    d3 => [ ['a', "b c d e f g"], ['b', "d e"], ['c', "f g"], ['d', ""], ['e', ""], ['f', ""], ['g', ""] ],
	    d4 => [ ['a', "a b"], ['b', "a b"] ],
	    d5 => [ ['a', "a"] ],
	},
	all_predecessors => {
	    d0 => [ ['a', ""] ],
	    d1 => [ ['a', ""] ],
	    d2a => [ ['a', ""], ['b', ""] ],
	    d2b => [ ['a', ""], ['b', "a"] ],
	    d2c => [ ['a', "a b"], ['b', "a b"] ],
	    d3 => [ ['a', ""], ['b', "a"], ['c', "a"], ['d', "a b"], ['e', "a b"], ['f', "a c"], ['g', "a c"] ],
	    d4 => [ ['a', "a b"], ['b', "a b"] ],
	    d5 => [ ['a', "a"] ],
	},
	predecessors_by_radius => {
	    d0 => [ ['a 1', ""] ],
	    d1 => [ ['a 1', ""] ],
	    d2a => [ ['a 1', ""], ['b 1', ""] ],
	    d2b => [ ['a 1', ""], ['b 1', "a"], ['b 2', "a"] ],
	    d2c => [ ['a 0', ""], ['b 1', "a"], ['b 2', "a b"] ],
	    d3 => [ ['a 1', ""], ['b 1', "a"], ['c 2', "a"], ['d 1', "b"], ['d 2', "a b"], ['e 1', "b"], ['f 1', "c"], ['g 1', "c"], ['g 2', "a c"] ],
	    d4 => [ ['a 1', "a b"], ['b 1', "a"] ],
	    d5 => [ ['a 1', "a"] ],
	},
	all_neighbors => {
	    d0 => [ ['a', ""] ],
	    d1 => [ ['a', ""] ],
	    d2a => [ ['a', ""], ['b', ""] ],
	    d2b => [ ['a', "b"], ['b', "a"] ],
	    d2c => [ ['a', "b"], ['b', "a"] ],
	    d3 => [ ['a', "b c d e f g"], ['b', "a c d e f g"], ['c', "a b d e f g"], ['d', "a b c e f g"], ['e', "a b c d f g"], ['f', "a b c d e g"], ['g', "a b c d e f"] ],
	    d4 => [ ['a', "a b"], ['b', "a"] ],
	    d5 => [ ['a', "a"] ],
	},
	all_reachable => {
	    d0 => [ ['a', ""] ],
	    d1 => [ ['a', ""] ],
	    d2a => [ ['a', ""], ['b', ""] ],
	    d2b => [ ['a', "b"], ['b', ""] ],
	    d2c => [ ['a', "a b"], ['b', "a b"] ],
	    d3 => [ ['a', "b c d e f g"], ['b', "d e"], ['c', "f g"], ['d', ""], ['e', ""], ['f', ""], ['g', ""] ],
	    d4 => [ ['a', "a b"], ['b', "a b"] ],
	    d5 => [ ['a', "a"] ],
	},
    }, 'directed');
}

{
    my $dg = make_graphs(\%V_E, 'Graph::Undirected', 'u');
    is $dg->{$_->[0]}, $_->[1], $_->[0] for (
	[ u0 => "" ],
	[ u1 => "a" ],
	[ u2a => "a,b" ],
	[ u2b => "a=b" ],
	[ u2c => "a=b" ],
	[ u3 => "a=b,a=c,b=d,b=e,c=f,c=g" ],
	[ u4 => "a=a,a=b" ],
	[ u5 => "a=a" ],
    );
    test_graphs($dg, {
	all_neighbors => {
	    u0 => [ ['a', ""] ],
	    u1 => [ ['a', ""] ],
	    u2a => [ ['a', ""], ['b', ""] ],
	    u2b => [ ['a', "b"], ['b', "a"] ],
	    u2c => [ ['a', "b"], ['b', "a"] ],
	    u3 => [ ['a', "b c d e f g"], ['b', "a c d e f g"], ['c', "a b d e f g"], ['d', "a b c e f g"], ['e', "a b c d f g"], ['f', "a b c d e g"], ['g', "a b c d e f"] ],
	    u4 => [ ['a', "a b"], ['b', "a"] ],
	    u5 => [ ['a', "a"] ],
	},
	all_reachable => {
	    u0 => [ ['a', ""] ],
	    u1 => [ ['a', ""] ],
	    u2a => [ ['a', ""], ['b', ""] ],
	    u2b => [ ['a', "b"], ['b', "a"] ],
	    u2c => [ ['a', "b"], ['b', "a"] ],
	    u3 => [ ['a', "b c d e f g"], ['b', "a c d e f g"], ['c', "a b d e f g"], ['d', "a b c e f g"], ['e', "a b c d f g"], ['f', "a b c d e g"], ['g', "a b c d e f"] ],
	    u4 => [ ['a', "a b"], ['b', "a"] ],
	    u5 => [ ['a', "a"] ],
	},
    }, 'undirected');
}

{
    my $d0  = Graph::Directed->new;
    $d0->add_edge(0,1);
    $d0->add_edge(1,0);
    my @g = sort $d0->all_successors(0);
    is_deeply \@g, [ 0, 1 ],
      'all_successors works on false names' or diag explain \@g;
}

done_testing;
