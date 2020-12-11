package Graph::AdjacencyMap::Light;

# THIS IS INTERNAL IMPLEMENTATION ONLY, NOT TO BE USED DIRECTLY.
# THE INTERFACE IS HARD TO USE AND GOING TO STAY THAT WAY AND
# ALMOST GUARANTEED TO CHANGE OR GO AWAY IN FUTURE RELEASES.

use strict;
use warnings;

use Graph::AdjacencyMap qw(:flags :fields);
use base 'Graph::AdjacencyMap';

use Scalar::Util qw(weaken);
use List::Util qw(first);

sub _V () { 2 } # Graph::_V
sub _E () { 3 } # Graph::_E
sub _F () { 0 } # Graph::_F

sub _new {
    my ($class, $flags, $arity, $graph) = @_;
    my $m = $class->SUPER::_new($flags | _LIGHT, $arity, [], {}, {}, $graph);
    weaken $m->[ _g ]; # So that DESTROY finds us earlier.
    return $m;
}

sub stringify {
    my $m = shift;
    my @rows;
    my $a = $m->[ _arity ];
    if ($a == 2) {
	my @p = sort keys %{ $m->[ _s ] };
	my @s = sort keys %{ $m->[ _p ] };
	@rows = [ 'to:', @s ];
	push @rows, map { my $p=$_; [ $p, map $m->has_path($p, $_), @s ] } @p;
    } elsif ($a == 1) {
	my $s = $m->[ _s ];
	push @rows, map [ $_, $s->{ $_ } ], sort map @$_, $m->paths;
    }
    'Graph: ' . $m->[ _g ] . "\n" . $m->SUPER::stringify . join '',
	map "$_\n",
	map join(' ', map sprintf('%4s', $_), @$_),
	@rows;
}

sub set_path {
    my $m = shift;
    return if @_ == 0 && !($m->[ _f ] & _HYPER);
    my ($n, $f, $a, $i, $s, $p) = @$m;
    if ($a == 2) {
	@_ = sort @_ if ($f & _UNORD);
    }
    my $e0 = shift;
    if ($a == 2) {
	my $e1 = shift;
	unless (exists $s->{ $e0 } && exists $s->{ $e0 }->{ $e1 }) {
	    $n = $m->[ _n ]++;
	    $i->[ $n ] = [ $e0, $e1 ];
	    $s->{ $e0 }->{ $e1 } = $n;
	    $p->{ $e1 }->{ $e0 } = $n;
	}
    } else {
	unless (exists $s->{ $e0 }) {
	    $n = $m->[ _n ]++;
	    $s->{ $e0 } = $n;
	    $i->[ $n ] = $e0;
	}
    }
}

sub has_path {
    my $m = shift;
    my ($n, $f, $a, $i, $s) = @$m;
    return 0 unless $a == @_;
    my $e;
    if ($a == 2) {
	@_ = sort @_ if ($f & _UNORD);
	$e = shift;
	return 0 unless exists $s->{ $e };
        $s = $s->{ $e };
    }
    $e = shift;
    exists $s->{ $e };
}

sub _get_path_id {
    my $m = shift;
    my ($n, $f, $a, $i, $s) = @$m;
    return undef unless $a == @_;
    my $e;
    if ($a == 2) {
	@_ = sort @_ if ($f & _UNORD);
	$e = shift;
	return undef unless exists $s->{ $e };
        $s = $s->{ $e };
    }
    $e = shift;
    $s->{ $e };
}

sub _get_path_count {
    my $m = shift;
    my ($n, $f, $a, $i, $s) = @$m;
    my $e;
    if (@_ == 2) {
	@_ = sort @_ if ($f & _UNORD);
	$e = shift;
	return undef unless exists $s->{ $e };
        $s = $s->{ $e };
    }
    $e = shift;
    return exists $s->{ $e } ? 1 : 0;
}

sub has_paths { keys %{ $_[0]->[ _s ] } }

sub paths {
    my $m = shift;
    return if !defined(my $i = $m->[ _i ]);
    my ($v) = first { defined } @$i;
    return grep defined, @$i if ref $v;
    return map [ $_ ], grep defined, @$i;
}

sub get_paths_by_ids {
    my ($m, $list) = @_;
    my $i = $m->[ 3 ];
    map [ map {
	my $p = $i->[ $_ ];
	my @v = defined $p ? ( ref $p eq 'ARRAY' ? @$p : $p ) : ( );
	@v == 1 ? $v[0] : \@v
    } @$_ ], @$list;
}

sub del_path {
    my $m = shift;
    my ($n, $f, $a, $i, $s, $p) = @$m;
    @_ = sort @_ if @_ > 1 and $f & _UNORD;
    my $e0 = shift;
    return 0 if !defined($n = $s->{ $e0 });
    if (@_ == 1) {
	my $e1 = shift;
	return 0 if !defined($n = $n->{ $e1 }); # "actual" n ie id
	delete $i->[ $n ];
	delete $s->{ $e0 }->{ $e1 };
	delete $p->{ $e1 }->{ $e0 };
	delete $s->{ $e0 } unless keys %{ $s->{ $e0 } };
	delete $p->{ $e1 } unless keys %{ $p->{ $e1 } };
    } else {
	delete $i->[ $n ];
	delete $s->{ $e0 };
    }
    return 1;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    my ($n, $f, $a, $i, $s, $p) = @$m;
    return 1 if $a > 1; # arity > 1, all integers, no names
    return 0 unless exists $s->{ $from };
    $s->{ $to } = delete $s->{ $from };
    $i->[ $s->{ $to } ] = $to;
    return 1;
}

sub __attr {
    # Major magic takes place here: we rebless the appropriate 'light'
    # map into a more complex map and then redispatch the method.
    # The other map types will sort @_ for _UNORD purposes.
    my $m = $_[0];
    my ($n, $f, $a, $i, $s, $p, $g) = @$m;
    my ($v) = first { defined } @$i;
    my @V = @{ $g->[ _V ] };
    my @E = $g->edges; # TODO: Both these (ZZZ) lines are mysteriously needed!
    # ZZZ: an example of failing tests is t/52_edge_attributes.t.
    if (ref $v eq 'ARRAY') { # Edges, then.
	# print "Reedging.\n";
	@E = $g->edges; # TODO: Both these (ZZZ) lines are mysteriously needed!
	require Graph::AdjacencyMap::Heavy;
	$g->[ _E ] = $m = Graph::AdjacencyMap::Heavy->_new(($f & ~_LIGHT), 2);
	$g->add_edges( @E );
    } else {
	# print "Revertexing.\n";
	require Graph::AdjacencyMap::Vertex;
	$m = Graph::AdjacencyMap::Vertex->_new(($f & ~_LIGHT), 1);
	$m->[ _n ] = $V[ _n ];
	$m->[ _i ] = $V[ _i ];
	$m->[ _s ] = $V[ _s ];
	$m->[ _p ] = $V[ _p ];
	$g->[ _V ] = $m;
    }
    $_[0] = $m;
    goto &{ ref($m) . "::__attr" }; # Redispatch.
}

sub _is_COUNT    () { 0 }
sub _is_MULTI    () { 0 }
sub _is_HYPER    () { 0 }
sub _is_UNIQ     () { 0 }
sub _is_REF      () { 0 }

1;
