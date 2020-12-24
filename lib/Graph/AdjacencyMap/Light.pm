package Graph::AdjacencyMap::Light;

# THIS IS INTERNAL IMPLEMENTATION ONLY, NOT TO BE USED DIRECTLY.
# THE INTERFACE IS HARD TO USE AND GOING TO STAY THAT WAY AND
# ALMOST GUARANTEED TO CHANGE OR GO AWAY IN FUTURE RELEASES.

use strict;
use warnings;

use Graph::AdjacencyMap qw(:flags :fields);
use base 'Graph::AdjacencyMap';

use Scalar::Util qw(weaken);

sub _V () { 2 } # Graph::_V
sub _E () { 3 } # Graph::_E

sub _is_COUNT    () { 0 }
sub _is_MULTI    () { 0 }
sub _is_HYPER    () { 0 }
sub _is_UNIQ     () { 0 }
sub _is_REF      () { 0 }

sub _new {
    my ($class, $flags, $arity, $graph) = @_;
    my $m = $class->SUPER::_new($flags | _LIGHT, $arity, [], {}, {}, $graph);
    weaken $m->[ _g ]; # So that DESTROY finds us earlier.
    return $m;
}

sub stringify {
    my $m = shift;
    'Graph: ' . $m->[ _g ] . "\n" . $m->SUPER::stringify;
}

sub set_path {
    my ($m, @args) = @_;
    return if @args == 0;
    my ($n, $f, $a, $i, $s, $p) = @$m;
    @args = sort @args if ($f & _UNORD) and $a == 2;
    my $e0 = shift @args;
    return $n if exists $s->{ $e0 } && (($a == 1) or exists $s->{ $e0 }->{ $args[0] });
    $n = $m->[ _n ]++;
    $i->[ $n ] = [ $e0, @args ];
    if ($a == 2) {
	my $e1 = shift @args;
	$s->{ $e0 }->{ $e1 } = $p->{ $e1 }->{ $e0 } = $n;
    } else {
	$s->{ $e0 } = $n;
    }
    $n;
}

sub paths_non_existing {
    push @_, 0;
    goto &_paths_lookup;
}

sub _paths_lookup {
    my ($m, $list, $want_exist) = @_;
    my ($n, $f, $a, $i, $s) = @$m;
    my $unord = $a > 1 && ($f & _UNORD);
    map {
	my @p = @$_;
	@p = sort @p if $unord;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	($want_exist xor !defined $this_s) ? ($want_exist ? $this_s : $_) : ();
    } @$list;
}

sub has_path {
    my ($f, $a, $s, @args) = ( @{ $_[0] }[ _f, _arity, _s ], @_[1..$#_] );
    return 0 unless $a == @args;
    @args = sort @args if ($f & _UNORD);
    my $e;
    $s = $s->{ shift @args } while defined $s and @args;
    defined $s ? 1 : 0;
}

sub get_ids_by_paths {
    push @_, 1;
    goto &_paths_lookup;
}

sub _get_path_count {
    &has_path ? 1 : undef;
}

sub has_paths { keys %{ $_[0]->[ _s ] } }

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
    $i->[ $s->{ $to } ] = [ $to ];
    return 1;
}

sub __attr {
    # Major magic takes place here: we rebless the appropriate 'light'
    # map into a more complex map and then redispatch the method.
    # The other map types will sort @_ for _UNORD purposes.
    my ($n, $f, $a, $i, $s, $p, $g) = @{ $_[0] };
    if ($a > 1) { # Edges, then.
	my @E = $g->edges;
	$_[0] = $g->[ _E ] = Graph::AdjacencyMap->_new(($f & ~_LIGHT), 2);
	$g->add_edges( @E );
    } else {
	my @V = @{ $g->[ _V ] };
	$_[0] = $g->[ _V ] = Graph::AdjacencyMap->_new(($f & ~_LIGHT), 1);
	@{ $_[0] }[ _n, _i, _s, _p ] = @V[ _n, _i, _s, _p ];
    }
    goto &Graph::AdjacencyMap::__attr; # Redispatch.
}

1;
