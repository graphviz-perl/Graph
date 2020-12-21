package Graph::AdjacencyMap::Vertex;

# THIS IS INTERNAL IMPLEMENTATION ONLY, NOT TO BE USED DIRECTLY.
# THE INTERFACE IS HARD TO USE AND GOING TO STAY THAT WAY AND
# ALMOST GUARANTEED TO CHANGE OR GO AWAY IN FUTURE RELEASES.

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

use Graph::AdjacencyMap qw(:flags :fields);
use base 'Graph::AdjacencyMap';

sub stringify {
    require Graph::AdjacencyMap::Heavy;
    goto &Graph::AdjacencyMap::Heavy::stringify;
}

sub __set_path_node {
    my ($m, $p, $l, @args) = @_;
    my $f = $m->[ _f ];
    my $id = $_[-1] if ($f & _MULTI);
    my $arity = $m->[ _arity ];
    return $m->_inc_node( \$p->[-1]->{ $l }, $id ) if exists $p->[-1]->{ $l };
    my $i = $m->_new_node( \$p->[-1]->{ $l }, $id );
    die "undefined index" if !defined $i;
    $m->[ _i ][ $i ] = [ @args[0..$arity-1] ];
    $id;
}

sub get_ids_by_paths {
    my ($m, $list) = @_;
    my @n;
    map !(@n = $m->__get_path_node(@$_)) ? () : ref $n[0] ? $n[0]->[ _ni ] : $n[0], @$list;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    return unless my ($n, $p, $k, $l) = $m->__get_path_node( $from );
    $m->[ _i ][ ref $n ? $n->[ _ni ] : $n ] = [ $to ];
    $p->[ -1 ]{ $to } = delete $p->[ -1 ]{ $l };
    return 1;
}

1;
