package Graph::AdjacencyMatrix;

use strict;
use warnings;

use Graph::BitMatrix;
use Graph::Matrix;

use base 'Graph::BitMatrix';

use Graph::AdjacencyMap qw(:flags :fields);

sub _AM () { 0 }
sub _DM () { 1 }
sub _V () { 2 } # Graph::_V
sub _E () { 3 } # Graph::_E

sub new {
    my ($class, $g, %opt) = @_;
    my @V = $g->vertices;
    my $want_distance = delete $opt{distance_matrix};
    my $d = Graph::_defattr();
    if (exists $opt{attribute_name}) {
	$d = delete $opt{attribute_name};
	$want_distance++;
    }
    my $want_transitive = delete $opt{is_transitive};
    Graph::_opt_unknown(\%opt);
    my $m = Graph::BitMatrix->new($g);
    my $self = bless [ $m, undef, \@V ], $class;
    return $self if !$want_distance;
    my $n = $self->[ _DM ] = Graph::Matrix->new($g);
    $n->set($_, $_, 0) for @V;
    my $n0 = $n->[0];
    my $n1 = $n->[1];
    my $undirected = $g->is_undirected;
    my $multiedged = $g->multiedged;
    for my $e ($g->edges) {
	my ($u, $v) = @$e;
        $n->set($u, $v, $multiedged
	    ? _multiedged_distances($g, $u, $v, $d)
	    : $g->get_edge_attribute($u, $v, $d)
        );
        $n->set($v, $u, $multiedged
	    ? _multiedged_distances($g, $v, $u, $d)
	    : $g->get_edge_attribute($v, $u, $d)
        ) if $undirected;
    }
    $self;
}

sub _multiedged_distances {
    my ($g, $u, $v, $attr) = @_;
    my %r;
    for my $id ($g->get_multiedge_ids($u, $v)) {
	my $w = $g->get_edge_attribute_by_id($u, $v, $id, $attr);
	$r{$id} = $w if defined $w;
    }
    keys %r ? \%r : undef;
}

sub adjacency_matrix { $_[0]->[ _AM ] }

sub distance_matrix { $_[0]->[ _DM ] }

sub vertices { @{ $_[0]->[ _V ] } }

sub is_adjacent {
    my ($m, $u, $v) = @_;
    $m->[ _AM ]->get($u, $v) ? 1 : 0;
}

sub distance {
    my ($m, $u, $v) = @_;
    defined $m->[ _DM ] ? $m->[ _DM ]->get($u, $v) : undef;
}

1;
__END__
=pod

=head1 NAME

Graph::AdjacencyMatrix - create and query the adjacency matrix of graph G

=head1 SYNOPSIS

    use Graph::AdjacencyMatrix;
    use Graph::Directed; # or Undirected

    my $g  = Graph::Directed->new;
    $g->add_...(); # build $g

    my $am = Graph::AdjacencyMatrix->new($g);
    $am->is_adjacent($u, $v)

    my $am = Graph::AdjacencyMatrix->new($g, distance_matrix => 1);
    $am->distance($u, $v)

    my $am = Graph::AdjacencyMatrix->new($g, attribute_name => 'length');
    $am->distance($u, $v)

    my $am = Graph::AdjacencyMatrix->new($g, ...);
    my @V  = $am->vertices();

    $g = Graph->new(multiedged => 1);
    $g->add_...(); # build $g
    $am = Graph::AdjacencyMatrix->new($g, distance_matrix => 1);
    $am->distance($u, $v) # returns hash-ref of ID => distance

=head1 DESCRIPTION

You can use C<Graph::AdjacencyMatrix> to compute the adjacency matrix
and optionally also the distance matrix of a graph, and after that
query the adjacencyness between vertices by using the C<is_adjacent()>
method, or query the distance between vertices by using the
C<distance()> method.

By default the edge attribute used for distance is C<w>, but you
can change that in new(), see below.

If you modify the graph after creating the adjacency matrix of it,
the adjacency matrix and the distance matrix may become invalid.

=head1 Methods

=head2 Class Methods

=over 4

=item new($g)

Construct the adjacency matrix of the graph $g.

=item new($g, options)

Construct the adjacency matrix of the graph $g with options as a hash.
The known options are

=over 8

=item distance_matrix => boolean

By default only the adjacency matrix is computed.  To compute also the
distance matrix, use the attribute C<distance_matrix> with a true value
to the new() constructor.

=item attribute_name => attribute_name

By default the edge attribute used for distance is C<w>.  You can
change that by giving another attribute name with the C<attribute_name>
attribute to new() constructor.  Using this attribute also implicitly
causes the distance matrix to be computed.

=back

=back

=head2 Object Methods

=over 4

=item is_adjacent($u, $v)

Return true if the vertex $v is adjacent to vertex $u, or false if not.

=item distance($u, $v)

Return the distance between the vertices $u and $v, or C<undef> if
the vertices are not adjacent.

If the underlying graph is multiedged, returns hash-ref of ID mapped
to distance. If a given edge ID does not have the attribute defined,
it will not be represented. If no edge IDs have the attribute, C<undef>
will be returned.

=item adjacency_matrix

Return the adjacency matrix itself (a list of bitvector scalars).

=item vertices

Return the list of vertices (useful for indexing the adjacency matrix).

=back

=head1 ALGORITHM

The algorithm used to create the matrix is two nested loops, which is
O(V**2) in time, and the returned matrices are O(V**2) in space.

=head1 SEE ALSO

L<Graph::TransitiveClosure>, L<Graph::BitMatrix>

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
