package Graph::TransitiveClosure;

use strict;
use warnings;

# COMMENT THESE OUT FOR TESTING AND PRODUCTION.
# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

use base 'Graph';
use Graph::TransitiveClosure::Matrix;

sub _G () { Graph::_G() }

sub new {
    my ($class, $g, %opt) = @_;
    Graph::__carp_confess(__PACKAGE__."->new given non-Graph '$g'")
	if !(ref $g and $g->isa('Graph'));
    %opt = (path_vertices => 1) unless %opt;
    my $attr = Graph::_defattr();
    if (exists $opt{ attribute_name }) {
	$attr = $opt{ attribute_name };
	# No delete $opt{ attribute_name } since we need to pass it on.
    }
    $opt{ reflexive } = 1 unless exists $opt{ reflexive };
    my $tcg = $g->new(
	multiedged => 0,
	($opt{ reflexive } ? (vertices => [$g->vertices]) : ()),
    );
    my $tcm = $g->_check_cache('transitive_closure_matrix', [],
	\&_transitive_closure_matrix_compute, %opt);
    my $tcm00 = $tcm->[0][0]; # 0=am, 0=bitmatrix
    my $tcm01 = $tcm->[0][1]; #     , 1=hash mapping v-name to the offset into dm data structures (in retval of $g->vertices)
    for my $u ($tcm->vertices) {
	my $tcm00i = $tcm00->[ $tcm01->{ $u } ];
	for my $v ($tcm->vertices) {
	    next if $u eq $v && ! $opt{ reflexive };
	    my $j = $tcm01->{ $v };
	    $tcg->add_edge($u, $v) if vec($tcm00i, $j, 1);
				      # $tcm->is_transitive($u, $v)
				      # $tcm->[0]->get($u, $v)
	}
    }
    $tcg->set_graph_attribute('_tcm', [ $g->[ _G ], $tcm ]);
    bless $tcg, $class;
}

sub _transitive_closure_matrix_compute {
    Graph::TransitiveClosure::Matrix->new(@_);
}

sub is_transitive {
    my $g = shift;
    $g->expect_no_args(@_);
    Graph::TransitiveClosure::Matrix::is_transitive($g);
}

sub transitive_closure_matrix {
    $_[0]->get_graph_attribute('_tcm')->[1];
}

1;
__END__
=pod

=head1 NAME

Graph::TransitiveClosure - create and query transitive closure of graph

=head1 SYNOPSIS

    use Graph::TransitiveClosure;
    use Graph::Directed; # or Undirected

    my $g  = Graph::Directed->new;
    $g->add_...(); # build $g

    # Compute the transitive closure graph.
    my $tcg = Graph::TransitiveClosure->new($g);
    $tcg->is_reachable($u, $v) # Identical to $tcg->has_edge($u, $v)

    # Being reflexive is the default, meaning that null transitions
    # (transitions from a vertex to the same vertex) are included.
    my $tcg = Graph::TransitiveClosure->new($g, reflexive => 1);
    my $tcg = Graph::TransitiveClosure->new($g, reflexive => 0);

    # is_reachable(u, v) is always reflexive.
    $tcg->is_reachable($u, $v)

    # You can check any graph for transitivity.
    $g->is_transitive()

    my $tcg = Graph::TransitiveClosure->new($g, path_length => 1);
    $tcg->path_length($u, $v)

    # path_vertices is on by default so this is a no-op.
    my $tcg = Graph::TransitiveClosure->new($g, path_vertices => 1);
    $tcg->path_vertices($u, $v)

    # see how many paths exist from $u to $v
    my $tcg = Graph::TransitiveClosure->new($g, path_count => 1);
    $tcg->path_length($u, $v)

    # Both path_length and path_vertices.
    my $tcg = Graph::TransitiveClosure->new($g, path => 1);
    $tcg->path_vertices($u, $v)
    $tcg->length($u, $v)

    my $tcg = Graph::TransitiveClosure->new($g, attribute_name => 'length');
    $tcg->path_length($u, $v)

=head1 DESCRIPTION

You can use C<Graph::TransitiveClosure> to compute the transitive
closure graph of a graph and optionally also the minimum paths
(lengths and vertices) between vertices, and after that query the
transitiveness between vertices by using the C<is_reachable()> and
C<is_transitive()> methods, and the paths by using the
C<path_length()> and C<path_vertices()> methods.

For further documentation, see the L<Graph::TransitiveClosure::Matrix>.

=head2 Class Methods

=over 4

=item new($g, %opt)

Construct a new transitive closure object.  Note that strictly speaking
the returned object is not a graph; it is a graph plus other stuff.  But
you should be able to use it as a graph plus a couple of methods inherited
from the Graph::TransitiveClosure::Matrix class.

=back

=head2 Object Methods

These are only the methods 'native' to the class: see
L<Graph::TransitiveClosure::Matrix> for more.

=over 4

=item is_transitive($g)

Return true if the Graph $g is transitive.

=item transitive_closure_matrix

Return the transitive closure matrix of the transitive closure object.

=back

=cut
