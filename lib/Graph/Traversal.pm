package Graph::Traversal;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

sub reset {
    my $self = shift;
    require Set::Object;
    $self->{ unseen } = Set::Object->new($self->{ graph }->vertices);
    $self->{ seen   } = Set::Object->new;
    $self->{ order     } = [ ];
    $self->{ preorder  } = [ ];
    $self->{ postorder } = [ ];
    $self->{ roots     } = [ ];
    $self->{ tree      } = Graph->new(directed => $self->{ graph }->directed);
    delete $self->{ terminate };
}

sub _see {
    my $self = shift;
    $self->see;
}

sub has_a_cycle {
    my ($u, $v, $t, $s) = @_;
    $s->{ has_a_cycle } = 1;
    $t->terminate;
}

sub find_a_cycle {
    my ($u, $v, $t, $s) = @_;
    my @cycle = ( $u );
    push @cycle, $v unless $u eq $v;
    my $path  = $t->{ order };
    if (@$path) {
	my $i = $#$path;
	while ($i >= 0 && $path->[ $i ] ne $v) { $i-- }
	if ($i >= 0) {
	    unshift @cycle, @{ $path }[ $i+1 .. $#$path ];
	}
    }
    $s->{ a_cycle } = \@cycle;
    $t->terminate;
}

my @KNOWN_CONFIG = qw(
     tree_edge seen_edge
     next_alphabetic next_numeric next_random
     has_a_cycle find_a_cycle
);
my @EXTRACT_CONFIG = qw(
    pre post pre_vertex post_vertex
    pre_edge post_edge back_edge down_edge cross_edge non_tree_edge
    first_root next_root next_successor
);

sub new {
    my ($class, $g, %attr) = @_;
    Graph::__carp_confess("Graph::Traversal: first argument is not a Graph")
	unless ref $g && $g->isa('Graph');
    my $self = bless { graph => $g, state => { } }, $class;
    $self->reset;
    if (exists $attr{ start }) {
	$attr{ first_root } = delete $attr{ start };
	$attr{ next_root  } = undef;
    }
    my @found_known = grep exists $attr{$_}, @EXTRACT_CONFIG;
    @$self{@found_known} = delete @attr{@found_known};
    $self->{ seen_edge } = $attr{ seen_edge }
	if exists $attr{ seen_edge } and ($g->multiedged || $g->countedged);
    $self->{ pre_edge } = $attr{ tree_edge } if exists $attr{ tree_edge };
    my $default_next =
	$attr{ next_alphabetic } ? \&Graph::_next_alphabetic :
	$attr{ next_numeric } ? \&Graph::_next_numeric :
	\&Graph::_next_random;
    $self->{ next_root } = $default_next if !exists $self->{ next_root };
    $self->{ first_root } =
	exists $self->{ next_root } ? $self->{ next_root } : $default_next
	if !exists $self->{ first_root };
    $self->{ next_successor } = $default_next if !exists $self->{ next_successor };
    if (exists $attr{ has_a_cycle }) {
	$self->{ back_edge } = my $has_a_cycle =
	    ref $attr{ has_a_cycle } eq 'CODE' ?
		$attr{ has_a_cycle } : \&has_a_cycle;
	$self->{ down_edge } = $has_a_cycle if $g->is_undirected;
    }
    if (exists $attr{ find_a_cycle }) {
	$self->{ back_edge } = my $find_a_cycle =
	    ref $attr{ find_a_cycle } eq 'CODE' ?
		$attr{ find_a_cycle } : \&find_a_cycle;
	$self->{ down_edge } = $find_a_cycle if $g->is_undirected;
    }
    $self->{ add } = \&add_order;
    $self->{ see } = \&_see;
    delete @attr{@KNOWN_CONFIG};
    Graph::_opt_unknown(\%attr);
    return $self;
}

sub terminate {
    my $self = shift;
    $self->{ terminate } = 1;
}

sub add_order {
    my ($self, @next) = @_;
    push @{ $self->{ order } }, @next;
}

sub visit {
    my ($self, @next) = @_;
    $self->{ unseen }->remove(@next);
    $self->{ seen }->insert(@next);
    $self->{ add }->( $self, @next );
    return unless my $p = $self->{ pre };
    $p->( $_, $self ) for @next;
}

sub visit_preorder {
    my ($self, @next) = @_;
    push @{ $self->{ preorder } }, @next;
    $self->{ preordern }->{ $_ } = $self->{ preorderi }++ for @next;
    $self->visit( @next );
}

sub visit_postorder {
    my ($self) = @_;
    my @post = reverse $self->{ see }->( $self );
    push @{ $self->{ postorder } }, @post;
    $self->{ postordern }->{ $_ } = $self->{ postorderi }++ for @post;
    if (my $p = $self->{ post }) {
	$p->( $_, $self ) for @post;
    }
    return unless (my $p = $self->{ post_edge }) and defined(my $u = $self->current);
    $p->( $u, $_, $self, $self->{ state }) for @post;
}

sub _callbacks {
    my ($self, $current, @all) = @_;
    return unless @all;
    my $nontree  = $self->{ non_tree_edge };
    my $back     = $self->{ back_edge };
    my $down     = $self->{ down_edge };
    my $cross    = $self->{ cross_edge };
    my $seen     = $self->{ seen_edge };
    my $bdc = defined $back || defined $down || defined $cross;
    return unless (defined $nontree || $bdc || defined $seen);
    my $u = $current;
    my $preu  = $self->{ preordern  }->{ $u };
    my $postu = $self->{ postordern }->{ $u };
    for my $v ( @all ) {
	if (!$self->{tree}->has_edge($u, $v) && (defined $nontree || $bdc) &&
		exists $self->{ seen }->{ $v }) {
	    $nontree->( $u, $v, $self, $self->{ state }) if $nontree;
	    if ($bdc) {
		my $postv = $self->{ postordern }->{ $v };
		if ($back &&
		    (!defined $postv || $postv >= $postu)) {
		    $back ->( $u, $v, $self, $self->{ state });
		} else {
		    my $prev = $self->{ preordern }->{ $v };
		    if ($down && $prev > $preu) {
			$down ->( $u, $v, $self, $self->{ state });
		    } elsif ($cross && $prev < $preu) {
			$cross->( $u, $v, $self, $self->{ state });
		    }
		}
	    }
	}
	next if !$seen;
	my $c = $self->graph->get_edge_count($u, $v);
	$seen->( $u, $v, $self, $self->{ state } ) while $c-- > 1;
    }
}

sub next {
    my $self = shift;
    return undef if $self->{ terminate };
    my @next;
    while ($self->seeing) {
	my $current = $self->current;
	my $next = Set::Object->new($self->{ graph }->successors($current));
	my @all = $next->members;
	$next = $next->difference($self->{seen});
	if ($next->size) {
	    @next = $self->{ next_successor }->( $self, { map +($_=>$_), $next->members } );
	    $self->{ tree }->add_edges(map [$current, $_], @next);
	    last unless my $p = $self->{ pre_edge };
	    $p->($current, $_, $self, $self->{ state }) for @next;
	    last;
	} else {
	    $self->visit_postorder;
	}
	return undef if $self->{ terminate };
	$self->_callbacks($current, @all);
    }
    unless (@next) {
	if (!@{ $self->{ roots } } and defined(my $first = $self->{ first_root })) {
	    return unless @next = ref $first eq 'CODE'
		? $first->( $self, { map +($_=>$_), $self->unseen } )
		: $first;
	}
	return if !@next and !$self->{ next_root };
	return if !@next and !(@next = $self->{ next_root }->( $self, { map +($_=>$_), $self->unseen } ));
	return if $self->{ seen }->contains($next[0]); # Sanity check.
	push @{ $self->{ roots } }, $next[0];
    }
    $self->visit_preorder( @next ) if @next;
    return $next[0];
}

sub _order {
    my ($self, $order) = @_;
    1 while defined $self->next;
    @{ $self->{ $order } };
}

sub preorder {
    my $self = shift;
    $self->_order( 'preorder' );
}

sub postorder {
    my $self = shift;
    $self->_order( 'postorder' );
}

sub unseen {
    my $self = shift;
    $self->{ unseen }->${ wantarray ? \'members' : \'size' };
}

sub seen {
    my $self = shift;
    $self->{ seen }->${ wantarray ? \'members' : \'size' };
}

sub seeing {
    my $self = shift;
    @{ $self->{ order } };
}

sub roots {
    my $self = shift;
    @{ $self->{ roots } };
}

sub is_root {
    my ($self, $v) = @_;
    for my $u (@{ $self->{ roots } }) {
	return 1 if $u eq $v;
    }
    return 0;
}

sub tree {
    my $self = shift;
    $self->{ tree };
}

sub graph {
    my $self = shift;
    $self->{ graph };
}

sub vertex_by_postorder {
    my ($self, $i) = @_;
    exists $self->{ postorder } && $self->{ postorder }->[ $i ];
}

sub postorder_by_vertex {
    my ($self, $v) = @_;
    exists $self->{ postordern } && $self->{ postordern }->{ $v };
}

sub postorder_vertices {
    my ($self, $v) = @_;
    exists $self->{ postordern } ? %{ $self->{ postordern } } : ();
}

sub vertex_by_preorder {
    my ($self, $i) = @_;
    exists $self->{ preorder } && $self->{ preorder }->[ $i ];
}

sub preorder_by_vertex {
    my ($self, $v) = @_;
    exists $self->{ preordern } && $self->{ preordern }->{ $v };
}

sub preorder_vertices {
    my ($self, $v) = @_;
    exists $self->{ preordern } ? %{ $self->{ preordern } } : ();
}

sub has_state {
    my ($self, $var) = @_;
    exists $self->{ state } && exists $self->{ state }->{ $var };
}

sub get_state {
    my ($self, $var) = @_;
    exists $self->{ state } ? $self->{ state }->{ $var } : undef;
}

sub set_state {
    my ($self, $var, $val) = @_;
    $self->{ state }->{ $var } = $val;
    return 1;
}

sub delete_state {
    my ($self, $var) = @_;
    delete $self->{ state }->{ $var };
    delete $self->{ state } unless keys %{ $self->{ state } };
    return 1;
}

1;
__END__
=pod

=head1 NAME

Graph::Traversal - traverse graphs

=head1 SYNOPSIS

Don't use Graph::Traversal directly, use Graph::Traversal::DFS
or Graph::Traversal::BFS instead.

    use Graph;
    my $g = Graph->new;
    $g->add_edge(...);
    use Graph::Traversal::...;
    my $t = Graph::Traversal::...->new($g, %opt);
    $t->...

=head1 DESCRIPTION

You can control how the graph is traversed by the various callback
parameters in the C<%opt>.  In the parameters descriptions below the
$u and $v are vertices, and the $self is the traversal object itself.

=head2 Callback parameters

The following callback parameters are available:

=over 4

=item tree_edge

Called when traversing an edge that belongs to the traversal tree.
Called with arguments ($u, $v, $self).

=item non_tree_edge

Called when an edge is met which either leads back to the traversal tree
(either a C<back_edge>, a C<down_edge>, or a C<cross_edge>).
Called with arguments ($u, $v, $self).

=item pre_edge

Called for edges in preorder.
Called with arguments ($u, $v, $self).

=item post_edge

Called for edges in postorder.
Called with arguments ($u, $v, $self).

=item back_edge

Called for back edges.
Called with arguments ($u, $v, $self).

=item down_edge

Called for down edges.
Called with arguments ($u, $v, $self).

=item cross_edge

Called for cross edges.
Called with arguments ($u, $v, $self).

=item pre

=item pre_vertex

Called for vertices in preorder.
Called with arguments ($v, $self).

=item post

=item post_vertex

Called for vertices in postorder.
Called with arguments ($v, $self).

=item first_root

Called when choosing the first root (start) vertex for traversal.
Called with arguments ($self, $unseen) where $unseen is a hash
reference with the unseen vertices as keys.

=item next_root

Called when choosing the next root (after the first one) vertex for
traversal (useful when the graph is not connected).  Called with
arguments ($self, $unseen) where $unseen is a hash reference with
the unseen vertices as keys.  If you want only the first reachable
subgraph to be processed, set the next_root to C<undef>.

=item start

Identical to defining C<first_root> and undefining C<next_root>.

=item next_alphabetic

Set this to true if you want the vertices to be processed in
alphabetic order (and leave first_root/next_root undefined).

=item next_numeric

Set this to true if you want the vertices to be processed in
numeric order (and leave first_root/next_root undefined).

=item next_successor

Called when choosing the next vertex to visit.  Called with arguments
($self, $next) where $next is a hash reference with the possible
next vertices as keys.  Use this to provide a custom ordering for
choosing vertices, as opposed to C<next_numeric> or C<next_alphabetic>.

=back

The parameters C<first_root> and C<next_successor> have a 'hierarchy'
of how they are determined: if they have been explicitly defined, use
that value.  If not, use the value of C<next_alphabetic>, if that has
been defined.  If not, use the value of C<next_numeric>, if that has
been defined.  If not, the next vertex to be visited is chosen randomly.

=head2 Methods

The following methods are available:

=over 4

=item unseen

Return the unseen vertices in random order.

=item seen

Return the seen vertices in random order.

=item seeing

Return the active fringe vertices in random order.

=item preorder

Return the vertices in preorder traversal order.

=item postorder

Return the vertices in postorder traversal order.

=item vertex_by_preorder

    $v = $t->vertex_by_preorder($i)

Return the ith (0..$V-1) vertex by preorder.

=item preorder_by_vertex

    $i = $t->preorder_by_vertex($v)

Return the preorder index (0..$V-1) by vertex.

=item vertex_by_postorder

    $v = $t->vertex_by_postorder($i)

Return the ith (0..$V-1) vertex by postorder.

=item postorder_by_vertex

    $i = $t->postorder_by_vertex($v)

Return the postorder index (0..$V-1) by vertex.

=item preorder_vertices

Return a hash with the vertices as the keys and their preorder indices
as the values.

=item postorder_vertices

Return a hash with the vertices as the keys and their postorder
indices as the values.

=item tree

Return the traversal tree as a graph.

=item has_state

    $t->has_state('s')

Test whether the traversal has state 's' attached to it.

=item get_state

    $t->get_state('s')

Get the state 's' attached to the traversal (C<undef> if none).

=item set_state

    $t->set_state('s', $s)

Set the state 's' attached to the traversal.

=item delete_state

    $t->delete_state('s')

Delete the state 's' from the traversal.

=back

=head2 Special callbacks

If in a callback you call the special C<terminate> method,
the traversal is terminated, no more vertices are traversed.

=head1 SEE ALSO

L<Graph::Traversal::DFS>, L<Graph::Traversal::BFS>

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
