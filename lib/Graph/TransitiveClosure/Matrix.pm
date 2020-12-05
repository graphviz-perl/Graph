package Graph::TransitiveClosure::Matrix;

use strict;
use warnings;

use Graph::AdjacencyMatrix;
use Graph::Matrix;
use Scalar::Util qw(weaken);
use List::Util qw(min);

sub _A() { 0 } # adjacency
sub _D() { 1 } # distance
sub _P() { 2 } # predecessors
sub _V() { 3 } # vertices
sub _G() { 4 } # the original graph (OG)

sub _new {
    my ($g, $class, $am_opt, $want_transitive, $want_reflexive, $want_path, $want_path_vertices, $want_path_count) = @_;
    my $m = Graph::AdjacencyMatrix->new($g, %$am_opt);
    my @V = $g->vertices;
    my %v2i; @v2i{ @V } = 0..$#V; # paths are in array -> stable ordering
    my $am = $m->adjacency_matrix;
    my $dm; # The distance matrix.
    my $pm; # The predecessor matrix.
    my @di;
    my @ai = @{ $am->[0] };
    my @pi;
    my $multi = $g->multiedged;
    unless ($want_transitive) {
	$dm = $m->distance_matrix;
	$dm = $m->[ Graph::AdjacencyMatrix::_DM ] = Graph::Matrix->new($g)
	    if !defined $dm; # if no distance_matrix in AM, we make our own
	if ($want_path_count) {
	    # force defined
	    @di = map [ (0) x @V ], 0..$#V;
	} else {
	    @di = @{ $dm->[0] };
	}
	$pm = Graph::Matrix->new($g);
	@pi = @{ $pm->[0] };
	for (my $iu = $#V; $iu >= 0; $iu--) {
	    for (my $iv = $#V; $iv >= 0; $iv--) {
		next unless
		    # $am->get($u, $v)
		    vec($ai[$iu], $iv, 1)
			;
		if ($want_path_count or
		    !defined $di[$iu][$iv] # $dm->get($u, $v)
		) {
		    # $dm->set($u, $v, $u eq $v ? 0 : 1)
		    $di[$iu][$iv] = $iu == $iv ? 0 : 1;
		} elsif ($multi and ref($di[$iu][$iv]) eq 'HASH') {
		    $di[$iu][$iv] = min values %{ $di[$iu][$iv] };
		}
		$pi[$iu]->[$iv] = $V[$iv] unless $iu == $iv;
	    }
	}
    }
    # XXX (see the bits below): sometimes, being nice and clean is the
    # wrong thing to do.  In this case, using the public API for graph
    # transitive matrices and bitmatrices makes things awfully slow.
    # Instead, we go straight for the jugular of the data structures.
    for (my $iu = $#V; $iu >= 0; $iu--) {
	my $didiu = $di[$iu];
	my $aiaiu = $ai[$iu];
	for (my $iv = $#V; $iv >= 0; $iv--) {
	    my $didiv = $di[$iv];
	    my $aiaiv = $ai[$iv];
	    if (
		# $am->get($v, $u)
		vec($aiaiv, $iu, 1)
		|| ($want_reflexive && $iu == $iv)) {
		my $aivivo = $aiaiv;
		if ($want_transitive) {
		    if ($want_reflexive) {
			for (my $iw = $#V; $iw >= 0; $iw--) {
			    next if $iw == $iu;
			    return 0
				if  vec($aiaiu, $iw, 1) &&
				   !vec($aiaiv, $iw, 1);
			}
			# See XXX above.
			# for my $w (@V) {
			#    my $aiw = $ai{$w};
			#    if (
			#	# $am->get($u, $w)
			#	vec($aiaiu, $aiw, 1)
			#	|| ($u eq $w)) {
			#	return 0
			#	    if $u ne $w &&
			#		# !$am->get($v, $w)
			#		!vec($aiaiv, $aiw, 1)
			#		    ;
			#	# $am->set($v, $w)
			#	vec($aiaiv, $aiw, 1) = 1
			#	    ;
			#     }
			# }
		    } else {
			# See XXX above.
			# for my $w (@V) {
			#     my $aiw = $ai{$w};
			#     if (
			#	# $am->get($u, $w)
			#	vec($aiaiu, $aiw, 1)
			#       ) {
			#	return 0
			#	    if $u ne $w &&
			#		# !$am->get($v, $w)
			#		!vec($aiaiv, $aiw, 1)
			#		    ;
			# 	# $am->set($v, $w)
			# 	vec($aiaiv, $aiw, 1) = 1
			# 	    ;
			#     }
			# }
			$aiaiv |= $aiaiu; # uncoverable statement
		    }
		} else {
		    $aiaiv |= $aiaiu;
		    vec($aiaiv, $iu, 1) = 1 if $want_reflexive;
		}
		if ($aiaiv ne $aivivo) {
		    $ai[$iv] = $aiaiv;
		    $aiaiu = $aiaiv if $iu == $iv;
		}
	    }
						   # $am->get($v, $u)
	    if ($want_path && !$want_transitive && vec($aiaiv, $iu, 1)) {
		for (my $iw = $#V; $iw >= 0; $iw--) {
		    next unless
			# See XXX above.
			# $am->get($u, $w)
			vec($aiaiu, $iw, 1)
			    ;
		    if ($want_path_count) {
			$didiv->[$iw]++ if $iw != $iu and $iw != $iv and $iu != $iv;
			next;
		    }
		    # See XXX above.
		    # $d0  = $dm->get($v, $w);
		    # $d1a = $dm->get($v, $u) || 1;
		    # $d1b = $dm->get($u, $w) || 1;
		    my $d0  = $didiv->[$iw];
		    # no override sum-zero paths which can happen with negative weights
		    my $d1a = $didiv->[$iu];
		    $d1a = 1 unless defined $d1a;
		    my $d1b = $didiu->[$iw];
		    $d1b = 1 unless defined $d1b;
		    my $d1 = $d1a + $d1b;
		    if (!defined $d0 || ($d1 < $d0)) {
			# print "d1 = $d1a ($v, $u) + $d1b ($u, $w) = $d1 ($v, $w) (".(defined$d0?$d0:"-").")\n";
			# See XXX above.
			# $dm->set($v, $w, $d1);
			$didiv->[$iw] = $d1;
			$pi[$iv]->[$iw] = $pi[$iv]->[$iu]
			    if $want_path_vertices;
		    }
		}
		# $dm->set($u, $v, 1)
		$didiu->[$iv] = 1
		    if $iu != $iv &&
		       # $am->get($u, $v)
		       vec($aiaiu, $iv, 1)
			   &&
		       # !defined $dm->get($u, $v);
		       !defined $didiu->[$iv];
	    }
	}
    }
    return 1 if $want_transitive;
    my %V; @V{ @V } = @V;
    $am->[0] = \@ai;
    $am->[1] = \%v2i;
    if (defined $dm) {
	$dm->[0] = \@di;
	$dm->[1] = \%v2i;
    }
    if (defined $pm) {
	$pm->[0] = \@pi;
	$pm->[1] = \%v2i;
    }
    weaken(my $og = $g);
    bless [ $am, $dm, $pm, \%V, $og ], $class;
}

sub new {
    my ($class, $g, %opt) = @_;
    my %am_opt = (distance_matrix => 1);
    $am_opt{attribute_name} = delete $opt{attribute_name}
	if exists $opt{attribute_name};
    $am_opt{distance_matrix} = delete $opt{distance_matrix}
	if $opt{distance_matrix};
    $opt{path_length} = $opt{path_vertices} = delete $opt{path}
	if exists $opt{path};
    my $want_path_length = delete $opt{path_length};
    my $want_path_count = delete $opt{path_count};
    my $want_path_vertices = delete $opt{path_vertices};
    my $want_reflexive = delete $opt{reflexive};
    $am_opt{is_transitive} = my $want_transitive = delete $opt{is_transitive}
	if exists $opt{is_transitive};
    die "Graph::TransitiveClosure::Matrix::new: Unknown options: @{[map { qq['$_' => $opt{$_}]} keys %opt]}"
	if keys %opt;
    $want_reflexive = 1 unless defined $want_reflexive;
    my $want_path = $want_path_length || $want_path_vertices || $want_path_count;
    # $g->expect_dag if $want_path;
    $am_opt{distance_matrix} = 0 if $want_path_count;
    _new($g, $class,
	 \%am_opt,
	 $want_transitive, $want_reflexive,
	 $want_path, $want_path_vertices, $want_path_count);
}

sub has_vertices {
    my $tc = shift;
    for my $v (@_) {
	return 0 unless exists $tc->[ _V ]->{ $v };
    }
    return 1;
}

sub is_reachable {
    my ($tc, $u, $v) = @_;
    return undef unless $tc->has_vertices($u, $v);
    return 1 if $u eq $v;
    $tc->[ _A ]->get($u, $v);
}

sub is_transitive {
    return __PACKAGE__->new($_[0], is_transitive => 1) if @_ == 1; # Any graph
    # A TC graph
    my ($tc, $u, $v) = @_;
    return undef unless $tc->has_vertices($u, $v);
    $tc->[ _A ]->get($u, $v);
}

sub vertices {
    my $tc = shift;
    values %{ $tc->[3] };
}

sub path_length {
    my ($tc, $u, $v) = @_;
    return undef unless $tc->has_vertices($u, $v);
    return 0 if $u eq $v;
    $tc->[ _D ]->get($u, $v);
}

sub path_predecessor {
    my ($tc, $u, $v) = @_;
    return undef if $u eq $v;
    return undef unless $tc->has_vertices($u, $v);
    $tc->[ _P ]->get($u, $v);
}

sub path_vertices {
    my ($tc, $u, $v) = @_;
    return unless $tc->is_reachable($u, $v);
    return wantarray ? () : 0 if $u eq $v;
    my @v = ( $u );
    while ($u ne $v) {
	last unless defined($u = $tc->path_predecessor($u, $v));
	push @v, $u;
    }
    $tc->[ _P ]->set($u, $v, [ @v ]) if @v;
    return @v;
}

sub all_paths {
    my ($tc, $u, $v) = @_;
    return if $u eq $v;
    my @found;
    push @found, [$u, $v] if $tc->[ _G ]->has_edge($u, $v);
    push @found,
        map [$u, @$_],
        map $tc->all_paths($_, $v),
        grep $tc->is_reachable($_, $v),
        grep $_ ne $v, $tc->[ _G ]->successors($u);
    @found;
}

1;
__END__
=pod

=head1 NAME

Graph::TransitiveClosure::Matrix - create and query transitive closure of graph

=head1 SYNOPSIS

    use Graph::TransitiveClosure::Matrix;
    use Graph::Directed; # or Undirected

    my $g  = Graph::Directed->new;
    $g->add_...(); # build $g

    # Compute the transitive closure matrix.
    my $tcm = Graph::TransitiveClosure::Matrix->new($g);

    # Being reflexive is the default,
    # meaning that null transitions are included.
    my $tcm = Graph::TransitiveClosure::Matrix->new($g, reflexive => 1);
    $tcm->is_reachable($u, $v)

    # is_reachable(u, v) is always reflexive.
    $tcm->is_reachable($u, $v)

    # The reflexivity of is_transitive(u, v) depends on the reflexivity
    # of the transitive closure.
    $tcg->is_transitive($u, $v)

    my $tcm = Graph::TransitiveClosure::Matrix->new($g, path_length => 1);
    my $n = $tcm->path_length($u, $v)

    my $tcm = Graph::TransitiveClosure::Matrix->new($g, path_vertices => 1);
    my @v = $tcm->path_vertices($u, $v)

    my $tcm =
        Graph::TransitiveClosure::Matrix->new($g,
                                              attribute_name => 'length');
    my $n = $tcm->path_length($u, $v)

    my @v = $tcm->vertices

=head1 DESCRIPTION

You can use C<Graph::TransitiveClosure::Matrix> to compute the
transitive closure matrix of a graph and optionally also the minimum
paths (lengths and vertices) between vertices, and after that query
the transitiveness between vertices by using the C<is_reachable()> and
C<is_transitive()> methods, and the paths by using the
C<path_length()> and C<path_vertices()> methods.

If you modify the graph after computing its transitive closure,
the transitive closure and minimum paths may become invalid.

=head1 Methods

=head2 Class Methods

=over 4

=item new($g)

Construct the transitive closure matrix of the graph $g.

=item new($g, options)

Construct the transitive closure matrix of the graph $g with options
as a hash. The known options are

=over 8

=item C<attribute_name> => I<attribute_name>

By default the edge attribute used for distance is C<weight>.  You can
change that by giving another attribute name with the C<attribute_name>
attribute to the new() constructor.

=item reflexive => boolean

By default the transitive closure matrix is not reflexive: that is,
the adjacency matrix has zeroes on the diagonal.  To have ones on
the diagonal, use true for the C<reflexive> option.

=item path => boolean

If set true, sets C<path_length> and C<path_vertices>. If either of
those are true (and C<path_vertices> is by default), then both are
calculated.

=item path_length => boolean

By default "false", but see above as overridden by default
C<path_vertices> being true. If calculated,
they can be retrieved using the path_length() method.

=item path_vertices => boolean

By default the paths are computed, with the boolean transitivity,
they can be retrieved using the path_vertices() method.

=item path_count => boolean

As an alternative to setting C<path_length>, if this is true then the
matrix will store the quantity of paths between the two vertices. This
is still retrieved using the path_length() method. The path vertices
will not be available. You should probably only use this on a DAG,
and not with C<reflexive>.

=back

=back

=head2 Object Methods

=over 4

=item is_reachable($u, $v)

Return true if the vertex $v is reachable from the vertex $u,
or false if not.

=item path_length($u, $v)

Return the minimum path length from the vertex $u to the vertex $v,
or undef if there is no such path.

=item path_vertices($u, $v)

Return the minimum path (as a list of vertices) from the vertex $u to
the vertex $v, or an empty list if there is no such path, OR also return
an empty list if $u equals $v.

=item has_vertices($u, $v, ...)

Return true if the transitive closure matrix has all the listed vertices,
false if not.

=item is_transitive($u, $v)

Return true if the vertex $v is transitively reachable from the vertex $u,
false if not.

=item vertices

Return the list of vertices in the transitive closure matrix.

=item path_predecessor

Return the predecessor of vertex $v in the transitive closure path
going back to vertex $u.

=item all_paths($u, $v)

Return list of array-refs with all the paths from $u to $v.

=back

=head1 RETURN VALUES

For path_length() the return value will be the sum of the appropriate
attributes on the edges of the path, C<weight> by default.  If no
attribute has been set, one (1) will be assumed.

If you try to ask about vertices not in the graph, undefs and empty
lists will be returned.

=head1 ALGORITHM

The transitive closure algorithm used is Warshall and Floyd-Warshall
for the minimum paths, which is O(V**3) in time, and the returned
matrices are O(V**2) in space.

=head1 SEE ALSO

L<Graph::AdjacencyMatrix>

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
