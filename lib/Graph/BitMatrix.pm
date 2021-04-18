package Graph::BitMatrix;

use strict;
use warnings;

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

sub _E () { 3 } # Graph::_E()
sub _i () { 3 } # Index to path.

sub new {
    my ($class, $g, %opt) = @_;
    my @V = $g->vertices;
    my $V = @V;
    my $Z = "\0" x (($V + 7) / 8);
    my %V; @V{ @V } = 0 .. $#V;
    my $bm = bless [ [ ( $Z ) x $V ], \%V ], $class;
    my $bm0 = $bm->[0];
    my $connect_edges = delete $opt{connect_edges};
    $connect_edges = 1 unless defined $connect_edges;
    my $transpose = delete $opt{transpose};
    Graph::_opt_unknown(\%opt);
    return $bm if !$connect_edges;
    # for (my $i = 0; $i <= $#V; $i++) {
    #    my $u = $V[$i];
    #    for (my $j = 0; $j <= $#V; $j++) {
    #	vec($bm0->[$i], $j, 1) = 1 if $g->has_edge($u, $V[$j]);
    #    }
    # }
    my $undirected = $g->is_undirected;
    for my $e ($g->edges) {
	my ($i0, $j0) = map $V{$_}, @$e;
	($j0, $i0) = ($i0, $j0) if $transpose;
	vec($bm0->[$i0], $j0, 1) = 1;
	vec($bm0->[$j0], $i0, 1) = 1 if $undirected;
    }
    $bm;
}

sub stringify {
    my ($m) = @_;
    my @V = sort keys %{ $m->[1] };
    my $top = join ' ', map sprintf('%4s', $_), 'to:', @V;
    my @indices = map $m->[1]{$_}, @V;
    my @rows;
    for my $n (@V) {
        my @vals = $m->get_row($n, @V);
        push @rows, join ' ', map sprintf('%4s', defined()?$_:''), $n, @vals;
    }
    join '', map "$_\n", $top, @rows;
}

sub set { push @_, 1; goto &_get_set }
sub unset { push @_, 0; goto &_get_set }
sub get { push @_, undef; goto &_get_set }
sub _get_set {
    my $val = pop;
    my ($m, $u, $v) = @_;
    my ($m0, $m1) = @$m[0, 1];
    return if grep !defined, my ($i, $j) = @$m1{ $u, $v };
    defined $val ? vec($m0->[$i], $j, 1) = $val : vec($m0->[$i], $j, 1);
}

sub _set_row {
    my $val = pop;
    my ($m, $u) = splice @_, 0, 2;
    my ($m0, $m1) = @$m[0, 1];
    return unless defined(my $i = $m1->{ $u });
    vec($m0->[$i], $_, 1) = $val for grep defined, @$m1{ @_ };
}
sub set_row { push @_, 1; goto &_set_row }
sub unset_row { push @_, 0; goto &_set_row }

sub get_row {
    my ($m, $u) = splice @_, 0, 2;
    my ($m0, $m1) = @$m[0, 1];
    return () x @_ unless defined(my $i = $m1->{ $u });
    map defined() ? (vec($m0->[$i], $_, 1) ? 1 : 0) : undef, @$m1{ @_ };
}

sub vertices {
    keys %{ $_[0]->[1] };
}

1;
__END__
=pod

=head1 NAME

Graph::BitMatrix - create and manipulate a V x V bit matrix of graph G

=head1 SYNOPSIS

    use Graph::BitMatrix;
    use Graph::Directed;
    my $g  = Graph::Directed->new;
    $g->add_...(); # build $g
    my $m = Graph::BitMatrix->new($g, %opt);
    $m->get($u, $v)
    $m->set($u, $v)
    $m->unset($u, $v)
    $m->get_row($u, $v1, $v2, ..., $vn)
    $m->set_row($u, $v1, $v2, ..., $vn)
    $m->unset_row($u, $v1, $v2, ..., $vn)
    $a->vertices()

=head1 DESCRIPTION

This class enables creating bit matrices that compactly describe
the connected of the graphs.

=head2 Class Methods

=over 4

=item new($g)

Create a bit matrix from a Graph $g.  The C<%opt>, if present,
can have the following options:

=over 8

=item *

connect_edges

If true or if not present, set the bits in the bit matrix that
correspond to edges.  If false, do not set any bits.  In either
case the bit matrix of V x V bits is allocated.

=item *

transpose

If true, set the bits in the bit matrix that correspond to edges
but in the reverse direction. This has the effect of transposing the
matrix. Obviously makes no difference to the result for undirected graphs.

=back

=back

=head2 Object Methods

=over 4

=item get($u, $v)

Return true if the bit matrix has a "one bit" between the vertices
$u and $v; in other words, if there is (at least one) a vertex going from
$u to $v.  If there is no vertex and therefore a "zero bit", return false.

=item set($u, $v)

Set the bit between the vertices $u and $v; in other words, connect
the vertices $u and $v by an edge.  The change does not get mirrored
back to the original graph.  Returns nothing.

=item unset($u, $v)

Unset the bit between the vertices $u and $v; in other words, disconnect
the vertices $u and $v by an edge.  The change does not get mirrored
back to the original graph.  Returns nothing.

=item get_row($u, $v1, $v2, ..., $vn)

Test the row at vertex C<u> for the vertices C<v1>, C<v2>, ..., C<vn>
Returns a list of I<n> truth values.

=item set_row($u, $v1, $v2, ..., $vn)

Sets the row at vertex C<u> for the vertices C<v1>, C<v2>, ..., C<vn>,
in other words, connects the vertex C<u> to the vertices C<vi>.
The changes do not get mirrored back to the original graph.
Returns nothing.

=item unset_row($u, $v1, $v2, ..., $vn)

Unsets the row at vertex C<u> for the vertices C<v1>, C<v2>, ..., C<vn>,
in other words, disconnects the vertex C<u> from the vertices C<vi>.
The changes do not get mirrored back to the original graph.
Returns nothing.

=item vertices

Return the list of vertices in the bit matrix.

=back

=head1 ALGORITHM

The algorithm used to create the matrix is two nested loops, which is
O(V**2) in time, and the returned matrices are O(V**2) in space.

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
