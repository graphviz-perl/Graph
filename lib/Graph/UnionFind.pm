package Graph::UnionFind;

use strict;
use warnings;

sub _PARENT  () { 0 }
sub _RANK    () { 1 }

sub new {
    my $class = shift;
    bless { }, $class;
}

sub add {
    my ($self, @elems) = @_;
    @elems = grep !defined $self->{$_}, @elems;
    @$self{ @elems } = map [ $_, 0 ], @elems;
}

sub _parent {
    return undef unless defined $_[1];
    Graph::__carp_confess(__PACKAGE__ . "::_parent: bad arity") if @_ < 2 or @_ > 3;
    if (@_ == 2) {
	exists $_[0]->{ $_[ 1 ] } ? $_[0]->{ $_[1] }->[ _PARENT ] : undef;
    } else {
	$_[0]->{ $_[1] }->[ _PARENT ] = $_[2];
    }
}

sub _rank {
    return unless defined $_[1];
    Graph::__carp_confess(__PACKAGE__ . "::_rank: bad arity") if @_ < 2 or @_ > 3;
    if (@_ == 2) {
	exists $_[0]->{ $_[1] } ? $_[0]->{ $_[1] }->[ _RANK ] : undef;
    } else {
	$_[0]->{ $_[1] }->[ _RANK ] = $_[2];
    }
}

sub find {
    my ($self, @v) = @_;
    my @ret;
    for my $x (@v) {
	push(@ret, undef), next unless defined(my $px = $self->_parent($x));
	$self->_parent( $x, $self->find( $px ) ) if $px ne $x;
	push @ret, $self->_parent( $x );
    }
    @ret;
}

sub union {
    my ($self, @edges) = @_;
    $self->add(map @$_, @edges);
    for my $e (@edges) {
	my ($px, $py) = $self->find( @$e );
	next if $px eq $py;
	my $rx = $self->_rank( $px );
	my $ry = $self->_rank( $py );
	# print "union($x, $y): px = $px, py = $py, rx = $rx, ry = $ry\n";
	if ( $rx > $ry ) {
	    $self->_parent( $py, $px );
	} else {
	    $self->_parent( $px, $py );
	    $self->_rank( $py, $ry + 1 ) if $rx == $ry;
	}
    }
}

sub same {
    my ($uf, $u, $v) = @_;
    my ($fu, $fv) = $uf->find($u, $v);
    return undef if grep !defined, $fu, $fv;
    $fu eq $fv;
}

1;
__END__
=pod

=head1 NAME

Graph::UnionFind - union-find data structures

=head1 SYNOPSIS

    use Graph::UnionFind;
    my $uf = Graph::UnionFind->new;

    # Add the vertices to the data structure.
    $uf->add($u);
    $uf->add($v);

    # Join the partitions of the vertices.
    $uf->union( $u, $v );

    # Find the partitions the vertices belong to
    # in the union-find data structure.  If they
    # are equal, they are in the same partition.
    # If the vertex has not been seen,
    # undef is returned.
    my $pu = $uf->find( $u );
    my $pv = $uf->find( $v );
    $uf->same($u, $v) # Equal to $pu eq $pv. 

    # Has the union-find seen this vertex?
    $uf->has( $v )

=head1 DESCRIPTION

I<Union-find> is a special data structure that can be used to track the
partitioning of a set into subsets (a problem also known as I<disjoint sets>).

C<Graph::UnionFind> is used for L<Graph/connected_components>,
L<Graph/connected_component>, and L<Graph/same_connected_components>
if you specify a true C<unionfind> parameter when you create an undirected
graph.

Union-find is one way: you cannot (easily) 'ununion' vertices once you
have 'unioned' them. This is why L<Graph> throws an exception if you
try to delete edges from a union-find graph.

=head2 API

=over 4

=item add

    $uf->add(@v)

Add the vertices to the union-find.

=item union

    $uf->union([$u, $v], [$w, $x], ...)

Add the edge u-v to the union-find.  Also implicitly adds the vertices.

=item find

    @partitions = $uf->find(@v)

For each given vertex, return the union-find partition it belongs to,
or C<undef> if it has not been added.

=item new

    $uf = Graph::UnionFind->new()

The constructor.

=item same

    $uf->same($u, $v)

Return true of the vertices belong to the same union-find partition
the vertex v belongs to, false otherwise.

=back

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut

