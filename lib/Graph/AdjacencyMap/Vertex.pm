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
    my $m = shift;
    my @rows;
    my $s = $m->[ _s ];
    my $d;
    push @rows, map [ $_, ref($d=$s->{$_}) ? "$d->[0],".$m->_dumper($d->[-1]) : $d ],
	sort map @$_, $m->paths;
    $m->SUPER::stringify . join '',
        map "$_\n",
        map join(' ', map sprintf('%4s', $_), @$_),
        @rows;
}

require overload; # for de-overloading

sub __strval {
  my ($k, $f) = @_;
  ref $k && ($f & _REF) &&
    (($f & _STR) ? !overload::Method($k, '""') : overload::Method($k, '""')) ?
	overload::StrVal($k) : $k;
}

sub __set_path {
    my $f = $_[0]->[ _f ];
    Graph::__carp_confess(sprintf __PACKAGE__.": arguments %d expected %d for\n".$_[0]->stringify, @_ - 1)
	if @_ != (2 + ($f & _MULTI));
    [ $_[0]->[ _s ] ||= { } ], [ __strval($_[1], $f) ];
}

sub __set_path_node {
    my ($m, $p, $l) = @_;
    my $f = $m->[ _f ];
    my $id = $_[-1] if ($f & _MULTI);
    return $m->_inc_node( \$p->[-1]->{ $l }, $id ) if exists $p->[-1]->{ $l };
    my $i = $m->_new_node( \$p->[-1]->{ $l }, $id );
    $m->[ _i ]->{ defined $i ? $i : "" } = $_[3];
}

sub set_path {
    my $m = shift;
    my $f = $m->[ _f ];
    my ($p, $k) = $m->__set_path( @_ );
    return unless defined $p && defined $k;
    my $l = defined $k->[-1] ? $k->[-1] : "";
    my $set = $m->__set_path_node( $p, $l, @_ );
    return $set;
}

sub __has_path {
    my $f = $_[0]->[ _f ];
    Graph::__carp_confess(sprintf __PACKAGE__.": arguments %d expected %d for\n".$_[0]->stringify, @_ - 1)
	if @_ != (2 + ($f & _MULTI));
    return unless defined(my $p = $_[0]->[ _s ]);
    return ([$p], [ __strval($_[1], $f) ]);
}

sub has_path {
    my $m = shift;
    return unless my ($p, $k) = $m->__has_path( @_ );
    return exists $p->[-1]->{ defined $k->[-1] ? $k->[-1] : "" };
}

sub has_path_by_multi_id {
    my $m = shift;
    my $id = pop;
    my ($e, $n) = $m->__get_path_node( @_ );
    return undef unless $e;
    return exists $n->[ _nm ]->{ $id };
}

sub _get_path_id {
    my $m = shift;
    my $f = $m->[ _f ];
    my ($e, $n) = $m->__get_path_node( @_ );
    return undef unless $e;
    return ref $n ? $n->[ _ni ] : $n;
}

sub _get_path_count {
    my $m = shift;
    my $f = $m->[ _f ];
    my ($e, $n) = $m->__get_path_node( @_ );
    return 0 unless $e && defined $n;
    return
	($f & _COUNT) ? $n->[ _nc ] :
	($f & _MULTI) ? scalar keys %{ $n->[ _nm ] } : 1;
}

sub __attr {
    my $m = shift;
    return unless @_ && ref $_[0] && @{ $_[0] };
    Graph::__carp_confess(sprintf __PACKAGE__.": arguments %d expected %d for\n".$m->stringify,
		  scalar @{ $_[0] }, $m->[ _a ])
        if @{ $_[0] } != $m->[ _a ];
    my $f = $m->[ _f ];
    return if !(@{ $_[0] } > 1 && ($f & _UNORDUNIQ));
    if (($f & _UNORDUNIQ) == _UNORD && @{ $_[0] } > 1) {
	@{ $_[0] } = sort @{ $_[0] }
    } else { $m->__arg(\@_) }
}

sub _get_id_path {
    my ($m, $i) = @_;
    return defined $m->[ _i ] ? $m->[ _i ]->{ $i } : undef;
}

sub del_path {
    my $m = shift;
    my $f = $m->[ _f ];
    my ($e, $n, $p, $k, $l) = $m->__get_path_node( @_ );
    return unless $e;
    my $c = ($f & _COUNT) ? --$n->[ _nc ] : 0;
    if ($c == 0) {
	delete $m->[ _i ]->{ ref $n ? $n->[ _ni ] : $n };
	delete $p->[ -1 ]->{ $l };
    }
    return 1;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    my ($e, $n, $p, $k, $l) = $m->__get_path_node( $from );
    return unless $e;
    $m->[ _i ]{ ref $n ? $n->[ _ni ] : $n } = $to;
    $p->[ -1 ]->{ $to } = delete $p->[ -1 ]->{ $l };
    return 1;
}

sub del_path_by_multi_id {
    my $m = shift;
    my $f = $m->[ _f ];
    my $id = pop;
    my ($e, $n, $p, $k, $l) = $m->__get_path_node( @_ );
    return unless $e;
    delete $n->[ _nm ]->{ $id };
    unless (keys %{ $n->[ _nm ] }) {
	delete $m->[ _i ]->{ $n->[ _ni ] };
	delete $p->[-1]->{ $l };
    }
    return 1;
}

sub paths {
    my $m = shift;
    return map { [ $_ ] } values %{ $m->[ _i ] } if defined $m->[ _i ];
    wantarray ? ( ) : 0;
}

1;
=pod

=head1 NAME

Graph::AdjacencyMap::Vertex - create and a map of graph vertices or edges

=head1 SYNOPSIS

    Internal.

=head1 DESCRIPTION

B<This module is meant for internal use by the Graph module.>

=head2 Object Methods

=over 4

=item del_path(@id)

Delete a Map path by ids.

=item del_path_by_multi_id($id)

Delete a Map path by a multi(vertex) id.

=item has_path(@id)

Return true if the Map has the path by ids, false if not.

=item has_path_by_multi_id($id)

Return true if the Map has the path by a multi(vertex) id, false if not.

=item paths

Return all the paths of the Map.

=item set_path(@id)

Set the path by @ids.

=back

=head1 AUTHOR AND COPYRIGHT

Jarkko Hietaniemi F<jhi@iki.fi>

=head1 LICENSE

This module is licensed under the same terms as Perl itself.

=cut
