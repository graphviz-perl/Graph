package Graph::AdjacencyMap::Light;

# THIS IS INTERNAL IMPLEMENTATION ONLY, NOT TO BE USED DIRECTLY.
# THE INTERFACE IS HARD TO USE AND GOING TO STAY THAT WAY AND
# ALMOST GUARANTEED TO CHANGE OR GO AWAY IN FUTURE RELEASES.

use strict;
use warnings;

use Graph::AdjacencyMap qw(:flags :fields);
use base 'Graph::AdjacencyMap';

# $SIG{__DIE__ } = \&Graph::__carp_confess;
# $SIG{__WARN__} = \&Graph::__carp_confess;

sub _is_COUNT    () { 0 }
sub _is_MULTI    () { 0 }
sub _is_UNIQ     () { 0 }
sub _is_REF      () { 0 }

sub _new {
    my ($class, $flags, $arity) = @_;
    (my $m = $class->SUPER::_new($flags | _LIGHT, $arity))->[ _attr ] = {};
    $m;
}

sub set_paths {
    my ($m, @paths) = @_;
    my ($f, $a, $i, $pi, $map_s, $map_p, @ids) = (@$m[ _f, _arity, _i, _pi, _s, _p ]);
    for (@paths) {
	my @args = @$_;
	Graph::__carp_confess("Wrong number of args: expected $a, got (@args)") if $a != @args;
	my $l = join ' ', @args;
	push(@ids, $pi->{ $l }), next if defined $pi->{ $l };
	$i->[ my $n = $m->[ _n ]++ ] = $_;
	$pi->{ $l } = $n;
	push @ids, $n;
	Graph::AdjacencyMap::_successors_add($f, $map_s, $map_p, $n, $_) if $map_s;
    }
    @ids;
}

sub get_ids_by_paths {
    my ($pi, $m, $list, $ensure, $deep) = ( @{ $_[0] }[ _pi ], @_ );
    map {
	my @ret = map {
	    my $id = $pi->{ join ' ', @$_ };
	    defined $id ? $id : $ensure ? $m->set_paths($_) : return;
	} $deep ? @$_ : $_;
	$deep ? \@ret : @ret;
    } @$list;
}

sub has_path {
    my ($a, $pi, @args) = ( @{ $_[0] }[ _arity, _pi ], @{ $_[1] } );
    Graph::__carp_confess("Wrong number of args, want $a, got (@args)") if $a != @args;
    $pi->{ join ' ', @args };
}

sub _get_path_count {
    defined(my $dummy = &has_path) ? 1 : 0; # defined &x asks if func defined
}

sub del_path {
    my ($f, $a, $i, $pi, $map_s, $map_p, $attr, @args) = ( @{ my $m = $_[0] }[ _f, _arity, _i, _pi, _s, _p, _attr ], @{ $_[1] } );
    Graph::__carp_confess("Wrong number of args, want $a, got (@args)") if $a != @args;
    my $l = join ' ', @args;
    return 0 if !exists $pi->{ $l };
    my $id = delete $pi->{ $l };
    delete $attr->{ $l };
    my $path = delete $i->[ $id ];
    Graph::AdjacencyMap::_successors_del($f, $map_s, $map_p, $id, $path) if $map_s;
    return 1;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    my ($a, $i, $pi, $attr) = @$m[ _arity, _i, _pi, _attr ];
    return 1 if $a > 1; # arity > 1, all integers, no names
    return 0 unless exists $pi->{ $from };
    $attr->{ $to } =     delete $attr->{ $from } if $attr->{ $from };
    $i->[ $pi->{ $to } = delete $pi->{ $from } ] = [ $to ];
    return 1;
}

sub _set_path_attr_common {
    (my $m = $_[0])->set_paths($_[1]);
    my ($attr, @args) = ( @$m[ _attr ], @{ $_[1] } );
    \$attr->{ join ' ', @args };
}

sub _get_path_attrs {
    my ($a, $attr, @args) = ( @{ $_[0] }[ _arity, _attr ], @{ $_[1] } );
    Graph::__carp_confess("Wrong number of args, want $a, got (@args)") if $a != @args;
    $attr->{ join ' ', @args };
}

sub _del_path_attrs {
    return undef unless defined &has_path;
    my ($a, $attr, @args) = ( @{ $_[0] }[ _arity, _attr ], @{ $_[1] } );
    my $l = join ' ', @args;
    return 0 unless exists $attr->{ $l };
    delete $attr->{ $l };
    1;
}

1;
