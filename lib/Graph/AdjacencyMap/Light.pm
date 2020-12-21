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

my @LOCAL_OVERRIDE = (_s, _p);

sub _is_COUNT    () { 0 }
sub _is_MULTI    () { 0 }
sub _is_REF      () { 0 }

sub _new {
    my ($class, $flags, $arity) = @_;
    (my $m = $class->SUPER::_new($flags | _LIGHT, $arity))->[ _attr ] = {};
    @$m[ @LOCAL_OVERRIDE ] = map $m->[ $_ ] ? [] : undef, @LOCAL_OVERRIDE;
    $m;
}

sub set_paths {
    my ($m, @paths) = @_;
    my ($f, $a, $i, $pi, $map_s, $map_p, @ids) = (@$m[ _f, _arity, _i, _pi, _s, _p ]);
    for (@paths) {
	my $k = $_;
	Graph::__carp_confess("Wrong number of args, want $a, got (@$k)") if $a != 1 and $a != @$k;
	my $l = $a == 1 ? $k : join ' ', @$k;
	push(@ids, $pi->{ $l }), next if defined $pi->{ $l };
	$i->[ my $n = $m->[ _n ]++ ] = $_;
	$pi->{ $l } = $n;
	push @ids, $n;
	_successors_add($f, $map_s, $map_p, $n, $_) if $map_s;
    }
    @ids;
}

sub _successors_set {
    my $val = pop;
    my ($f, $map_s, $map_p, $id, $path) = @_;
    my $pairs = Graph::AdjacencyMap::_successors_cartesian(($f & _UNORD), 0, $path);
    no warnings 'uninitialized'; # needed 5.8
    vec($map_s->[ $_->[0] ], $_->[1], 1) = $val for @$pairs; # row-major
    return if !$map_p;
    vec($map_p->[ $_->[1] ], $_->[0], 1) = $val for @$pairs;
}
sub _successors_add { push @_, 1; goto &_successors_set }
sub _successors_del { push @_, 0; goto &_successors_set }

sub _paths_fromto {
    my $offset = pop;
    my ($i, $pi, $f, $map_x, @v) = ( @{ $_[0] }[ _i, _pi, _f, $offset ], @_[1..$#_] );
    Graph::__carp_confess("undefined vertex") if grep !defined, @v;
    require Set::Object;
    my ($paths, $invert, $unord) = (Set::Object->new, $offset == _p, $f & _UNORD);
    for my $tuple (grep defined $_->[1], map [$_, $map_x->[$_]], @v) {
	my ($v, $s) = ($tuple->[0], scalar unpack("b*", $tuple->[1]));
	$paths->insert(join ' ', (
	    $unord ? sort($v, pos($s) - 1) :
	    $invert ? (pos($s) - 1, $v) : ($v, pos($s) - 1)
	)) while $s =~ /1/g;
    }
    map $i->[ $pi->{ $_ } ], $paths->members;
}
sub paths_from { push @_, _s; goto &_paths_fromto }
sub paths_to { push @_, _p; goto &_paths_fromto }

sub _cessors {
    my $offset = pop;
    my ($map_x, @v) = ( @{ $_[0] }[ $offset ], @_[1..$#_] );
    Graph::__carp_confess("undefined vertex") if grep !defined, @v;
    require Set::Object;
    my $c = Set::Object->new;
    for my $row (grep defined, @$map_x[ @v ]) {
	# 10x quicker than: grep vec($row, $_, 1), 0..$#$m
	my $s = unpack("b*", $row);
	$c->insert(pos($s) - 1) while $s =~ /1/g;
    }
    $c->members;
}
sub successors { push @_, _s; goto &_cessors }
sub predecessors { push @_, _p; goto &_cessors }

sub has_successor {
    my ($map_s, $u, $v) = ( @{ $_[0] }[ _s ], @_[1, 2] );
    Graph::__carp_confess("undefined vertex") if grep !defined, $u, $v;
    vec(($map_s->[ $u ] || return 0), $v, 1);
}

sub get_ids_by_paths {
    my ($pi, $m, $list, $ensure, $deep) = ( @{ $_[0] }[ _pi ], @_ );
    $deep ||= 0;
    map {
	my @ret = map {
	    my @ret2 = map {
		my $id = $pi->{ $_ };
		defined $id ? $id : $ensure ? $m->set_paths($_) : return;
	    } $deep > 1 ? @$_ : $_;
	    $deep > 1 ? \@ret2 : @ret2;
	} $deep ? @$_ : $_;
	$deep ? \@ret : @ret;
    } @$list;
}

sub has_path {
    my ($a, $pi, $k) = ( @{ $_[0] }[ _arity, _pi ], $_[1] );
    Graph::__carp_confess("Wrong number of args, want $a, got (@$k)") if $a != 1 and $a != @$k;
    $pi->{ $a == 1 ? $k : join ' ', @$k };
}

sub _get_path_count {
    defined(my $dummy = &has_path) ? 1 : 0; # defined &x asks if func defined
}

sub del_path {
    my ($f, $a, $i, $pi, $map_s, $map_p, $attr, $k) = ( @{ my $m = $_[0] }[ _f, _arity, _i, _pi, _s, _p, _attr ], $_[1] );
    Graph::__carp_confess("Wrong number of args, want $a, got (@$k)") if $a != 1 and $a != @$k;
    my $l = $a == 1 ? $k : join ' ', @$k;
    return 0 if !exists $pi->{ $l };
    my $id = delete $pi->{ $l };
    delete $attr->{ $l };
    my $path = delete $i->[ $id ];
    _successors_del($f, $map_s, $map_p, $id, $path) if $map_s;
    return 1;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    my ($a, $i, $pi, $attr) = @$m[ _arity, _i, _pi, _attr ];
    return 1 if $a > 1; # arity > 1, all integers, no names
    return 0 unless exists $pi->{ $from };
    $attr->{ $to } =     delete $attr->{ $from } if $attr->{ $from };
    $i->[ $pi->{ $to } = delete $pi->{ $from } ] = $to;
    return 1;
}

sub _set_path_attr_common {
    (my $m = $_[0])->set_paths($_[1]);
    my ($a, $attr, $k) = ( @$m[ _arity, _attr ], $_[1] );
    my $l = $a == 1 ? $k : join ' ', @$k;
    \$attr->{ $l };
}

sub _get_path_attrs {
    my ($a, $attr, $k) = ( @{ $_[0] }[ _arity, _attr ], $_[1] );
    Graph::__carp_confess("Wrong number of args, want $a, got (@$k)") if $a != 1 and $a != @$k;
    my $l = $a == 1 ? $k : join ' ', @$k;
    $attr->{ $l };
}

sub _del_path_attrs {
    return undef unless defined &has_path;
    my ($a, $attr, $k) = ( @{ $_[0] }[ _arity, _attr ], $_[1] );
    my $l = $a == 1 ? $k : join ' ', @$k;
    return 0 unless exists $attr->{ $l };
    delete $attr->{ $l };
    1;
}

1;
