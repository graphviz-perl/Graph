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

sub set_path {
    my ($m, @args) = ($_[0], @{ $_[1] });
    return if @args == 0;
    my ($n, $f, $a, $i, $s) = @$m;
    my $e0 = shift @args;
    return $n if exists $s->{ $e0 } && (($a == 1) or exists $s->{ $e0 }->{ $args[0] });
    $n = $m->[ _n ]++;
    $i->[ $n ] = [ $e0, @args ];
    if ($a == 2) {
	my $e1 = shift @args;
	$s->{ $e0 }->{ $e1 } = $n;
    } else {
	$s->{ $e0 } = $n;
    }
    $n;
}

sub paths_non_existing {
    push @_, 0;
    goto &_paths_lookup;
}

sub _paths_lookup {
    my ($m, $list, $want_exist) = @_;
    my ($n, $f, $a, $i, $s) = @$m;
    map {
	my @p = @$_;
	my $this_s = $s;
	$this_s = $this_s->{ shift @p } while defined $this_s and @p;
	($want_exist xor !defined $this_s) ? ($want_exist ? $this_s : $_) : ();
    } @$list;
}

sub has_path {
    my ($f, $a, $s, @args) = ( @{ $_[0] }[ _f, _arity, _s ], @{ $_[1] } );
    return 0 unless $a == @args;
    $s = $s->{ shift @args } while defined $s and @args;
    defined $s ? 1 : 0;
}

sub get_ids_by_paths {
    push @_, 1;
    goto &_paths_lookup;
}

sub _get_path_count {
    &has_path ? 1 : undef;
}

sub has_paths { keys %{ $_[0]->[ _s ] } }

sub del_path {
    my ($i, $s, $attr, @args) = ( @{ my $m = $_[0] }[ _i, _s, _attr ], @{ $_[1] } );
    return 0 if !defined(my $n = $s->{ my $e0 = shift @args });
    if (@args == 1) {
	my $e1 = shift @args;
	return 0 if !defined($n = $n->{ $e1 });
	delete $s->{ $e0 }->{ $e1 };
	delete $s->{ $e0 } unless keys %{ $s->{ $e0 } };
	delete $attr->{ $e0 }->{ $e1 };
	delete $attr->{ $e0 } unless keys %{ $attr->{ $e0 } };
    } else {
	delete $s->{ $e0 };
	delete $attr->{ $e0 };
    }
    delete $i->[ $n ];
    return 1;
}

sub rename_path {
    my ($m, $from, $to) = @_;
    my ($a, $i, $s, $attr) = @$m[ _arity, _i, _s, _attr ];
    return 1 if $a > 1; # arity > 1, all integers, no names
    return 0 unless exists $s->{ $from };
    $s->{ $to } = delete $s->{ $from };
    $attr->{ $to } = delete $attr->{ $from } if $attr->{ $from };
    $i->[ $s->{ $to } ] = [ $to ];
    return 1;
}

sub _set_path_attr_common {
    &set_path;
    my ($attr, @e) = ( @{ $_[0] }[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } ||= {} while $attr and @e > 1;
    \$attr->{ $e[0] };
}

sub _get_path_attrs {
    my ($attr, @e) = ( @{ $_[0] }[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } while $attr and @e > 0;
    $attr ? $attr : ();
}

sub _del_path_attrs {
    return undef unless &has_path;
    my ($attr, @e) = ( @{ $_[0] }[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } while $attr and @e > 1;
    return 0 unless $attr and exists $attr->{ $e[0] };
    delete $attr->{ $e[0] };
    1;
}

1;
