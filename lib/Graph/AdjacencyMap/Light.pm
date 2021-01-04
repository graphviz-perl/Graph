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
    my ($a, $i, $s, @ids) = (@$m[ _arity, _i, _s ]);
    for (@paths) {
	my @args = @$_;
	Graph::__carp_confess("Wrong number of args: expected $a, got (@args)") if $a != @args;
	my $e0 = shift @args;
	push(@ids, $s->{ $e0 }), next if $a == 1 && exists $s->{ $e0 };
	push(@ids, $s->{ $e0 }{ $args[0] }), next if $a == 2 && exists $s->{ $e0 } && defined $s->{ $e0 }{ $args[0] };
	$i->[ my $n = $m->[ _n ]++ ] = [ $e0, @args ];
	if ($a == 2) {
	    $s->{ $e0 }{ shift @args } = $n;
	} else {
	    $s->{ $e0 } = $n;
	}
	push @ids, $n;
    }
    @ids;
}

sub get_ids_by_paths {
    my ($s, $m, $list, $ensure, $deep) = ( @{ $_[0] }[ _s ], @_ );
    map {
	my @ret = map {
	    my ($this_s, @p) = ($s, @$_);
	    $this_s = $this_s->{ shift @p } while defined $this_s and @p;
	    defined $this_s ? $this_s : $ensure ? $m->set_paths($_) : return;
	} $deep ? @$_ : $_;
	$deep ? \@ret : @ret;
    } @$list;
}

sub has_path {
    my ($a, $s, @args) = ( @{ $_[0] }[ _arity, _s ], @{ $_[1] } );
    return undef unless $a == @args;
    $s = $s->{ shift @args } while defined $s and @args;
    $s;
}

sub _get_path_count {
    defined(my $dummy = &has_path) ? 1 : 0; # defined &x asks if func defined
}

sub has_any_paths { keys %{ $_[0]->[ _s ] } }

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
    (my $m = $_[0])->set_paths($_[1]);
    my ($attr, @e) = ( @$m[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } ||= {} while $attr and @e > 1;
    \$attr->{ $e[0] };
}

sub _get_path_attrs {
    my ($attr, @e) = ( @{ $_[0] }[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } while $attr and @e > 0;
    $attr ? $attr : ();
}

sub _del_path_attrs {
    return undef unless defined &has_path;
    my ($attr, @e) = ( @{ $_[0] }[ _attr ], @{ $_[1] } );
    $attr = $attr->{ shift @e } while $attr and @e > 1;
    return 0 unless $attr and exists $attr->{ $e[0] };
    delete $attr->{ $e[0] };
    1;
}

1;
