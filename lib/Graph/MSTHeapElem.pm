package Graph::MSTHeapElem;

use strict;
use warnings;

sub new {
    my $class = shift;
    bless { u => $_[0], v => $_[1], w => $_[2] }, $class;
}

sub heap {
    my $self = shift;
    @_ ? ($self->{heap} = shift) : $self->{heap};
}

sub cmp {
    ($_[0]->{ w } || 0) <=> ($_[1]->{ w } || 0);
}

sub val {
    @{ $_[0] }{ qw(u v w) };
}

1;
