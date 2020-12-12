use strict; use warnings;
use Test::More tests => 9;
package Array;
use Graph::Attribute array => 0;
sub new { bless [], shift }
package main;
my $o = Array->new();
ok(!$o->_g_has_attributes());
is(my $a = $o->_g_get_attributes(), undef);
ok($o->_g_set_attributes({foo => 42}));
ok($o->_g_has_attributes());
ok($a = $o->_g_get_attributes());
is($a->{foo}, 42);
ok($o->_g_delete_attributes());
ok(!$o->_g_has_attributes());
is($a = $o->_g_get_attributes(), undef);
