use strict; use warnings;
use Test::More;
eval "use Test::Strict";
plan skip_all => "Test::Strict required for testing strictness/warnings" if $@;

all_perl_files_ok();