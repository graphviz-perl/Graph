use strict; use warnings;
use ExtUtils::MakeMaker;

require 5.006;

my $mm_ver = $ExtUtils::MakeMaker::VERSION;
if ($mm_ver =~ /_/) { # dev version
    $mm_ver = eval $mm_ver;
    die $@ if $@;
}

my %PREREQ_PM = (
    'List::Util' => '1.45',
    'Scalar::Util' => 0,
    'Heap' => '0.80',
    'Set::Object' => '1.40',
);

if ($] >= 5.008) {
    $PREREQ_PM{'Storable'} = '2.05';
    $PREREQ_PM{'Safe'	 } = 0,
}

my $repo = 'graphviz-perl/Graph';
WriteMakefile(
    NAME            => 'Graph',
    VERSION_FROM    => 'lib/Graph.pm',
    PREREQ_PM       => \%PREREQ_PM,
    AUTHOR          => 'Jarkko Hietaniemi <jhi@iki.fi>',

    ($mm_ver >= 6.31
        ? (LICENSE => 'perl')
        : ()
    ),

    ($mm_ver >= 6.48
        ? (MIN_PERL_VERSION => 5.006)
        : ()
    ),

    ($mm_ver <= 6.45
        ? ()
        : (META_MERGE => {
            'meta-spec' => { version => 2 },
            resources => {
                bugtracker  => { web => "https://github.com/$repo/issues" },
                repository  => {
                    type => 'git',
                    web  => "https://github.com/$repo",
                    url  => "git://github.com/$repo.git",
                },
            },
            prereqs => {
                develop => {
                    requires => {
                        'Test::Pod::Coverage' => '1.00',
                        'Test::Pod' => '1.00',
                    },
                },
                test => {
                    requires => {
                        'Test::More'	=> '0.82', # explain
                        'Math::Complex'	=> 0,
                    },
                    recommends => {
                        'App::Prove' => '3.00', # prove -j4
                    },
                },
            },
        })
    ),
);

# Some Debian distributions have a broken List::Util (see rt.cpan.org #9568)
eval 'require Scalar::Util; import Scalar::Util qw(weaken)';
if ($@) {
    die <<__EOF__;
$@
You do not have Scalar::Util::weaken, cannot continue, aborting.
__EOF__
}
