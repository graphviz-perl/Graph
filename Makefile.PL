use ExtUtils::MakeMaker;

require 5.006;
use Getopt::Long;

my $mm_ver = $ExtUtils::MakeMaker::VERSION;
if ($mm_ver =~ /_/) { # dev version
    $mm_ver = eval $mm_ver;
    die $@ if $@;
}

my $renum;

GetOptions('renumber' => \$renum);

do 'util/renum.pl' if $renum;

my %PREREQ_PM =
    (
    'List::Util'	=> 0,
    'Scalar::Util'	=> 0,
    'Math::Complex'	=> 0,
    'Test::More'	=> 0
    );

if ($] >= 5.008) {
    $PREREQ_PM{'Storable'} = '2.05';
    $PREREQ_PM{'Safe'	 } = 0,
}

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
                bugtracker  => 'http://rt.cpan.org/Public/Dist/Display.html?Name=Graph',
                repository  => {
                    type => 'git',
                    web  => 'https://github.com/neilbowers/Graph',
                    url  => 'git://github.com/neilbowers/Graph.git',
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
