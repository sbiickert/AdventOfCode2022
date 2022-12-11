#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
use Date::Calc qw(Add_Delta_Days);

# https://adventofqode.org
say "Advent of Qode 2022, Day 10: Wait, how many days do they have in a year?";

my $years = 67;
my $DAYS_IN_MARS_YEAR = 686;

my $days = $years * $DAYS_IN_MARS_YEAR;

my @date_of_first_aggression = Add_Delta_Days(2022, 12, 10, -1 * $days);

say join('/', @date_of_first_aggression);

exit( 0 );

