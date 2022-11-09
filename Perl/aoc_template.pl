#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2018;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
my $INPUT_FILE = 'day<##>_test.txt';
#my $INPUT_FILE = 'day<##>_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code <##>, Day <##>: <##>";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my @input = @_;
}

sub solve_part_two {
	my @input = @_;
}
