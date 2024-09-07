#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../input';
my $INPUT_FILE = 'day<##>_test.txt';
#my $INPUT_FILE = 'day<##>_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code <##>, Day <##>: <##>";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	
}

sub solve_part_two(@input) {
	
}
