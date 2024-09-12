#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day06_test.txt';
my $INPUT_FILE = 'day06_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2022, Day 6: Tuning Trouble";

solve_part_one(@input[0]);
solve_part_two(@input[0]);

exit( 0 );

sub solve_part_one(Str $input) {
	my Int $marker_end = find_marker($input, 4);
	say "Part One: the end of the marker is at position $marker_end.";
}

sub solve_part_two(Str $input) {
	my Int $marker_end = find_marker($input, 14);
	say "Part Two: the end of the marker is at position $marker_end.";
}
	
sub find_marker(Str $buffer, Int $len --> Int) {
	my @chars = $buffer.split('', :skip-empty);
	my $i = 0;
	my $j = $i + $len;
	while (True) {
		my $char_set = Set.new(@chars[$i..^$j]);
		if $char_set.keys.elems == $len {
			return $i + $len;
		}
		$i += 1;
		$j = $i + $len;
	}
	return -1;
}
