#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day04_test.txt';
my $INPUT_FILE = 'day04_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 4: Camp Cleanup";

my @parsed = parse_extents(@input);

solve_part_one(@parsed);
solve_part_two(@parsed);

exit( 0 );

sub solve_part_one(@input) {
	my $count_contains = 0;
	for @input -> @pair {
		$count_contains += 1 if @pair[0].contains(@pair[1]) || @pair[1].contains(@pair[0]);
	}
	say "Part One: The number of pairs where one range contains the other is $count_contains.";
}

sub solve_part_two(@input) {
	my $count_overlaps = 0;
	for @input -> @pair {
		$count_overlaps += 1 if @pair[0].overlaps(@pair[1]) || @pair[1].overlaps(@pair[0]);
	}
	say "Part Two: The number of pairs where one range contains the other is $count_overlaps.";	
}

sub parse_extents(@input --> Array) {
	my @extents = @input.map(-> $line {
		$line ~~ /(\d+) \- (\d+) \, (\d+) \- (\d+)/;
		my $e1 = Extent1D.new(range => $0..$1);
		my $e2 = Extent1D.new(range => $2..$3);
		($e1,$e2);
	});
	return @extents;
}
