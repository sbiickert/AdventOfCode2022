#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day03_test.txt';
my $INPUT_FILE = 'day03_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 03: Rucksack Reorganization";

my Int %priorities = Hash.new;
for 'a' .. 'z' -> $c { %priorities{$c} = $c.ord - 96; }
for 'A' .. 'Z' -> $C { %priorities{$C} = $C.ord - 38; }
%priorities{''} = 0;

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $sum = 0;
	for @input -> $line {
		my $first = $line.substr(0, $line.chars / 2);
		my $last = $line.substr($line.chars / 2);
		my $first_set = Set.new($first.split(''));
		my $last_set = Set.new($last.split(''));
		my $common = $first_set (&) $last_set;
		for $common.keys -> $letter {
			$sum = $sum + %priorities{$letter};
		}
	}
	say "Part One: the sum of priorities of common letters is $sum.";
}

sub solve_part_two(@input) {
	
}
