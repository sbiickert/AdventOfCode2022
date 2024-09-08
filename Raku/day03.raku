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

solve_part_one(@input);
solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $sum = 0;
	for @input -> $line {
		my $first = $line.substr(0, $line.chars / 2);
		my $first_set = Set.new($first.split('', :skip-empty));
		my $last = $line.substr($line.chars / 2);
		my $last_set = Set.new($last.split('', :skip-empty));
		my $common = $first_set (&) $last_set;
		for $common.keys -> $letter {
			$sum = $sum + %priorities{$letter};
		}
	}
	say "Part One: the sum of priorities of common letters is $sum.";
}

sub solve_part_two(@input) {
	my $sum = 0;
	
	loop (my $i = 0; $i < @input.elems; $i += 3) {
		my $set0 = Set.new(@input[$i+0].split('', :skip-empty));
		my $set1 = Set.new(@input[$i+1].split('', :skip-empty));
		my $set2 = Set.new(@input[$i+2].split('', :skip-empty));
		my $common = $set0 (&) $set1 (&) $set2;
		for $common.keys -> $letter {
			$sum = $sum + %priorities{$letter};
		}
	}
	say "Part Two: the sum of priorities of group badges is $sum.";
}
