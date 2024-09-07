#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day01_test.txt';
my $INPUT_FILE = 'day01_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 01: Calorie Counting";

solve_part_one(@input);
solve_part_two(@input);

exit( 0 );

sub solve_part_one(@input) {
	my $most_calories = 0;
	for @input -> @elf_is_carrying {
		my $sum = @elf_is_carrying.reduce(&infix:<+>);
		$most_calories = max($most_calories, $sum);
	}
	say "The most calories carried by an elf is $most_calories.";
}

sub solve_part_two(@input) {
	my @elf_calories = @input.map(->@elf_is_carrying {
		@elf_is_carrying.reduce(&infix:<+>)
	});
	my @sorted = @elf_calories.sort.reverse;
	my $sum_3 = @sorted[0..2].reduce(&infix:<+>);
	say "The calories carried by the top three elves is $sum_3.";
}
