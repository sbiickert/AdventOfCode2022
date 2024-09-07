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

my @elf_calories = @input.map(->@elf_is_carrying {
	@elf_is_carrying.reduce(&infix:<+>)
}).sort.reverse;

solve_part_one(@elf_calories);
solve_part_two(@elf_calories);

exit( 0 );

sub solve_part_one(@elf_calories) {
	my $most_calories = @elf_calories[0];
	say "The most calories carried by an elf is $most_calories.";
}

sub solve_part_two(@elf_calories) {
	my $sum_3 = @elf_calories[0..2].reduce(&infix:<+>);
	say "The calories carried by the top three elves is $sum_3.";
}
