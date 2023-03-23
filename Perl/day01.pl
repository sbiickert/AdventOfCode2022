#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2018;
use autodie;
use List::Util qw(max);
#use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day01_test.txt';
my $INPUT_FILE = 'day01_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 01: Calorie Counting";

my @elf_calories = solve_part_one(@input);
solve_part_two(@elf_calories);

exit( 0 );

sub solve_part_one {
	my @groups = @_;
	
	my @all_elf_calories = ();
	
	for my $g (@groups) {
		my $elf_calories = 0;
		my @group = @{$g};
		for my $num (@group) {
			$elf_calories += $num;
		}
		push( @all_elf_calories, $elf_calories);
	}
	
	my $max_calories = max(@all_elf_calories);
	say "Part One: the largest number of calories an elf is carrying is $max_calories.";
	
	return @all_elf_calories;
}

sub solve_part_two {
	my @input = @_;
	
	my @sorted_elf_calories = sort {$b <=> $a} @input;
	my $top_three = $sorted_elf_calories[0] +
					$sorted_elf_calories[1] +
					$sorted_elf_calories[2];
	say "Part Two: the top three elves are carrying $top_three calories.";
}
