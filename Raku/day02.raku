#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day02_test.txt';
my $INPUT_FILE = 'day02_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 02: Rock Paper Scissors";

my Int %lookup = 'A' => 1, 'X' => 1, 'B' => 2, 'Y' => 2, 'C' => 3, 'Z' => 3;

# Maps to lists of pairs of numbers ((#,#),(#,#))
my @rounds = @input.map(-> $r {
	my @throws = $r.split(' ');
	(%lookup{@throws[0]},%lookup{@throws[1]});
});

solve_part_one(@rounds);
solve_part_two(@rounds);

exit( 0 );

sub solve_part_one(@rounds) {
	my $total = calc_score(@rounds);
	say "Part One: my total score is $total.";
}

sub solve_part_two(@rounds) {
	my @adjusted = @rounds.map(-> @r {
		my ($throw, $result) = @r;
		my $my_throw = (($throw + $result - 3) % 3) + 1;
		($throw, $my_throw);
	});
	
	my $total = calc_score(@adjusted);
	say "Part Two: my total score is $total.";
}

sub calc_score(@rounds --> Int) {
	my @scores = @rounds.map(-> @round {
		my ($throw, $my_throw) = @round;
		my $score = $my_throw;
		given ($my_throw - $throw) % 3 {
			when  0 { $score += 3; } # draw
			when  1 { $score += 6; } # win
		}
		$score;
	});
	my $total = @scores.reduce(&infix:<+>);
}
