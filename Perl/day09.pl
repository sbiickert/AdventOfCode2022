#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day09_test.txt';
my $INPUT_FILE = 'day09_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2022, Day 09: Rope Bridge";

our %offsets = ( 'R' => C2D_create(1, 0),
				 'L' => C2D_create(-1, 0),
				 'U' => C2D_create(0, 1),
				 'D' => C2D_create(0, -1) );
				 
my $result1 = solve(2, @input);
say "Part One: tail visited $result1 places.";

my $result2 = solve(10, @input);
say "Part Two: tail visited $result2 places.";

exit( 0 );

sub solve {
	my ($knot_count, @input) = @_;
	my @knots = ();
	for my $k (1..$knot_count) {
		push(@knots, C2D_create(0,0));
	}
	my $visited = G2D_create('.', 'queen');
	G2D_set($visited, $knots[-1], '#');
	
	for my $line (@input) {
		my ($dir, $count) = split(' ', $line);
		for my $n (1..$count) {
			$knots[0] = C2D_add($knots[0], $offsets{$dir});
			for my $t (1..$#knots) {
				$knots[$t] = follow($knots[$t-1], $knots[$t]);
			}
			G2D_set($visited, $knots[-1], '#');
		}
	}

	my $hist = G2D_histogram($visited);
	
# 	G2D_set($visited, [0,0], 's');
# 	G2D_set($visited, $knots[0], 'H');
# 	for my $t (1..$#knots) {
# 		G2D_set($visited, $knots[$t], $t);
# 	}
# 	G2D_print($visited);
	
	return $hist->{'#'};
}

sub follow {
	my ($head, $tail) = @_;
	
	if (C2D_distance($head, $tail) > 1.5) {
		my $delta = C2D_delta($head, $tail);
		my $dx = 0; my $dy = 0;
		if ($delta->[0] != 0) {
			# Move 1 in x direction
			$dx = ($head->[0] < $tail->[0]) ? -1 : 1;	
		}
		if ($delta->[1] != 0) {
			# Move 1 in y direction
			$dy = ($head->[1] < $tail->[1]) ? -1 : 1;	
		}
		$tail = C2D_add($tail, [$dx, $dy]);
	}
	
	return $tail;
}