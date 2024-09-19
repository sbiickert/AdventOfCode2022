#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
use AOC::Geometry;
use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day08_test.txt';
my $INPUT_FILE = 'day08_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 8: Treetop Tree House";

my $map = Grid.new(rule => AdjacencyRule::ROOK, default => '.');
$map.load(@input);

#$map.print;

solve_part_one($map);
solve_part_two($map);

exit( 0 );

sub solve_part_one(Grid $map) {
	my $interior_ext = $map.extent.inset(1);
	my $count_visible = $map.extent.area - $interior_ext.area; # Outer trees
	
	for $interior_ext.all_coords -> $c {
		my $ht = $map.get($c).Int;
		my Bool $visible = False;
		my @dirs = adjacent_dirs($map.rule);
		for @dirs -> $dir {
			my Bool $visible_in_dir = True;
			my $offset_c = $c.offset($dir);
			while $map.extent.contains($offset_c) {
				my $other_ht = $map.get($offset_c).Int;
				if $other_ht >= $ht {
					$visible_in_dir = False;
					last;
				}
				$offset_c = $offset_c.offset($dir);
			}
			if $visible_in_dir {
				$visible = True;
				last;
			}
		}
		$count_visible++ if $visible;
	}
	say "Part One: a total of $count_visible trees are visible.";
}

sub solve_part_two(Grid $map) {
	my $interior_ext = $map.extent.inset(1);
	my Coord $best = Nil;
	my Int $best_score = 0;
	
	for $interior_ext.all_coords -> $c {
		my $ht = $map.get($c).Int;
		my Int $score = 1;
		my @dirs = adjacent_dirs($map.rule);
		for @dirs -> $dir {
			my $dist = 0;
			my $offset_c = $c.offset($dir);
			while $map.extent.contains($offset_c) {
				my $other_ht = $map.get($offset_c).Int;
				$dist++;
				if $other_ht >= $ht {
					last;
				}
				$offset_c = $offset_c.offset($dir);
			}
			$score *= $dist;
		}
		if $score > $best_score {
			$best_score = $score;
			$best = $c;
		}
	}
	say "Part Two: the location with the best score is $best with score $best_score.";
}
