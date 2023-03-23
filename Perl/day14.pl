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
#my $INPUT_FILE = 'day14_test.txt';
my $INPUT_FILE = 'day14_challenge.txt';
our @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 14: Regolith Reservoir";

our $sand_pt = [500,0];
our @fall_offsets = ([0,1], [-1,1], [1,1]);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	my $grid = create_grid(@input);
	my $ext = G2D_extent($grid);
	
	my $sand_count = 0;
	while (1) {
		my $rest_point = drop_sand($grid);
		if (E2D_contains($ext, $rest_point)) {
			$sand_count++;
		}
		else {
			last;
		}
	}
	
	#G2D_print($grid);
	say "Part One: the amount of sand is $sand_count.";
}

sub solve_part_two {
	my $grid = create_grid(@input);
	my $ext = G2D_extent($grid);

	my $y_max = E2D_max( G2D_extent($grid) )->[1] + 2;
	draw_line($grid, [490 - $y_max, $y_max], [510 + $y_max, $y_max]);

	my $sand_count = 0;
	while (1) {
		my $rest_point = drop_sand($grid);
		if (C2D_equal($sand_pt, $rest_point)) {
			$sand_count++;
			last;
		}
		else {
			$sand_count++;
		}
		(E2D_contains($ext, $rest_point)) or die C2D_to_str($rest_point);

	}

	#G2D_print($grid);
	say "Part Two: the amount of sand is $sand_count.";
}

sub create_grid {
	my @input = @_;
	my $grid = G2D_create('.', 'queen');
	
	for my $line (@input) {
		my @coords = ();
		for my $coord (split(' -> ', $line)) {
			my @xy = split(',', $coord);
			push(@coords, \@xy);
		}
		for my $c (0..$#coords-1) {
			draw_line($grid, $coords[$c], $coords[$c+1]);
		}
	}
	G2D_set($grid, $sand_pt, '+');
	#G2D_print($grid);
	return $grid;
}

sub draw_line {
	my ($grid, $start, $end) = @_;
	#say C2D_to_str($start) . ' to ' . C2D_to_str($end);
	my $delta = C2D_delta($start, $end);
	my $md = C2D_manhattan($start, $end);
	my $offset = [$delta->[0]/$md, $delta->[1]/$md];
	
	my $draw_coord = $start;
	G2D_set($grid, $draw_coord, '#');
	#say "\tDrawing " . C2D_to_str($draw_coord);
	for my $i (0..$md-1) {
		$draw_coord = C2D_add($draw_coord, $offset);
		G2D_set($grid, $draw_coord, '#');
		#say "\tDrawing " . C2D_to_str($draw_coord);
	}
}

sub drop_sand {
	my $grid = shift;
	my $ext = G2D_extent($grid);
	my $point = C2D_add($sand_pt, [0,0]);
	while (E2D_contains($ext, $point)) {
		my $b_moved = 0;
		for my $offset (@fall_offsets) {
			my $check = C2D_add($point, $offset);
			if (G2D_get($grid, $check) eq '.') {
				$point = $check;
				$b_moved = 1;
				last;
			}
		}
		if (!$b_moved) {
			# Came to rest
			G2D_set($grid, $point, 'o');
			return $point;
		}
	}
	return $point;
}