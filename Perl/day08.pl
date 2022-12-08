#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day08_test.txt';
my $INPUT_FILE = 'day08_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 08: Treetop Tree House";

my $trees = parse_trees(@input);

solve_part_one($trees);
solve_part_two($trees);

exit( 0 );

sub solve_part_one {
	my $trees = shift;
	my $vis = G2D_create(' ', 'rook');
	my $ext = G2D_extent($trees);
	my $size = E2D_max($ext)->[0]; # assume square
	my @fwd = (0..$size);
	my @rev = reverse @fwd;
	my $max_h;
	
	for (my $r = 0; $r <= $size; $r++) {
		for my $offset_list (\@fwd, \@rev) {
			$max_h = -1;
			for my $x (@{$offset_list}) {
				my $c2d = C2D_create($x, $r);
				my $h = G2D_get($trees, $c2d);
				if ($h > $max_h) {
					G2D_set($vis, $c2d, $h);
					$max_h = $h;
				}
			}
		}
	}
	
	for (my $c = 0; $c <= $size; $c++) {
		for my $offset_list (\@fwd, \@rev) {
			$max_h = -1;
			for my $y (@{$offset_list}) {
				my $c2d = C2D_create($c, $y);
				my $h = G2D_get($trees, $c2d);
				if ($h > $max_h) {
					G2D_set($vis, $c2d, $h);
					$max_h = $h;
				}
			}
		}
	}
	
	my $vis_count = scalar G2D_coords($vis);
	
	say "Part One: the number of visible trees is $vis_count.";
}

sub solve_part_two {
	my $trees = shift;
	my $scenic = G2D_create(' ', 'rook');
	my $ext = G2D_extent($trees);
	
	for my $x (0..E2D_max($ext)->[0]) {
		for my $y (0..E2D_max($ext)->[1]) {
			my $tree_loc = C2D_create($x, $y);
			G2D_set($scenic, $tree_loc, calc_scenic_score($trees, $tree_loc));
		}
	}
	
	my $hist = G2D_histogram($scenic);
	my @scores = sort { $b <=> $a } keys %{$hist}; 
	
	say "Part Two: the maximum scenic score is $scores[0].";
}

sub calc_scenic_score {
	my ($trees, $tree_loc) = @_;
	my $ext = G2D_extent($trees);
	my @offsets = G2D_offsets($trees);
	my $tree_h = G2D_get($trees, $tree_loc);;
	my $score = 1;
	
	for my $offset (@offsets) {
		my $vd = 0;
		
		my $loc = C2D_add($tree_loc, $offset);
		
		while (E2D_contains($ext, $loc)) {
			my $h = G2D_get($trees, $loc);
			if ($h >= $tree_h) {
				$vd = C2D_manhattan($tree_loc, $loc);
				last;
			}
			$loc = C2D_add($loc, $offset);
		}
		
		if ($vd == 0) {
			# Were not blocked
			$vd = C2D_manhattan($tree_loc, $loc) - 1;
		}
		
		$score *= $vd;
	}
	return $score;
}

sub parse_trees {
	my @input = @_;
	my $trees = G2D_create(0, 'rook');
	
	my $y = 0;
	for my $row (@input) {
		my $x = 0;
		for my $val (split('', $row)) {
			G2D_set($trees, C2D_create($x, $y), $val);
			$x++;
		}
		$y++;
	}
	#G2D_print($trees);
	return $trees;
}