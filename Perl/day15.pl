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
#my $INPUT_FILE = 'day15_test.txt';
my $INPUT_FILE = 'day15_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 15: Beacon Exclusion Zone";

our %sensors;
our %beacons;

parse_input();

# get_impossible_ranges(3363767);
# die;
solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	# Test vs Challenge
	my $search_row = scalar(keys %sensors) > 15 ? 2000000 : 10;
	my @ranges = get_impossible_ranges($search_row);
	
	my $sum = 0;
	for my $r (@ranges) {
		$sum += E1D_size($r);
		for my $b (values %beacons) {
			if ($b->[1] == $search_row && E1D_contains_value($r, $b->[0])) {
				#say "beacon at " . C2D_to_str($b);
				$sum--;
			}
		}
	}
	
	say "Part One: the number of spots a beacon can't be is $sum.";
}

sub solve_part_two {
	my $min = 0;
	my $max = 20;
	if (scalar(keys %sensors) > 15) {
		$max = 4000000;
	}
	
	my $possible = [];
	my $check = E1D_create($min, $max);
	for my $y ($min..$max) {
		my @ranges = get_impossible_ranges($y);
		my $b_ok = 0;
		for my $r (@ranges) {
			if (E1D_contains($r, $check)) {
				$b_ok = 1;
				last;
			}
		}
		if (!$b_ok) {
			say "No range completely overlaps " . E1D_to_str($check) . " at y = $y";
			# Should be two ranges, with a gap of 1 between
			#say join("\n", map { E1D_to_str($_) } @ranges);
			if ($ranges[1][0] < $ranges[0][0]) {
				@ranges = reverse @ranges;
			}
			my $x = $ranges[0][1]+1;
			$possible = [$x, $y];
			say "possible is " . C2D_to_str($possible);
			last;
		}
		say $y if $y % 250000 == 0;
	}
	
	my $tf = $possible->[0] * 4000000 + $possible->[1];
	say "Part Two: the tuning frequency is $tf.";
}

sub get_impossible_ranges {
	my $search_row = shift;
	my $DEBUG = 0;
	
	# Identify sensors that are within their md of the search row
	my @close_sensors = ();
	for my $key (keys %sensors) {
		my $sensor = $sensors{$key};
		my $dist_to_search = abs($sensor->{'pos'}[1] - $search_row);
		push(@close_sensors, $sensor) if $dist_to_search <= $sensor->{'md'};
	}
	
	my @ranges = ();
	for my $sensor (@close_sensors) {
		my $dist_to_search = abs($sensor->{'pos'}[1] - $search_row);
		my $xmin = $sensor->{'pos'}[0] - $sensor->{'md'} + $dist_to_search;
		my $xmax = $sensor->{'pos'}[0] + $sensor->{'md'} - $dist_to_search;
		my $r = E1D_create($xmin, $xmax);
		say "sensor at " . C2D_to_str($sensor->{'pos'}) . " with MD of $sensor->{'md'} means no beacons in " . E1D_to_str($r) if $DEBUG;
		push(@ranges, $r);
	}
	
	say join("\n", map { E1D_to_str($_) } @ranges) if $DEBUG;
	my $b_overlaps = 1;
	OUTER: while ($b_overlaps) {
		$b_overlaps = 0;
		for my $i (0..$#ranges-1) {
			for my $j ($i+1..$#ranges) {
				if (E1D_overlaps($ranges[$i], $ranges[$j])) {
					my $union = E1D_union($ranges[$i], $ranges[$j]);
					say E1D_to_str($ranges[$i]) . ' + ' . E1D_to_str($ranges[$j]) . " -> " . E1D_to_str($union) if $DEBUG;
					$ranges[$i] = $union;
					splice(@ranges, $j, 1);
					$b_overlaps = 1;
					say join("\n", map { E1D_to_str($_) } @ranges) if $DEBUG;
					next OUTER;
				}
			}
		}
	}
	say "final" if $DEBUG;
	say join("\n", map { E1D_to_str($_) } @ranges) if $DEBUG;
	return @ranges;
}

sub parse_input {
	%sensors = ();
	%beacons = ();
	
	my $grid = G2D_create('.', 'rook');
	for my $line (@input) {
		$line =~ m/x=(-?\d+), y=(-?\d+).+x=(-?\d+), y=(-?\d+)/;
		my $s_coord = C2D_create($1,$2);
		my $b_coord = C2D_create($3,$4);
		my $md = C2D_manhattan($s_coord, $b_coord);
		$sensors{C2D_to_str($s_coord)} = {'pos' => $s_coord,
										  'closest' => $b_coord,
										  'md' => $md };
		$beacons{C2D_to_str($b_coord)} = $b_coord;
		
		G2D_set($grid, $s_coord, 'S');
		G2D_set($grid, $b_coord, 'B');
	}
	G2D_print($grid) if scalar(keys %sensors) < 15;
}