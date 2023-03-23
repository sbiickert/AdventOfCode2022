#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
# use Storable 'dclone';
# use Carp qw(confess cluck);
# $SIG{__WARN__} = 'cluck';
# $SIG{__DIE__} = 'confess';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day12_test.txt';
my $INPUT_FILE = 'day12_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 12: Hill Climbing Algorithm";

our ($elev, $start, $end) = parse_elevation_map(@input);

my $cost_part1 = find_path($start);
say "Part One: cost to reach E was " . $cost_part1;

my @lowest = G2D_coords_with_value($elev, 1); # 1 is 'a'
my @costs_part2 = ();
for my $low (@lowest) {
	my $cost = find_path($low);
	if ($cost > 0) { # If a path can't be found, -1 is returned
		push(@costs_part2, $cost);
	}
}
@costs_part2 = sort {$a <=> $b} @costs_part2;
say "Part Two: lowest cost to reach E was " . $costs_part2[0];

exit( 0 );

sub find_path {
	my $starting_point = shift;
	#say "find_path starting at " . C2D_to_str($starting_point);
	my $cost = G2D_create('100000000', 'rook');
	my $pos = $starting_point;
	my @to_visit = ();
	
	G2D_set($cost, $pos, 0);
	
	while (G2D_get($cost, $end) eq '100000000') {
		if (!defined($pos)) {
			return -1;
		}
		
		my @neighbors = G2D_neighbors($cost, $pos);
		my $cost_at_pos = (G2D_get($cost, $pos));
		my $elev_at_pos = (G2D_get($elev, $pos));
		for my $n (@neighbors) {
			my $n_elev = G2D_get($elev, $n);
			next if $n_elev eq '.';
			my $n_cost = G2D_get($cost, $n);
			if ($n_cost <= $cost_at_pos+1 || $n_elev - $elev_at_pos > 1) {
				next;
			}
			G2D_set($cost, $n, $cost_at_pos+1);
			push(@to_visit, $n);
		}
		
		# Lowest cost last
		@to_visit = sort {G2D_get($cost, $b) <=> G2D_get($cost, $a)} @to_visit;
		
		$pos = pop @to_visit;
	}
	return G2D_get($cost, $end);
}

sub parse_elevation_map {
	my @input = @_;
	my $map = G2D_create('.', 'rook');
	my $rows = scalar @input;
	my $cols = length($input[0]);
	my $start = [];
	my $end = [];
	my %lookup = ();
	
	my $i = 1;
	for my $letter ('a'..'z') {
		$lookup{$letter} = $i;
		$i++;
	}
	
	for my $r (0..$rows-1) {
		my @col_letters = split('', $input[$r]);
		for my $c (0..$cols-1) {
			my $letter = $col_letters[$c];
			if ($letter eq 'S') {
				$start = [$c, $r];
				$letter = 'a';
			}
			elsif ($letter eq 'E') {
				$end = [$c, $r];
				$letter = 'z';
			}
			G2D_set($map, [$c, $r], $lookup{$letter});
		}
	}
	
	#G2D_print($map);
	#say "Start: " . C2D_to_str($start);
	#say "End: " . C2D_to_str($end);
	
	return ($map, $start, $end);
}