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
use Storable 'dclone';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day12_test.txt';
my $INPUT_FILE = 'day12_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 12: Hill Climbing Algorithm";

our ($elev, $start, $end) = parse_elevation_map(@input);

solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my $cost = G2D_create('100000000', 'rook');
	my $pos = $start;
	my @to_visit = ();
	my $v_map = dclone($elev);
	
	G2D_set($cost, $pos, 0);
	my $visit_count = 0;
	while (G2D_get($cost, $end) eq '100000000') {
		say C2D_to_str($pos);
		G2D_set($v_map, $pos, '#');
		if (++$visit_count % 100 == 0) {
			say $visit_count;
			#G2D_print($v_map);
			#die;
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
			#say "Cost at " . C2D_to_str($n) . " was $n_cost, setting to " . ($cost_at_pos+1);
			G2D_set($cost, $n, $cost_at_pos+1);
			push(@to_visit, $n);
		}
		
		# Lowest cost last
		@to_visit = sort {G2D_get($cost, $b) <=> G2D_get($cost, $a)} @to_visit;
		
		$pos = pop @to_visit;
	}
	
	say "Part One: cost to reach E was " . G2D_get($cost, $end);
}

sub solve_part_two {
	my @input = @_;
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