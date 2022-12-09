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
#my $INPUT_FILE = 'day09_test.txt';
my $INPUT_FILE = 'day09_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 09: Rope Bridge";

our %offsets = ( 'R' => C2D_create(1, 0),
				 'L' => C2D_create(-1, 0),
				 'U' => C2D_create(0, 1),
				 'D' => C2D_create(0, -1) );
				 
solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my @input = @_;
	my $head = C2D_create(0,0);
	my $tail = C2D_create(0,0);
	my $visited = G2D_create('.', 'queen');
	G2D_set($visited, $tail, '#');
	
	for my $line (@input) {
		my ($dir, $count) = split(' ', $line);
		for my $n (1..$count) {
			$head = C2D_add($head, $offsets{$dir});
			$tail = follow($head, $tail);
			G2D_set($visited, $tail, '#');
		}
	}
	
# 	G2D_set($visited, [0,0], 's');
# 	G2D_set($visited, $head, 'H');
# 	G2D_set($visited, $tail, 'T');
#	G2D_print($visited);
	
	my $hist = G2D_histogram($visited);
	my $places_tail_visited = $hist->{'#'};
	
	say "Part One: tail visited $places_tail_visited places.";
}

sub solve_part_two {
	my @input = @_;
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