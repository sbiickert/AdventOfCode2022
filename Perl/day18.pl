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
#my $INPUT_FILE = 'day18_test.txt';
my $INPUT_FILE = 'day18_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 18: Boiling Boulders";

my @coords = parse_coords(@input);

solve_part_one(@coords);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my @coords = @_;
	my $grid = G3D_create('.', 'rook');
	
	for my $coord (@coords) {
		G3D_set($grid, $coord, '#');
	}
	
	my $surface_area = 0;
	for my $coord (@coords) {
		my @neighbors = G3D_neighbors($grid, $coord);
		for my $n (@neighbors) {
			$surface_area++ if (G3D_get($grid, $n) ne '#');
		}
	}
	
	say "Part One: surface area is $surface_area.";
}

sub solve_part_two {
	my @input = @_;
}

sub parse_coords {
	my @input = @_;
	my @coords = ();
	
	for my $line (@input) {
		my ($x, $y, $z) = split(',', $line);
		push(@coords, [$x, $y, $z]);
	}
	return @coords;
}