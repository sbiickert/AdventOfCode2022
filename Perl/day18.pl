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

our $LAVA = 	'#';
our $AIR = 		'.';
our $TRAPPED = 	'*';
our $FREE = 	'~';

our $grid = G3D_create($AIR, 'rook');

for my $coord (@coords) {
	G3D_set($grid, $coord, $LAVA);
}

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	my $surface_area = 0;
	for my $coord (@coords) {
		my @neighbors = G3D_neighbors($grid, $coord);
		for my $n (@neighbors) {
			$surface_area++ if (G3D_get($grid, $n) ne $LAVA);
		}
	}
	
	say "Part One: surface area is $surface_area.";
}

sub solve_part_two {
	my $surface_area = 0;
	for my $coord (@coords) {
		my @neighbors = G3D_neighbors($grid, $coord);
		for my $n (@neighbors) {
			my $n_val = G3D_get($grid, $n);
			if ($n_val eq $AIR) {
				$n_val = check_if_trapped($n);
			}
			$surface_area++ if ($n_val eq $FREE);
		}
	}
	
	say "Part Two: surface area is $surface_area.";
}

sub check_if_trapped {
	my $start = shift;
	my $ext = G3D_extent($grid);
	my @to_visit = ($start);
	my %visited = ();
	my $pos = $start;
	
	while (scalar(@to_visit) > 0) {
		my $pos = pop(@to_visit);
		#say "Checking " . C3D_to_str($pos);
		
		# Outside the extent of the droplet
		my $is_free = !E3D_contains($ext, $pos);
		if ($is_free) {
			set_values($FREE, values %visited);
			return $FREE;
		}
		
		my $val = G3D_get($grid, $pos);
		
		if ($val eq $LAVA) {
			next;
		}
		
		$visited{C3D_to_str($pos)} = $pos;
		
		# If it's free, then everything we've visited is free
		if ($val eq $FREE) {
			set_values($FREE, values %visited);
			return $FREE;
		}
		# If it's trapped, then everything we've visited is trapped
		if ($val eq $TRAPPED) {
			set_values($TRAPPED, values %visited);
			return $TRAPPED;
		}
		
		for my $n (G3D_neighbors($grid, $pos)) {
			if (!exists($visited{C3D_to_str($n)})) {
				push(@to_visit, $n);
			}
		}
	}

	set_values($TRAPPED, values %visited);
	return $TRAPPED;
}

sub set_values {
	my ($value, @coords) = @_;
	for my $c (@coords) {
		G3D_set($grid, $c, $value);
	}
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