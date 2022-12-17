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
#my $INPUT_FILE = 'day17_test.txt';
my $INPUT_FILE = 'day17_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 17: Pyroclastic Flow";

our @rocks;
our @jets;

parse_input(@input);

solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my $grid = G2D_create('.', 'rook');
	
	for my $x (0..6) {
		G2D_set($grid, [$x, 0], '#');
	}
	G2D_set($grid, [-1,0], '+');
	G2D_set($grid, [7,0], '+');
	
	my $rock_index = 0;
	my $jet_index = 0;
	my $tower_height = 0;
	for my $round (1..2022) {
		add_rock($grid, $rocks[$rock_index], $tower_height);
		
		#G2D_print($grid, 1);
		
		while (1) {
			# jet
			#say $jets[$jet_index];
			move_rock($grid, $jets[$jet_index] eq '<' ? [-1,0] : [1,0]);
			$jet_index++;
			$jet_index = 0 if $jet_index > $#jets;
			#G2D_print($grid, 1);
			# gravity
			#say 'v';
			if (!move_rock($grid, [0,-1])) {
				# Turn @'s into #'s
				land_rock($grid);
				last;
			}
			#G2D_print($grid, 1);
		}
		
		$rock_index++;
		$rock_index = 0 if $rock_index > $#rocks;
		
		$tower_height = get_tower_height($grid);
	}
	
	say "Part One: the tower height is $tower_height.";
}

sub get_tower_height {
	my $grid = shift;
	my @land = G2D_coords_with_value($grid, '#');
	my $ext = E2D_build(@land);
	return E2D_max($ext)->[1];
}

sub add_rock {
	my ($grid, $rock, $ymax) = @_;
	my $origin = [2, $ymax+3+1]; # 3 above max is 3+1
	
	for my $coord (@{$rock}) {
		G2D_set($grid, C2D_add($origin, $coord), '@');
	}
	my $ext = G2D_extent($grid);
	my $new_ymax = E2D_max($ext)->[1];
	for my $y ($ymax..$new_ymax) {
		G2D_set($grid, [-1,$y], '|');
		G2D_set($grid, [ 7,$y], '|');
	}
}

sub move_rock {
	my ($grid, $offset) = @_;
	my @rock = G2D_coords_with_value($grid, '@');
	my @offset_rock = ();
	for my $coord (@rock) {
		my $offset_coord = C2D_add($coord, $offset);
		my $value = G2D_get($grid, $offset_coord);
		return 0 if ($value eq '#' or $value eq '|');
		push(@offset_rock, $offset_coord);
	}
	# There hasn't been a collision if we've gotten to here
	for my $coord (@rock) {
		G2D_set($grid, $coord, '.');
	}	
	for my $coord (@offset_rock) {
		G2D_set($grid, $coord, '@');
	}
	return 1;
}

sub land_rock {
	my $grid = shift;
	my @rock = G2D_coords_with_value($grid, '@');
	for my $coord (@rock) {
		G2D_set($grid, $coord, '#');
	}
	#say 'Rock landed.';
}

sub solve_part_two {
	my @input = @_;
}

sub parse_input {
	my @input = @_;
	my $jet_group = pop(@input);
	
	@jets = split('', $jet_group->[0]);
	
	for my $gref (@input) {
		my @rock = ();
		my @group = @{$gref};
		my $row = 0;
		for (my $r = $#group; $r >= 0; $r--) {
			my $line = $group[$row];
			my @chars = split('', $line);
			for my $c (0..$#chars) {
				push(@rock, C2D_create($c, $r)) if $chars[$c] eq '#';
			}
			$row++;
		}
		push(@rocks, \@rock);
	}
}