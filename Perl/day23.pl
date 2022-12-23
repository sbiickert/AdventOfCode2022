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
#my $INPUT_FILE = 'day23_test.txt';
my $INPUT_FILE = 'day23_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 23: Unstable Diffusion";

my $map = parse_map(@input);
our %OFFS = ('N' => [[0,-1], [1,-1], [-1,-1]], 	# N, NE, NW
			'S' => [[0,1], [1,1], [-1,1]],		# S, SE, SW
			'W' => [[-1,0], [-1,-1], [-1,1]],	# W, NW, SW
			'E' => [[1,0], [1,-1], [1,1]]);		# E, NE, SE

solve_part_one($map);

$map = parse_map(@input);
solve_part_two($map);

exit( 0 );

sub solve_part_one {
	my $map = shift;
	
	#G2D_print($map);
	my @ORDER = ('N', 'S', 'W', 'E');

	for my $round (1..10) {
		my $count = move_elves($map, @ORDER);
		#say $round;
		#G2D_print($map);
		my $temp = shift(@ORDER);
		push(@ORDER, $temp);
	}
	
	my $ext = G2D_extent($map);
	my @elf_coords = G2D_coords_with_value($map, '#');
	
	my $empty = E2D_area($ext) - scalar(@elf_coords);
	say "Part One: the empty space is $empty.";
}

sub solve_part_two {
	my $map = shift;
	
	#G2D_print($map);
	my @ORDER = ('N', 'S', 'W', 'E');

	my $round = 1;
	while (1) {
		say $round if $round % 25 == 0;
		my $move_count = move_elves($map, @ORDER);
		if ($move_count == 0) {
			say "No elves moved.";
			last;
		}
		#G2D_print($map);
		my $temp = shift(@ORDER);
		push(@ORDER, $temp);
		$round++;
	}
		
	say "Part Two: no elves moved at round $round.";
	#G2D_print($map);
}

sub move_elves {
	my ($map, @order) = @_;
	
	# First half: elves propose movement
	my @elf_coords = G2D_coords_with_value($map, '#');
	
	my %proposals = ();
	for my $ec (@elf_coords) {
		#say "elf at " . C2D_to_str($ec);
		my @neighbors = G2D_neighbors($map, $ec);
		
		my $no_elves = 1;
		for my $n (@neighbors) {
			if (G2D_get($map, $n) eq '#') {
				$no_elves = 0;
				last;
			}
		}
		next if $no_elves;
		
		for my $dir (@order) {
			#say $dir;
			my $offsets = $OFFS{$dir};
			$no_elves = 1;
			for my $o (@{$offsets}) {
				my $n = C2D_add($ec, $o);
				if (G2D_get($map, $n) eq '#') {
					#say "found elf at " . C2D_to_str($n);
					$no_elves = 0;
					last;
				}
			}
			if ($no_elves) {
				# Propose moving towards $dir
				my $key = C2D_to_str(C2D_add($ec, $offsets->[0]));
				#say "propose moving to $key";
				$proposals{$key} = [] if !exists($proposals{$key});
				push(@{$proposals{$key}}, C2D_to_str($ec));
				last;
			}
		}
	}
	
	# Second half: elves move if they were the only one to propose moving to coord
	my $count = 0;
	for my $coord_str (keys %proposals) {
		my @list = @{$proposals{$coord_str}};
		#say join(', ', @list);
		if (scalar(@list) == 1) {
			# Only one elf proposed to move to the coord
			#say "moving elf from $list[0] to $coord_str";
			my $ec = C2D_from_str($list[0]);
			my $coord = C2D_from_str($coord_str);
			G2D_clear($map, $ec);
			G2D_set($map, $coord, '#');
			$count++;
		}
	}
	
	return $count;
}

sub parse_map {
	my @input = @_;
	my $map = G2D_create('.', 'queen');
	
	for my $row (0..$#input) {
		my @chars = split('', $input[$row]);
		for my $col (0..$#chars) {
			my $char = $chars[$col];
			G2D_set($map, [$col, $row], '#') if ($char eq '#');
		}
	}
	
	return $map;
}