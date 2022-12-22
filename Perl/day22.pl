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
#my $INPUT_FILE = 'day22_test.txt';
my $INPUT_FILE = 'day22_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 22: Monkey Map";

our $start;
our @DIRS = ('>', 'v', '<', '^');
our @OFFS = ([1,0], [0,1], [-1,0], [0,-1]);

my $map = parse_map($input[0]);
my @moves = parse_moves($input[1]);

solve_part_one($map, @moves);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my ($map, @moves) = @_;
	my $facing = 0;
	my $pos = C2D_add($start, [0,0]); # copy
	
	G2D_set($map, $pos, $DIRS[$facing]);
	for my $i (0..$#moves) {
		if ($moves[$i] =~ m/\d+/) {
			for my $m (1..$moves[$i]) {
				$pos = move($map, $pos, $facing);
				G2D_set($map, $pos, $DIRS[$facing]);
			}
		}
		else {
			$facing = turn($facing, $moves[$i]);
		}
		G2D_set($map, $pos, $DIRS[$facing]);
	}
	
	#G2D_print($map);
	my $password = 1000 * $pos->[1] + 4 * $pos->[0] + $facing;
	say "Part One: the end position is " . C2D_to_str($pos) . " and the password is $password.";
}

sub solve_part_two {
	my @input = @_;
}

sub move {
	my ($map, $pos, $facing) = @_;
	
	# Get the value in front
	my $next_pos = C2D_add($pos, $OFFS[$facing]);
	my $next_value = G2D_get($map, $next_pos);
	
	if ($next_value eq ' ') {
		# Need to wrap. Back up until find the other edge.
		my $r_facing = reverse_direction($facing);
		my $back_pos = C2D_add($pos, $OFFS[$r_facing]);
		my $back_value = G2D_get($map, $back_pos);
		while (1) {
			my $next_back_pos = C2D_add($back_pos, $OFFS[$r_facing]);
			my $back_value = G2D_get($map, $next_back_pos);
			if ($back_value eq ' ') {
				$next_pos = $back_pos;
				$next_value = G2D_get($map, $back_pos);
				last;
			}
			$back_pos = $next_back_pos;
		}
	}
	if ($next_value eq '#') {
		return $pos;
	}
	return $next_pos;
}

sub turn {
	my ($facing, $turn) = @_;
	$facing += ($turn eq 'L') ? -1 : 1;
	return $facing % 4;
}

sub reverse_direction {
	my $facing = shift;
	return ($facing + 2) % 4;
}

sub parse_map {
	my $ref = shift;
	my @input = @{$ref};
	my $map = G2D_create(' ', 'rook');
	
	for my $r (0..$#input) {
		my @chars = split('', $input[$r]);
		for my $c (0..$#chars) {
			G2D_set($map, [$c+1, $r+1], $chars[$c]);
			$start = [$c+1, $r+1] if ($r == 0 && !defined($start) && $chars[$c] eq '.');
		}
	}
	
	#G2D_print($map);
	#say C2D_to_str($start);
	
	return $map;
}

sub parse_moves {
	my $ref = shift;
	my @input = @{$ref};
	my $line = $input[0];
	my @moves = $line =~ m/\d+|[LR]/g;
	#say join(' ', @moves);
	return @moves;
}