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
#my $INPUT_FILE = 'day22_test.txt';
my $INPUT_FILE = 'day22_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 22: Monkey Map";

our $start;
our @DIRS = ('>', 'v', '<', '^');
our @OFFS = ([1,0], [0,1], [-1,0], [0,-1]);

my $map = parse_map($input[0]);
my @moves = parse_moves($input[1]);

solve_part($map, 1, @moves);

$map = parse_map($input[0]); # reset
solve_part($map, 2, @moves);

exit( 0 );

sub solve_part {
	my ($map, $wrap_rule, @moves) = @_;
	my $facing = 0;
	my $pos = C2D_add($start, [0,0]); # copy
	my $debug_map = dclone($map); # copy
	
	G2D_set($debug_map, $pos, $DIRS[$facing]);
	for my $i (0..$#moves) {
		#say $moves[$i];
		if ($moves[$i] =~ m/\d+/) {
			for my $m (1..$moves[$i]) {
				($pos, $facing) = move($map, $pos, $facing, $wrap_rule);
				G2D_set($debug_map, $pos, $DIRS[$facing]);
				#say "moved to " .  C2D_to_str($pos) . " facing $DIRS[$facing]";
			}
		}
		else {
			$facing = turn($facing, $moves[$i]);
			#say "turned $moves[$i]";
		}
		#say "at " .  C2D_to_str($pos) . " facing $DIRS[$facing]";
		G2D_set($debug_map, $pos, $DIRS[$facing]);
		#G2D_print($debug_map);
		#say '';
	}
	
	#G2D_print($debug_map);
	my $password = 1000 * $pos->[1] + 4 * $pos->[0] + $facing;
	say "Part $wrap_rule: the end position is " . C2D_to_str($pos) . " and the password is $password.";
}

sub move {
	my ($map, $pos, $facing, $wrap_rule) = @_;
	
	# Get the value in front
	my $next_pos = C2D_add($pos, $OFFS[$facing]);
	my $next_facing = $facing;
	my $next_value = G2D_get($map, $next_pos);
	
	if ($next_value eq ' ') {
		if ($wrap_rule == 1) {
			$next_pos = wrap_part1($map, $pos, $facing);
		}
		else {
			($next_pos, $next_facing) = wrap_part2($map, $pos, $facing);
		}
		$next_value = G2D_get($map, $next_pos);
	}
	
	if ($next_value eq '#') {
		return ($pos, $facing);
	}
	return ($next_pos, $next_facing);
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

sub wrap_part1 {
	my ($map, $pos, $facing) = @_;
	# Back up until find the other edge.
	my $r_facing = reverse_direction($facing);
	my $back_pos = C2D_add($pos, $OFFS[$r_facing]);
	my $back_value = G2D_get($map, $back_pos);
	while (1) {
		my $next_back_pos = C2D_add($back_pos, $OFFS[$r_facing]);
		my $back_value = G2D_get($map, $next_back_pos);
		if ($back_value eq ' ') {
			return $back_pos;
		}
		$back_pos = $next_back_pos;
	}
}

our %edges = ();

sub init_edges {
	my $is_test = shift;
	if ($is_test) {
		$edges{1} = E2D_create([9,1], [12,4]);
		$edges{2} = E2D_create([1,5], [4,8]);
		$edges{3} = E2D_create([5,5], [8,8]);
		$edges{4} = E2D_create([9,5], [12,8]);
		$edges{5} = E2D_create([9,9], [12,12]);
		$edges{6} = E2D_create([13,9], [16,12]);
	}
	else {
		$edges{1} = E2D_create( [51,1],   [100,50]);
		$edges{2} = E2D_create([101,1],   [150,50]);
		$edges{3} = E2D_create([51,51],  [100,100]);
		$edges{4} = E2D_create([1,101],   [50,150]);
		$edges{5} = E2D_create([51,101], [100,150]);
		$edges{6} = E2D_create([1,151],   [50,200]);
	}
}

sub wrap_part2 {
	my ($map, $pos, $facing) = @_;
	my $cube_face = G2D_get($map, $pos); # 1-6
	my $ext = G2D_extent($map);
	my $next_face;
	my $next_pos = [0,0];
	my $next_facing = $facing;
	
	if (E2D_width($ext) == 16) {
		# Test input
		if ( $cube_face == 1 ) {
			if ($facing == 3) { # up
				$next_face = 2;
				$next_facing = reverse_direction($facing);
				$next_pos = [$edges{1}[2] - $pos->[0] + $edges{2}[0], $edges{2}[1]];
			}
			elsif ($facing == 0) { # right
				$next_face = 6; # TESTED
				$next_facing = reverse_direction($facing);
				$next_pos = [$edges{6}[2], $edges{6}[3] - ($pos->[1] - $edges{1}[1])];
			}
			elsif($facing == 2) { # left
				$next_face = 3; # TESTED
				$next_facing = turn($facing, 'L'); # was left, now down
				$next_pos = [$pos->[1] - $edges{1}[1] + $edges{3}[0], $edges{3}[1]];
			}
		}
		if ( $cube_face == 2 ) {
			if ($facing == 2) { # left
				$next_face = 6; # TESTED
				$next_facing = turn($facing, 'R'); # was left, now up
				$next_pos = [$edges{2}[3] - $pos->[1] + $edges{6}[0], $edges{6}[3]];
			}
			elsif ($facing == 3) { # up
				$next_face = 1; # TESTED
				$next_facing = reverse_direction($facing);
				$next_pos = [$edges{1}[2] - ($pos->[0] - $edges{2}[0]), $edges{1}[1]];
			}
			elsif ($facing == 1) { # down
				$next_face = 5; # TESTED
				$next_facing = reverse_direction($facing); # was left, now up
				$next_pos = [$edges{2}[2] - $pos->[0] + $edges{5}[0], $edges{5}[3]];
			}
		}
		if ( $cube_face == 3 ) {
			if ($facing == 3) {	# up
				$next_face = 1; # TESTED
				$next_facing = turn($facing, 'R'); # was up, now right
				$next_pos = [$edges{1}[0], $pos->[0] - $edges{3}[0] + $edges{1}[1]];
			}
			elsif ($facing == 1) { # down
				$next_face = 5; # TESTED
				$next_facing = turn($facing, 'L'); # was down, now right
				$next_pos = [$edges{5}[0], $edges{5}[3] - ($edges{3}[3] - $pos->[1])];
			}
		}
		if ( $cube_face == 4 ) { # right
			$next_face = 6; # TESTED
			$next_facing = turn($facing, 'R'); # was right, now down
			$next_pos = [$edges{6}[2] - ($pos->[1] - $edges{4}[1]), $edges{6}[1]];
		}
		if ( $cube_face == 5 ) {
			if ($facing == 2) { # left
				$next_face = 3; # TESTED
				$next_facing = turn($facing, 'R'); # was left, now up
				$next_pos = [$edges{3}[2] - ($pos->[1] - $edges{5}[1]), $edges{3}[3]];
			}
			elsif ($facing == 1) { # down
				$next_face = 2; # TESTED
				$next_facing = reverse_direction($facing);
				$next_pos = [$edges{2}[2] - ($pos->[0] - $edges{5}[0]), $edges{2}[3]]
			}
		}
		if ( $cube_face == 6 ) {
			if ($facing == 3) { # up
				$next_face = 4; # TESTED
				$next_facing = turn($facing, 'L'); # was up, now left
				$next_pos = [$edges{4}[2], $edges{4}[3] - ($pos->[0] - $edges{6}[0])];
			}
			elsif ($facing == 1) { # down
				$next_face = 2; # TESTED
				$next_facing = turn($facing, 'L'); # was down, now right
				$next_pos = [$edges{2}[0], $edges{2}[0] + ($edges{6}[2] - $pos->[1])];
			}
			elsif ($facing == 0) { # right
				$next_face = 1; # TESTED
				$next_facing = reverse_direction($facing);
				$next_pos = [$edges{1}[2], $edges{6}[3] - $pos->[1] + $edges{1}[1]];
			}
		}
	}
	else {
		# Challenge input
		if ( $cube_face == 1 ) {
			if ($facing == 3) { # up
				$next_face = 6; # TESTED
				$next_facing = 0; # right
				$next_pos = [$edges{6}[0], $edges{6}[1] + $pos->[0] - $edges{1}[0]];
			}
			elsif ($facing == 2) { # left
				$next_face = 4; # TESTED
				$next_facing = 0; # right
				$next_pos = [$edges{4}[0], $edges{4}[1] + $edges{1}[3] - $pos->[1]];
			}
		}
		elsif ( $cube_face == 2 ) {
			if ($facing == 3) { # up
				$next_face = 6; # TESTED
				$next_facing = $facing; # up
				$next_pos = [$edges{6}[0] + $pos->[0] - $edges{2}[0], $edges{6}[3]];
			}
			elsif ($facing == 0) { # right
				$next_face = 5; # TESTED
				$next_facing = 2; # left
				$next_pos = [$edges{5}[2], $edges{5}[3] - ($pos->[1] - $edges{2}[1])];
			}
			elsif ($facing == 1) { # down
				$next_face = 3; # TESTED
				$next_facing = 2; # left
				$next_pos = [$edges{3}[2], $edges{3}[1] + $pos->[0] - $edges{2}[0]];
			}
		}
		elsif ( $cube_face == 3 ) {
			if ($facing == 2) { # left
				$next_face = 4; # TESTED
				$next_facing = 1; # down
				$next_pos = [$edges{4}[0] + $pos->[1] - $edges{3}[1], $edges{4}[1]];
			}
			elsif ($facing == 0) { # right
				$next_face = 2; # TESTED
				$next_facing = 3; # up
				$next_pos = [$edges{2}[0] + $pos->[1] - $edges{3}[1], $edges{2}[3]];
			}
		}
		elsif ( $cube_face == 4 ) {
			if ($facing == 3) { # up
				$next_face = 3; # TESTED
				$next_facing = 0; # right
				$next_pos = [$edges{3}[0], $edges{3}[1] + $pos->[0] - $edges{4}[0]];
			}
			elsif ($facing == 2) { # left
				$next_face = 1; # TESTED
				$next_facing = 0; # right
				$next_pos = [$edges{1}[0], $edges{1}[1] + $edges{4}[3] - $pos->[1]];
			}
		}
		elsif ( $cube_face == 5 ) {
			if ($facing == 0) { # right
				$next_face = 2; # TESTED
				$next_facing = 2; # left
				$next_pos = [$edges{2}[2], $edges{2}[1] + $edges{5}[3] - $pos->[1]];
			}
			elsif ($facing == 1) { # down
				$next_face = 6; # TESTED
				$next_facing = 2; # left
				$next_pos = [$edges{6}[2], $edges{6}[1] + $pos->[0] - $edges{5}[0]];
			}
		}
		elsif ( $cube_face == 6 ) {
			if ($facing == 0) { # right
				$next_face = 5; # TESTED
				$next_facing = 3; # up
				$next_pos = [$edges{5}[0] + $pos->[1] - $edges{6}[1], $edges{5}[3]];
			}
			elsif ($facing == 1) { # down
				$next_face = 2; # TESTED
				$next_facing = $facing;
				$next_pos = [$edges{2}[0] + $pos->[0] - $edges{6}[0], $edges{2}[1]];
			}
			elsif ($facing == 2) { # left
				$next_face = 1; # TESTED
				$next_facing = 1; # down
				$next_pos = [$edges{1}[0] + $pos->[1] - $edges{6}[1], $edges{1}[1]];
			}
		}
	}
	#say "Wrapping from face $cube_face to $next_face,";
	#say C2D_to_str($pos) . " to " . C2D_to_str($next_pos);
	#say "Facing $DIRS[$next_facing].";
	return ($next_pos, $next_facing);
}

sub parse_map {
	my $ref = shift;
	my @input = @{$ref};
	my $map = G2D_create(' ', 'rook');
	
	for my $r (0..$#input) {
		my @chars = split('', $input[$r]);
		for my $c (0..$#chars) {
			if ($chars[$c] eq ' ') { next; }
			my $coord = [$c+1, $r+1];
			$start = $coord if ($r == 0 && !defined($start) && $chars[$c] eq '.');
			if ($chars[$c] eq '.') {
				$chars[$c] = face_number($coord, $#input < 20);
			}
			G2D_set($map, $coord, $chars[$c]);
		}
	}
	
	init_edges($#input < 20);
	#G2D_print($map);
	#die;
	#say C2D_to_str($start);
	
	return $map;
}

sub face_number {
	my ($coord, $is_test) = @_;
	my $face_size = ($is_test) ? 4 : 50;
	my $col = int(($coord->[0]-1) / $face_size);
	my $row = int(($coord->[1]-1) / $face_size);
	if ($is_test) {
		if ($col == 2 && $row == 0) { return 1; }
		if ($col == 0 && $row == 1) { return 2; }
		if ($col == 1 && $row == 1) { return 3; }
		if ($col == 2 && $row == 1) { return 4; }
		if ($col == 2 && $row == 2) { return 5; }
		if ($col == 3 && $row == 2) { return 6; }
	}
	else {
		#say C2D_to_str($coord) . " is at row $row, col $col.";
		if ($col == 1 && $row == 0) { return 1; }
		if ($col == 2 && $row == 0) { return 2; }
		if ($col == 1 && $row == 1) { return 3; }
		if ($col == 0 && $row == 2) { return 4; }
		if ($col == 1 && $row == 2) { return 5; }
		if ($col == 0 && $row == 3) { return 6; }
	}
	return 'X';
}

sub parse_moves {
	my $ref = shift;
	my @input = @{$ref};
	my $line = $input[0];
	my @moves = $line =~ m/\d+|[LR]/g;
	#say join(' ', @moves);
	return @moves;
}