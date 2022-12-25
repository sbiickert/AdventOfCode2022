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
use List::Util qw(min);

no warnings 'recursion';

use AOC::Util;
use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day24_test.txt';
my $INPUT_FILE = 'day24_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 24: Blizzard Basin";

our @DIRS = ('>', 'v', '<', '^');
our %DIR_IDX = ('>' => 0, 'v' => 1, '<' => 2, '^' => 3);
our %DIR_VAL = ('>' => 2**0, 'v' => 2**1, '<' => 2**2, '^' => 2**3);
our %DIR_OFF = ('>' => [1,0], 'v' => [0,1], '<' => [-1,0], '^' => [0,-1]);
our %VAL_DIR = ( 2**0 => '>', 2**1 => 'v', 2**2 => '<', 2**3 => '^');

our $VALLEY; # created in parse_map
our $START = [1,0];
our $END;    # created in parse_map
our @MAPS = (parse_map(@input));

our $BEST_MOVES;
our %CACHE;

solve();
#solve_part_two(@input);

exit( 0 );

sub solve {
	$BEST_MOVES = 500;
	%CACHE = ();
	my $move_count = run(0, $START->[0], $START->[1]);
	say "Part One: the minimum move count is $move_count.";
	
	my $temp = $START;
	$START = $END;
	$END = $temp;
	$BEST_MOVES = 1000;
	%CACHE = ();

	my $moves_back = run($move_count, $START->[0], $START->[1]);
	
	$temp = $START;
	$START = $END;
	$END = $temp;
	$BEST_MOVES = 1500;
	%CACHE = ();

	my $moves_there_again = run($moves_back, $START->[0], $START->[1]);
	say "Part Two: the minimum move count is $moves_there_again.";
}

sub run {
	my ($moves, $x, $y) = @_;
	my $pos = [$x, $y];
	
	#say "Starting run at M:$moves, X:$x, Y:$y";
	
	# Return early if no way to get to END in fewer moves than BEST_MOVES
	my $result = 10000000;
	if ($moves + C2D_manhattan($pos, $END) > $BEST_MOVES) {
		#say "Was at $x, $y, couldn't get to end before $BEST_MOVES";
		return $result;
	}
	
	$moves++;
	my $cache_key = join(',', ($moves, $x, $y));
	if (exists($CACHE{$cache_key})) { return $CACHE{$cache_key}; }
	
	my $map = get_map($moves);
	
	# Where could we have moved to?
	my @results = ();
	for my $n (G2D_neighbors($map, $pos)) {
		next if ($n->[1] < 0); # Don't go north from the start
		
		if (C2D_equal($END, $n))  {
			# Found the end
			#say "Found end @ move $moves.";
			if ($moves <= $BEST_MOVES) {
				say $moves;
				$BEST_MOVES = $moves;
			}
			return $moves;
		}
		
		my $n_val = G2D_get($map, $n);
		if ($n_val ne '#' and $n_val eq '0') {
			# Spot to hop to
			push(@results, run($moves, $n->[0], $n->[1]));
		}
	}
	
	if (G2D_get($map, $pos) eq '0') {
		# Standing still is a move
		push(@results, run($moves, $pos->[0], $pos->[1]));
	}
	
	if (scalar( @results ) > 0) {
		$result = min(@results);
	}
# 	else {
# 		say "died at time $time at $x, $y: nowhere to go";
# 		print_map($map);
# 	}
	
	$CACHE{$cache_key} = $result;
	
	return $result;
}

sub get_map {
	my $time = shift;
	if ($time <= $#MAPS) { return $MAPS[$time]; }
	while ($#MAPS < $time) {
		push(@MAPS, move_blizzards($MAPS[-1]));
	}
	return $MAPS[$time];
}

sub move_blizzards {
	my $map = shift;
	my $ext = G2D_extent($map);
	my $out = dclone($VALLEY);
	$ext = E2D_inset($ext, 1);
	
	for my $r ($ext->[1]..$ext->[3]) {
		for my $c ($ext->[0]..$ext->[2]) {
			my $b_pos = [$c, $r];
			my $value = G2D_get($map, $b_pos);
			next if !is_blizzard($value);
			#say "blizzard at " . C2D_to_str($b_pos);
			for my $dir (@DIRS) {
				next if (!($value & $DIR_VAL{$dir}));
				#say "blizzard pointing $dir";
				my $offset = $DIR_OFF{$dir};
				my $new_pos = C2D_add([$c,$r], $offset);
				if (!E2D_contains($ext, $new_pos)) {
					# Wrap
					if    ($new_pos->[0] < $ext->[0]) { $new_pos->[0] = $ext->[2]; }
					elsif ($new_pos->[0] > $ext->[2]) { $new_pos->[0] = $ext->[0]; }
					elsif ($new_pos->[1] < $ext->[1]) { $new_pos->[1] = $ext->[3]; }
					elsif ($new_pos->[1] > $ext->[3]) { $new_pos->[1] = $ext->[1]; }
					#say "wrapped to " . C2D_to_str($new_pos);
				}
				die if (!E2D_contains($ext, $new_pos));
				my $current_value = G2D_get($out, $new_pos);
				#say "Current value at " . C2D_to_str($new_pos) . " is $current_value";
				G2D_set($out, $new_pos, $current_value + $DIR_VAL{$dir});
				#say "Setting value at " . C2D_to_str($new_pos) . " to ". ($current_value + $DIR_VAL{$dir});
			}
		}
	}
	return $out;
}

sub solve_part_two {
	my @input = @_;
}

sub parse_map {
	my @input = @_;
	my $map = G2D_create('0', 'rook');
	$VALLEY = G2D_create('0', 'rook');
	
	for my $row (0..$#input) {
		my @line = split('', $input[$row]);
		for my $col (0..$#line) {
			my $char = $line[$col];
			if ($char eq '#') {
				G2D_set($map, [$col, $row], $char);
				G2D_set($VALLEY, [$col, $row], $char);
			}
			elsif ($char ne '.') {
				G2D_set($map, [$col, $row], $DIR_VAL{$char});
			}
		}
	}
	my $ext = G2D_extent($map);
	
	$END = [$ext->[2]-1, $ext->[3]];
	return $map;
}

sub print_map {
	my ($g2d, $pos) = @_;
	my $e2d = G2D_extent($g2d);
	my $ymin = $e2d->[1];
	my $ymax = $e2d->[3];
	for (my $y = $ymin; $y <= $ymax; $y++) {
		my @row = ();
		for (my $x = $e2d->[0]; $x <= $e2d->[2]; $x++) {
			my $c = C2D_create($x, $y);
			if (defined($pos) && C2D_equal($pos, $c)) {
				push( @row, 'E' );
			}
			else {
				push( @row, value_to_char(G2D_get($g2d, $c)) );
			}
		}
		say join('', @row);
	}
}

sub value_to_char {
	my $value = shift;
	return '.' if !$value;
	return $value if (!is_blizzard($value));
	
	my $char;
	my $count = 0;
	for my $v (values %DIR_VAL) {
		if ($value & $v) {
			$char = $VAL_DIR{$v};
			$count++;
		}
	}
	
	if ($count > 1) { return $count; }
	return $char;
}

sub is_blizzard {
	my $value = shift;
	if ($value eq '0') { return 0; }
	if ($value eq '#') { return 0; }
	#if ($value eq '.') { return 0; }
	#if ($value eq 'E') { return 0; }
	#if ($value eq 'F') { return 0; }
	return 1;
}