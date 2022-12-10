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
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day10_test.txt';
my $INPUT_FILE = 'day10_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2022, Day 10: Cathode Ray Tube";

our @program = parse_operations(@input);

solve_part_one();
solve_part_two(@program);

exit( 0 );

sub solve_part_one {
	my $cycle = 1;
	my $x = 1;
	my $sig_strength = 0;
	
	for my $operation (@program) {
		if ($operation->{'op'} eq 'noop') {
			$cycle++;
		}
		elsif ($operation->{'op'} eq 'addx') {
			$cycle++;
			#say "After cycle $cycle, X is $x " . $cycle * $x;
			if (($cycle + 20) % 40 == 0) { $sig_strength += $cycle * $x; }
			$x += $operation->{'val'};
			$cycle++
		}
		#say "After cycle $cycle, X is $x " . $cycle * $x;
		if (($cycle + 20) % 40 == 0) { $sig_strength += $cycle * $x; }
	}
	
	say "Part One: signal strength is $sig_strength.";
}

sub solve_part_two {
	my @program = @_;
	my $cycle = 1;
	my $x = 1;
	my $op = 0;
	my $next_op_in = 0;
	my @pixels = ();

	for my $cycle (1..240) {
		if (!$op) {
			$op = shift(@program);
			if ($op->{'op'} eq 'noop') {
				$next_op_in = 1;
			}
			elsif ($op->{'op'} eq 'addx') {
				$next_op_in = 2;
			}
		}
		
		# Draw
		my $col = ($cycle-1) % 40;
		my $char = (abs($x - $col) <= 1) ? '#' : '.';
		push(@pixels, $char);
		
		if ($col == 39) {
			say join('', @pixels);
			@pixels = ();
		}
		
		if (--$next_op_in == 0) {
			if ($op->{'op'} eq 'addx') {
				#print "add $op->{'val'} to $x: ";
				$x += $op->{'val'};
				#say $x;
			}
			$op = 0;
		}
	}
}

sub parse_operations {
	my @lines = @_;
	my @ops = ();
	
	for my $line (@lines) {
		if ($line eq 'noop') {
			push(@ops, {'op' => 'noop'});
		}
		elsif ($line =~ m/(\w+) (.+)/) {
			push(@ops, {'op' => $1, 'val' => $2});
		}	
	}
	
	return @ops;
}