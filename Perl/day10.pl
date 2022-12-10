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
#solve_part_two(@input);

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
			if (($cycle + 20) % 40 == 0) { $sig_strength += $cycle * $x; say $sig_strength; }
			$x += $operation->{'val'};
			$cycle++
		}
		#say "After cycle $cycle, X is $x " . $cycle * $x;
		if (($cycle + 20) % 40 == 0) { $sig_strength += $cycle * $x; say $sig_strength; }
	}
	
	say "Part One: signal strength is $sig_strength.";
}

sub solve_part_two {
	my @input = @_;
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