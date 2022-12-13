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
use List::Util qw(max);
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day13_test.txt';
my $INPUT_FILE = 'day13_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 13: Distress Signal";

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my @input = @_;
	
	my $index = 1;
	my $sum = 0;
	
	for my $pair (@input) {
		my $comp_result = compare(eval($pair->[0]), eval($pair->[1]));
		if ($comp_result < 1) {
			# In the right order (equal, or left less than right)
			$sum += $index;
		}
		$index++;
	}
	
	say "Part One: the sum of indexes is $sum.";
}

sub compare {
	# returns 	1 if left is greater than right, 
	# 			0 if left equals right, and 
	#			-1 if left is less than left
	my ($left, $right) = @_;
	
	if (ref($left) && !ref($right))		{ $right = [$right]; }
	elsif (!ref($left) && ref($right))	{ $left = [$left]; }
	
	if (ref($left)) {
		# lists on left and right
		my $max_length = max(scalar(@{$left}), scalar(@{$right}));
		for my $i (0..$max_length-1) {
			if (!defined($left->[$i])) 	{ return -1; }
			if (!defined($right->[$i]))	{ return 1; }
			my $comp_result = compare($left->[$i], $right->[$i]);
			if ($comp_result != 0)		{ return $comp_result; }
		}
	}
	else {
		# integers on left and right
		return $left <=> $right;
	}
	return 0;
}

sub solve_part_two {
	my @input = @_;
}
