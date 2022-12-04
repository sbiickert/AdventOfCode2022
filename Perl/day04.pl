#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2018;
use autodie;
use Data::Dumper;
use List::Util qw(min max);

use AOC::Util;
use AOC::SpatialUtil;

use feature 'signatures';

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day04_test.txt';
my $INPUT_FILE = 'day04_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 4: Camp Cleanup";

my @assignments = parseRanges(@input);

solve(@assignments);

exit( 0 );

sub solve {
	my @input = @_;
	my $contain_count = 0;
	my $overlap_count = 0;
	
	for my $elf_pair (@input) {
		if (E1D_contains($elf_pair->[0], $elf_pair->[1]) ||
			E1D_contains($elf_pair->[1], $elf_pair->[0])) {
			$contain_count++;
		}
		if (E1D_overlaps($elf_pair->[0], $elf_pair->[1])) {
			$overlap_count++;
		}
	}
	
	say "Part One: the number of assignments with complete overlap is $contain_count.";
	say "Part One: the number of assignments with partial overlap is $overlap_count.";
}

sub solve_part_two {
	my @input = @_;
}

sub parseRanges( @lines ) {
	my @elf_pairs = ();
	
	for my $line (@lines) {
		$line =~ m/(\d+)-(\d+),(\d+)-(\d+)/;
		my @pair = ();
		$pair[0] = E1D_create($1, $2);
		$pair[1] = E1D_create($3, $4);
		push(@elf_pairs, \@pair);
	}
		
	return @elf_pairs;
}

