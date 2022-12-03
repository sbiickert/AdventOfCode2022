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
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day03_test.txt';
my $INPUT_FILE = 'day03_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 03: Rucksack Reorganization";

our $priority = {};
my @letters = 'a'..'z';
push( @letters, 'A'..'Z');

for (my $p = 0; $p <= $#letters; $p++) {
	$priority->{$letters[$p]} = $p+1;
}

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my @input = @_;
	
	my $priority_sum = 0;
	for my $line (@input) {
		my $halflen = length($line) / 2;
		my @parts = (substr($line, 0, $halflen), substr($line, $halflen));
		my @items = split('', $parts[0]);
		my $bFound = 0;
		for (my $i = 0; $i <= $#items; $i++) {
			if (index($parts[1], $items[$i]) > -1) {
				$priority_sum += $priority->{$items[$i]};
				#say "Common letter is " . $items[$i];
				$bFound = 1;
				last;
			}
		}
		die "Found no common letter in $line" if !$bFound;
	}
	
	say "Part One: the priority sum of common items is $priority_sum.";
}

sub sortItems {
	my $unsorted = shift;
	my $lower = $unsorted; my $upper = $unsorted;
	$lower =~ s/[A-Z]//g;
	$upper =~ s/[a-z]//g;
	my $sorted = join('', sort( split('', $lower) )) . join('', sort( split('', $upper) ));
	return $sorted;
}

sub solve_part_two {
	my @input = @_;
}
