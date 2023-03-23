#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2018;
use autodie;
use Data::Dumper;
use List::Util qw(uniqstr);
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
solve_part_two(@input);

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

sub solve_part_two {
	my @input = @_;
	
	my $priority_sum = 0;
	for (my $g = 0; $g <= $#input; $g+=3) {
		my @group = @input[$g..$g+2];
		my @uniq = uniqstr(split('', $group[0]));
		foreach my $i (1..2) {
			my @u = uniqstr(split('', $group[$i]));
			# intersection of @uniq and @u
			my %hash;
			@hash{@u} = (1) x @u;
			@uniq = grep {  $hash{$_} } @uniq;
		}
		# Should only be one letter left in @uniq
		(scalar(@uniq) == 1) or die "@uniq contained: " . join(', ', @uniq);
		$priority_sum += $priority->{$uniq[0]};
	}
	
	say "Part Two: the priority sum of group badges $priority_sum.";
}
