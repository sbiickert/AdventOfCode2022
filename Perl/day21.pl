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
#my $INPUT_FILE = 'day21_test.txt';
my $INPUT_FILE = 'day21_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 21: Monkey Math";

our %monkey_lookup;
build_lookup(@input);

solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	my $number = run_monkey_jobs('root');
	
	say "Part One: the result of the monkeys doing their jobs is $number.";
}

sub solve_part_two {
	my @inputs_to_root = split(' ', $monkey_lookup{'root'});
	
	my $humn = 0;
	my $search_size = 1024 * 1024 * 1024 * 1024;
	
	my $diff1 = difference_for_run($humn, $inputs_to_root[0], $inputs_to_root[2]);
	my $diff2 = difference_for_run($humn + $search_size, $inputs_to_root[0], $inputs_to_root[2]);
	# 1 if increasing humn results in positive diff change, -1 if opposite
	my $directionality = ($diff1 < $diff2) ? 1 : -1;

	my $diff = difference_for_run($humn, $inputs_to_root[0], $inputs_to_root[2]) * $directionality;
	my $search_direction = 1;
	$humn += $search_size;
	
	while ($diff != 0) {
		if ($diff < 0) {
			# Need larger humn
			$search_size /= 2 if $search_direction == -1; # turn around
			$humn += $search_size;
			$search_direction = 1;
		}
		elsif ($diff > 0) {
			# Need smaller humn
			$search_size /= 2 if $search_direction == 1; # turn around
			$humn -= $search_size;
			$search_direction = -1;
		}
		$diff = difference_for_run($humn, $inputs_to_root[0], $inputs_to_root[2]) * $directionality;
	}
	
	say "Part Two: the humn input was $humn.";
}

sub difference_for_run {
	my ($humn, $id1, $id2) = @_;
	#say "Setting humn to $humn.";
	$monkey_lookup{'humn'} = $humn;
	my $r1 = run_monkey_jobs($id1);
	my $r2 = run_monkey_jobs($id2);
	my $diff = $r1 - $r2;
	#say "$r1 - $r2 = $diff";
	return $diff;
}

sub run_monkey_jobs {
	my $id = shift;
	my $job = $monkey_lookup{$id};
	
	if ($job =~ m/^(\d+)$/) {
		return int($job);
	}
	my @parts = split(' ', $job);
	$parts[0] = run_monkey_jobs($parts[0]);
	$parts[2] = run_monkey_jobs($parts[2]);
	return eval join(' ', @parts);
}

sub build_lookup {
	my @input = @_;
	
	%monkey_lookup = ();
	for my $line (@input) {
		$line =~ m/(\w+): (.+)/;
		$monkey_lookup{$1} = $2;
	}
}