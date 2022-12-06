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
use List::Util qw(uniqstr);
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day06_test.txt';
my $INPUT_FILE = 'day06_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);

say "Advent of Code 2022, Day 06: Tuning Trouble";

my $signal = $input[0];

solve($signal);

exit( 0 );

sub solve {
	my $signal = shift;
	
	my $index1 = find_marker($signal, 4);
	my $index2 = find_marker($signal, 14);
	
	say "Part One: the packet marker started at $index1";
	say "Part Two: the message started at $index2";
}

sub find_marker {
	my $signal = shift;
	my $message_length = shift;
	my $i = $message_length;
	my @queue = split('', substr($signal, $i-$message_length, $i));
	
	my @u = uniqstr(@queue);
	while (scalar(@u) < $message_length && $i < length($signal)) {
		shift(@queue);
		push(@queue, substr($signal, $i, 1));
		@u = uniqstr(@queue);
		$i++;
	}
	
	return $i;
}