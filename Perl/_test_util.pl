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

use AOC::Util;

my $INPUT_PATH = '../input';
my $INPUT_FILE = 'day00_test.txt';

test_read_input("$INPUT_PATH/$INPUT_FILE");
test_read_grouped_input("$INPUT_PATH/$INPUT_FILE");


sub test_read_input {
	say 'Testing reading input';
	my $input_file = shift;
	my @input = read_input($input_file);
	(scalar(@input) == 10) or die "Wrong number of lines";
	for my $line (@input) {
		say $line;
	}
	
	say 'Testing reading input, ignoring empty lines';
	@input = read_input($input_file, 1);
	(scalar(@input) == 8) or die "Wrong number of lines";
	for my $line (@input) {
		say $line;
	}
}

sub test_read_grouped_input {
	say 'Testing reading grouped input';
	my $input_file = shift;
	my @input = read_grouped_input($input_file);
	(scalar(@input) == 3) or die "Wrong number of groups";
	for my $group_ref (@input) {
		my @group = @{$group_ref};
		for my $line (@group) {
			say "$line";
		}
		say '----'
	}
	
	say 'Testing just reading group 1';
	my @group = read_grouped_input($input_file, 1);
	(scalar(@group) == 2) or die "Wrong number of lines in group 1";
	for my $line (@group) {
		say "$line";
	}
	
	say 'Testing just reading a group index out of range (10)';
	@group = read_grouped_input($input_file, 10);
	(!@group) or die "That should have been empty.";
	say "Successfully returned an empty array."
}

