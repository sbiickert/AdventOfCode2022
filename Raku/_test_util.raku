#!/usr/bin/env raku

use lib $*PROGRAM.dirname;
use AOC::Util;

my $INPUT_PATH = '../input';
my $INPUT_FILE = 'day00_test.txt';

test_read_input("$INPUT_PATH/$INPUT_FILE");
test_read_grouped_input("$INPUT_PATH/$INPUT_FILE");

sub test_read_input($input_file) {
	say 'Testing reading input';
	my @input = read_input($input_file);
	(@input.elems == 10) or die "Wrong number of lines";
	.say for @input;
	
	say 'Testing reading input, ignoring empty lines';
	@input = read_input($input_file, True);
	(@input.elems == 8) or die "Wrong number of lines";
	.say for @input;
}

sub test_read_grouped_input($input_file) {
	say 'Testing reading grouped input';
	my @input = read_grouped_input($input_file);
	(@input.elems == 3) or die "Wrong number of groups";
	for @input -> @group {
		.say for @group;
		say '----'
	}
	
	say 'Testing just reading group 1';
	my @group = read_grouped_input($input_file, 1);
	say @group;
	(@group.elems == 2) or die "Wrong number of lines in group 1";
	.say for @group;
	
	say 'Testing just reading a group index out of range (10)';
	@group = read_grouped_input($input_file, 10);
	(!@group) or die "That should have been empty.";
	say "Successfully returned an empty array."
}

