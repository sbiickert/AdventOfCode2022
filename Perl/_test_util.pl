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

use AOC::Util;

my $INPUT_PATH = '../input';
my $INPUT_FILE = 'day00_test.txt';

test_read_input("$INPUT_PATH/$INPUT_FILE");
test_read_grouped_input("$INPUT_PATH/$INPUT_FILE");


sub test_read_input {
	say 'Testing reading input';
	my $input_file = shift;
	my @input = read_input($input_file);
	for my $line (@input) {
		say $line;
	}
}

sub test_read_grouped_input {
	say 'Testing reading grouped input';
	my $input_file = shift;
	my @input = read_grouped_input($input_file);
	for my $group_ref (@input) {
		my @group = @{$group_ref};
		for my $line (@group) {
			say "$line";
		}
		say '----'
	}
}

