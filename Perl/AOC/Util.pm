#!/usr/bin/env perl

BEGIN {
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $local_lib;

package AOC::Util;
use Modern::Perl 2022;
use Exporter;
use feature 'signatures';

our @ISA = qw( Exporter );
#our @EXPORT_OK = qw(a b c);
our @EXPORT = qw(read_input read_grouped_input);

# Read Input
sub read_input($input_file, $remove_empty_lines = 0) {
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		push(@content, $line) unless (length($line) == 0 && $remove_empty_lines);
	}
	
	close $input;
	
	return @content;
}

# Read Grouped Input

# If $group_index is not given or negative, then all
# groups are returned as an array of array refs.

# If $group_index is a valid index for a group, then
# only the lines of that group are returned as an array of strings.

# If $group_index is out of range, then an empty array is returned.
sub read_grouped_input($input_file, $group_index = -1) {
	my @content = read_input($input_file);
	my @groups = ();
	my $g_ref = [];
	
	for my $line (@content) {
		if ($line eq '') {
			push( @groups, $g_ref );
			$g_ref = [];
		}
		else {
			push(@{$g_ref}, $line);
		}
	}
	
	if (scalar(@{$g_ref}) > 0) {
		push( @groups, $g_ref );
	}
	
	if ($group_index >= 0) {
		if ($group_index <= $#groups) {
			return @{$groups[$group_index]};
		}
		return ();
	}
	
	return @groups;
}

1;