#!/usr/bin/env perl

BEGIN {
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $local_lib;

package AOC::Util;
use Modern::Perl 2018;
use Exporter;
use feature 'signatures';

our @ISA = qw( Exporter );
#our @EXPORT_OK = qw(a b c);
our @EXPORT = qw(read_input read_grouped_input);

# Read Input
sub read_input($input_file) {
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	
	while (my $line = <$input>) {
		chomp $line;
		push(@content, $line);
	}
	
	close $input;
	
	return @content;
}

# Read Grouped Input
sub read_grouped_input($input_file) {
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
	
	return @groups;
}

1;