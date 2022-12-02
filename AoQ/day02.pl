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
use List::Util qw(sum);

# https://adventofqode.org
say "Advent of Qode 2022, Day 02: Even If we Answer this Correctly...";

solve();

exit( 0 );

sub solve {
	my $LIMIT = 1000;
	my @evens_with_4 = ();
	for my $i (1..$LIMIT) {
		if (($i % 2 == 0) && (index($i, '4') != -1)) {
			push( @evens_with_4, $i );
		}
	}
	
	my $sum = sum(@evens_with_4);
	say "AoQ: the sum of even numbers with a 4 in them from 1 to $LIMIT is $sum.";
}
