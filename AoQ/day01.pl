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

# https://adventofqode.org
say "Advent of Qode 2022, Day 01: What are the Odds?";

solve();

exit( 0 );

sub solve {
	my $LIMIT = 1000;
	my @odds = ();
	for my $i (1..$LIMIT) {
		push(@odds, $i) if $i % 2 == 1;
	}
	
	my $sum = 0;
	for (my $i = 0; $i <= $#odds; $i++) {
		$sum += $odds[$i] if $i % 2 == 0; # zero-indexed
	}
	say "Part One: the sum of odd odd numbers from 1 to $LIMIT is $sum.";
}
