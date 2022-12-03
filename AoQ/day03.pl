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

# https://adventofqode.org
say "Advent of Qode 2022, Day 03: What is their Primary motive?";

solve();

exit( 0 );

sub solve {
	#my $LIMIT = 29;
	my $LIMIT = 1000003;
	my @primes = (2,3);
	for my $i (4..$LIMIT) {
		push( @primes, $i ) if is_prime($i, \@primes);
		say $i if $i % 10000 == 0;
	}
	#say join(', ', @primes);
	say "AoQ: the number of prime numbers from 1 to $LIMIT is " . scalar(@primes);
}

sub is_prime {
	my ($num, $primes) = @_;
	foreach my $prime (@{$primes}) {
		if ($num % $prime == 0) { return 0; }
	}
	return 1;
}