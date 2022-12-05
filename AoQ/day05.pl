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
say "Advent of Qode 2022, Day 05: Flummoxing Flighty Floors";

#my $INPUT = "((())()(())";
#my $INPUT = "2(2)";

open my $input, '<', 'day05.txt' or die "Failed to open input: $!";
my $INPUT = <$input>;
close $input;

solve($INPUT);

exit( 0 );

sub solve {
	my $input = shift;
	my @chars = split('', $input);
	my $floor = 0;
	
	for (my $c = 0; $c <= $#chars; $c++) {
		my $factor = 1;
		my $char = $chars[$c];
		if ( $char =~ m/\d/ ) {
			$factor = $char;
			$char = $chars[++$c];
		}
		if ($char eq '(') 		{ $floor += $factor * 1; }
		elsif ($char eq ')') 	{ $floor += $factor * -1; }
	}

	say "AoQ: the office is on floor " . $floor;
}
