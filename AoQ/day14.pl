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
use List::Util qw(uniq);
use List::MoreUtils qw(first_index);

# https://adventofqode.org
say "Advent of Qode 2022, Day 14: Robots Games";

play(47);

exit(0);

sub play {
	my $rounds = shift;
	my @state = (1);
	my $time = 2;

	for my $round (2..$rounds) {
		@state = process(@state);
		$time += scalar(@state) * 2;
		$time += 5; # if $round < $rounds;
	}

	say "The game took $time seconds to play.";
}

sub process {
	my @state = @_;
	my @result = ();
	my $d = 0;
	my $count = 0;
	
	for my $i (0..$#state) {
		if ($state[$i] != $d) {
			push(@result, $count, $d) if ($count > 0);
			$d = $state[$i];
			$count = 0;
		}
		$count++;
	}
	push(@result, $count, $d);
	
	say join('', @state) . ' -> ' . join('', @result);
	return @result;
}