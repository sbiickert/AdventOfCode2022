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
say "Advent of Qode 2022, Day 07: Martians Among Us?";

open my $input, '<', 'day07.txt' or die "Failed to open input: $!";
my @INPUT = <$input>;
close $input;

solve(@INPUT);

exit( 0 );

sub solve {
	my @names = @_;
	my $count = 0;
	
	for my $orig (@names) {
		chomp $orig;
		my $name = lc($orig);
		$name =~ s/[^mars]//ig;
		if ($name eq 'mars') {
			#say "$orig is a martian.";
			$count++;
		}
	}
	
	say "There are $count martians.";
}
