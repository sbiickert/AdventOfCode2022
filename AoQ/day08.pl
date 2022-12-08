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

# https://adventofqode.org
say "Advent of Qode 2022, Day 08: Pondering Passphrases";

open my $input, '<', 'day08.txt' or die "Failed to open input: $!";
my @INPUT = <$input>;
close $input;

solve(@INPUT);

exit( 0 );

sub solve {
	my @passphrases = @_;
	my $count = 0;
	
	for my $phrase (@passphrases) {
		chomp $phrase;
		
		my $altered = lc($phrase);
		next if ($altered ne $phrase);
		
		$altered =~ s/[^a-z ]//g;
		next if ($altered ne $phrase);
		
		my @words = split(/ /, $phrase);
		my %lookup = ();
		for my $word (@words) {
			$lookup{$word} = 1;
		}
		next if (scalar(keys(%lookup)) ne scalar(@words));
		
		# At this point, the phrase has passed the tests
		$count++;
	}
	
	say "There are $count valid passphrases.";
}
