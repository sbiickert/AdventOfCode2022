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
say "Advent of Qode 2022, Day 06: Martian Signal Sifter";

#my @INPUT = ("12230000000002", "12210000000003", "11110000000004", "12340000000000", "912121212121290000000009");

open my $input, '<', 'day06.txt' or die "Failed to open input: $!";
my @INPUT = <$input>;
close $input;

solve(@INPUT);

exit( 0 );

sub solve {
	my @streams = @_;
	
	# serial numbers are only on the challenge data
	my $streams_have_serial_numbers = (scalar(@streams) > 10);
	
	for my $stream (@streams) {
		chomp $stream;
		my $data = substr($stream, 0, length($stream)-10);
		my $checksum = substr($stream, length($stream)-10);
		
		my $serial_no = "test";
		if ($streams_have_serial_numbers) {
			$serial_no = substr($data, 0, 10);
			$data = substr($data, 10);
		}
		
		my $calculated = calc_checksum($data);
		
		if ($checksum == $calculated) {
			say "Checksum $checksum for $stream is correct.";
			say "Serial number is $serial_no.";
			last;
		}
	}
}

sub calc_checksum {
	my $data = shift;
	my @digits = split('', $data);
	
	my $checksum = 0;
	for (my $i = 0; $i <= $#digits; $i++) {
		my $next =  $i < $#digits ? $i+1 : 0;
		$checksum += $digits[$i] if $digits[$next] == $digits[$i];
	}
	
	return $checksum
}
