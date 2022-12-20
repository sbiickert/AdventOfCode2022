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
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day20_test.txt';
my $INPUT_FILE = 'day20_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 20: Grove Positioning System";

my @linked_list = build_dll(@input);

my $p1 = solve(\@linked_list, 1);
say "Part One: the sum of the coord values is $p1.";

@linked_list = build_dll(@input);
for my $i (0..$#linked_list) {
	$linked_list[$i]{'value'} *= 811589153;
}
my $p2 = solve(\@linked_list, 10);
say "Part Two: the sum of the coord values is $p2.";

exit( 0 );

sub solve {
	my ($dll, $mix_count) = @_;
	
	my $zero;
	for my $rep (1..$mix_count) {
		$zero = mix_dll($dll);
	}
	
	my @coord = ();
	my $ptr = $zero;
	
	for my $i (1..3000) {
		$ptr = $ptr->{'next'};
		if ($i % 1000 == 0) { push(@coord, $ptr->{'value'}); }
	}
	
	say join(', ', @coord);
	my $sum = 0;
	for my $c (@coord) {
		$sum += $c;
	}
	return $sum;
}

sub build_dll {
	my @input = @_;
	my @dll = ();
	for my $line (@input) {
		push(@dll, {'value' => int($line)});
	}
	for my $i (0..$#dll) {
		my $prev = $i-1;
		my $next = $i+1;
		$prev = $#dll if ($prev < 0);
		$next = 0 if ($next >$#dll);
		$dll[$i]{'prev'} = $dll[$prev];
		$dll[$i]{'next'} = $dll[$next];
	}
	return @dll;
}

sub mix_dll {
	my $dll = shift;
	my $zero;
	my $list_length = scalar( @{$dll} ) -1;
	
	for my $i (0..$#{$dll}) {
		my $mover = $dll->[$i];
		if ($mover->{'value'} == 0) { $zero = $mover };
		
		my $diff = abs($mover->{'value'})  % $list_length;
		if ($diff > 0) {
			# Remove links to $mover
			$mover->{'prev'}{'next'} = $mover->{'next'};
			$mover->{'next'}{'prev'} = $mover->{'prev'};
			
			my $dest_prev = $mover;
			if ($mover->{'value'} > 0) {
				for my $j (1..$diff) { $dest_prev = $dest_prev->{'next'}; }
			}
			elsif ($mover->{'value'} < 0) {
				for my $j (1..$diff+1) { $dest_prev = $dest_prev->{'prev'}; }
			}
			my $dest_next = $dest_prev->{'next'};
		
			# Insert $mover
			$dest_prev->{'next'} = $mover;
			$dest_next->{'prev'} = $mover;
			$mover->{'prev'} = $dest_prev;
			$mover->{'next'} = $dest_next;
		}
	}	
	#print_dll($dll, $zero);
	
	return $zero;
}

sub print_dll {
	my ($dll, $first) = @_;
	my @values = ();
	my $ptr = $first;
	for my $i (0..$#{$dll}) {
		push(@values, $ptr->{'value'});
		$ptr = $ptr->{'next'};
	}
	say join(', ', @values);
}