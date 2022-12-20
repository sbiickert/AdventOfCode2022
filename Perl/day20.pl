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

solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
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
	my $zero = mix_dll(\@dll);
	
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
	
	say "Part One: the sum of the coord values is $sum.";
}

sub solve_part_two {
	my @input = @_;
}

sub mix_dll {
	my $dll = shift;
	my $first_ptr = $dll->[0];
	my $zero;
	
	for my $i (0..$#{$dll}) {
		my $mover = $dll->[$i];
		
		my $diff = abs($mover->{'value'});
		if ($diff > 0) {
			# Track first_ptr
			if ($mover == $first_ptr) {
				$first_ptr = $mover->{'next'};
			}
			
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
		else {
			$zero = $mover;
		}
		
		#print_dll($dll, $first_ptr);
	}
	
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