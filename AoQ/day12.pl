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
use Date::Calc qw(Add_Delta_Days);

# https://adventofqode.org
say "Advent of Qode 2022, Day 12: Blinky Park's Blighted 2nd Factor";

open my $input_file, '<', 'day12.txt' or die "Failed to open input: $!";
my @INPUT = <$input_file>;
close $input_file;

our @instructions = ();

# my @test = ("on 3x2", "rotate column x=2 by 1", "rotate row y=0 by 6");
# parse_instructions(@test);
# my @screen = init_screen(7,3);
parse_instructions(@INPUT);
my @screen = init_screen(40,6);
 
for my $i (@instructions) {
	if ($i->{'type'} eq 'on') {
		for my $c (0..$i->{'x'}-1) {
			for my $r (0..$i->{'y'}-1) {
				$screen[$r][$c] = '#';
			}
		}
	}
	else {
		if ($i->{'xy'} eq 'y') {
			my $row = $i->{'coord'};
			for my $count (1..$i->{'value'}) {
				my $temp = pop(@{$screen[$row]});
				unshift(@{$screen[$row]}, $temp);
			}
		}
		else {
			my $col = $i->{'coord'};
			my $temp = $screen[$#screen][$col];
			for my $count (1..$i->{'value'}) {
				for (my $row = $#screen; $row > 0; $row--) {
					$screen[$row][$col] = $screen[$row-1][$col];
				}
				$screen[0][$col] = $temp;
			}
		}
	}
}

print_screen(@screen);


exit( 0 );

sub print_screen {
	my @screen = @_;
	for my $row (@screen) {
		say join('', @{$row});
	}
}

sub parse_instructions {
	my @input = @_;
	
	for my $line (@input) {
		chomp $line;
		if ($line =~ m/on (\d+)x(\d+)/) {
			my $i = {'type' => 'on', 'x' => $1, 'y' => $2};
			push(@instructions, $i);
		}
		elsif ($line =~ m/rotate \w+ ([x|y])=(\d+) by (\d+)/) {
			my $i = {'type' => 'rotate', 'xy' => $1, 'coord' => $2, 'value' => $3};
			push(@instructions, $i);
		}
	}
}

sub init_screen {
	my ($cols, $rows) = @_;
	my @s = ();
	
	for my $r (0..$rows-1) {
		for my $c (0..$cols-1) {
			$s[$r][$c] = ".";
		}
	}
	return @s;
}