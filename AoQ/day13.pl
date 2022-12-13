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
say "Advent of Qode 2022, Day 13: The North Pole Trust. Yes it's a real bank!";

open my $input_file, '<', 'day13.txt' or die "Failed to open input: $!";
my @INPUT = <$input_file>;
close $input_file;

my @martian_log_lines = grep(/Mary Willis \| SUCCESS/, @INPUT);
#say join('', @martian_log_lines);

#my %after = ();
my %before = ();

for my $line (@martian_log_lines) {
	chomp $line;
	$line =~ m/(\d{3})$/;
	my @digits = split('', $1);
	
# 	for my $i (0..1) {
# 		my $d = $digits[$i];
# 		if (!defined $after{$d}) {
# 			$after{$d} = [];
# 		}
# 		for my $j ($i+1..2) {
# 			push( @{$after{$d}}, $digits[$j]);
# 		}
# 	}
	for my $i (1..2) {
		my $d = $digits[$i];
		if (!defined $before{$d}) {
			$before{$d} = [];
		}
		for my $j (0..$i-1) {
			push( @{$before{$d}}, $digits[$j]);
		}
	}
}

for my $d (keys %before) {
	my @temp = @{$before{$d}};
	@temp = uniq(@temp);
	$before{$d} = \@temp;
}
# for my $d (keys %after) {
# 	my @temp = @{$after{$d}};
# 	@temp = uniq(@temp);
# 	$after{$d} = \@temp;
# }

print Dumper(\%before);
#print Dumper(\%after);

# Working from the print (backwards and forwards), the order is 
#3769015482
#3769015482
# I worked through the input to check it. Didn't find anything violating this.
# Don't seem to be getting any points, though.