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
#my $INPUT_FILE = 'day25_test.txt';
my $INPUT_FILE = 'day25_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 25: Full of Hot Air";

# my $test_snafu = "2=-01";
# my $decimal = snafu_to_decimal($test_snafu);
# say "Decimal value of $test_snafu is $decimal.";


solve_part_one(@input);
#solve_part_two(@input);

exit( 0 );

sub test_translation {
	my @input = @_;
	for my $line (@input) {
		my $decimal = snafu_to_decimal($line);
		my $snafu = decimal_to_snafu($decimal);
		say "$line --> $decimal --> $snafu";
		($line eq $snafu) or die;
	}
}

sub solve_part_one {
	my @input = @_;
	my $sum = 0;
	
	for my $line (@input) {
		my $decimal = snafu_to_decimal($line);
		my $snafu = decimal_to_snafu($decimal);
		#say "$line --> $decimal --> $snafu";
		($line eq $snafu) or die;
		$sum += $decimal;
	}
	
	say "The sum is $sum.";
	
	my $snafu = decimal_to_snafu($sum);
	my $decimal = snafu_to_decimal($snafu);
	#say "$num --> $snafu --> $decimal";
	($sum eq $decimal) or die;
	
	say "Part One: $sum in snafu is $snafu.";
}

sub solve_part_two {
	my @input = @_;
}

sub snafu_to_decimal {
	my $snafu = shift;
	my $dec = 0;
	
	my @s_digits = reverse split('', $snafu);
	
	for my $i (0..$#s_digits) {
		my $d = $s_digits[$i];
		if		($d eq '=') { $dec -= 2 * 5**$i; }
		elsif	($d eq '-') { $dec -= 1 * 5**$i; }
		elsif	($d == 2) 	{ $dec += 2 * 5**$i; }
		elsif	($d == 1) 	{ $dec += 1 * 5**$i; }
		# Zero-------
	}
	
	return $dec;
}

sub decimal_to_snafu {
	my $decimal = shift;
	my @s = ();
	
	my $i = 40;
	while ($i >= 0) {
		my $val = 5**$i;
		my $quo = int($decimal / $val);
		my $rem = $decimal % $val;
		push(@s, $quo);
		$decimal = $rem;
		$i--;
	}
	
	for ($i = 0; $i <= $#s; $i++) {
		my $temp = $s[$i] - 2;
		if ($temp > 0) {
			$s[$i] = -3 + $temp; # 2 -> -1, 1 -> -2
			$s[$i-1] ++;
			$i -= 2; # Go back, we might need to cascade the change
		}
	}
	
	for ($i = 0; $i <= $#s; $i++) {
		if ($s[$i] == -1) { $s[$i] = '-'; }
		elsif ($s[$i] == -2) { $s[$i] = '='; }
	}
	
	my $joined = join('', @s);
	$joined =~ s/^0+//;
	return $joined;
}