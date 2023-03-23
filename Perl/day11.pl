#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day11_test.txt';
my $INPUT_FILE = 'day11_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 11: Monkey in the Middle";

use integer;

my @monkeys = parse_monkeys(@input);
my $mb = solve(20, 1, @monkeys);
say "Part One: the monkey business is $mb.";

@monkeys = parse_monkeys(@input); # Ensure at the starting state
$mb = solve(10000, 0, @monkeys);
say "Part Two: the monkey business is $mb.";

exit( 0 );

sub solve {
	my ($num_rounds, $has_relief, @monkeys) = @_;
	
	my $lcm = 1;
	for my $monkey (@monkeys) {
		$lcm *= $monkey->{'test'};
	}
	#say "LCM is $lcm";
	
	my $round = 1;
	while ($round <= $num_rounds) {
		for my $monkey (@monkeys) {
			while (scalar @{$monkey->{'items'}} > 0) {
				$monkey->{'count'} += 1;
				
				my $worry = shift(@{$monkey->{'items'}});
				#say "Item with worry $worry.";
				
				my $val;
				if ($monkey->{'val'} eq 'old')	{ $val = $worry; }
				else 							{ $val = int($monkey->{'val'}); }
				
				if ($monkey->{'op'} eq '+') {
					$worry += $val;
				}
				else {
					$worry *= $val;
				}
				#say "Worry increased to $worry.";
				
				# Relief
				if ($has_relief) {
					$worry = int($worry / 3);
				}
				else {
					$worry = $worry % $lcm;
				}
				#say "Relief: $worry.";
				
				if ($worry % $monkey->{'test'} == 0) {
					#say "Throwing to monkey $monkey->{'if'}.";
					push(@{$monkeys[$monkey->{'if'}]{'items'}}, $worry);
				}
				else {
					#say "Throwing to monkey $monkey->{'else'}.";
					push(@{$monkeys[$monkey->{'else'}]{'items'}}, $worry);
				}
			}
		}
		$round++;
	}
	
	my @counts = ();
	for my $monkey (@monkeys) {
		push(@counts, $monkey->{'count'});
		#say "Monkey $monkey->{'id'} inspected items $monkey->{'count'} times.";
	}
	@counts = sort {$b <=> $a} @counts;
	
	my $mb = $counts[0] * $counts[1];
	return $mb;
}

sub parse_monkeys {
	my @input = @_;
	
	my @monkeys = ();
	my $monkey = {};
	for my $line (@input) {
		if ($line =~ m/^Monkey (\d+)/) {
			$monkey->{'id'} = $1;
		}
		elsif ($line =~ m/Starting items: (.+)/) {
			my @arr = map(int, split(', ', $1));
			$monkey->{'items'} = \@arr;
		}
		elsif ($line =~ m/Operation: new = old ([\*\+]) (\w+)/) {
			$monkey->{'op'} = $1;
			$monkey->{'val'} = $2;
		}
		elsif ($line =~ m/Test: divisible by (.+)/) {
			$monkey->{'test'} = int($1);
		}
		elsif ($line =~ m/If true: throw to monkey (\d+)/) {
			$monkey->{'if'} = $1;
		}
		elsif ($line =~ m/If false: throw to monkey (\d+)/) {
			$monkey->{'else'} = $1;
			# Last line, save Monkey
			$monkey->{'count'} = 0;
			push(@monkeys, $monkey);
			$monkey = {};
		}
	}
	#print Dumper(@monkeys);
	return @monkeys;
}