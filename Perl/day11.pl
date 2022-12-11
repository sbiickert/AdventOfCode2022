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
#my $INPUT_FILE = 'day11_test.txt';
my $INPUT_FILE = 'day11_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 11: Monkey in the Middle";

use integer;

my @monkeys = parse_monkeys(@input);
solve_part_one(@monkeys);

@monkeys = parse_monkeys(@input); # Ensure at the starting state
solve_part_two(@monkeys);

exit( 0 );

sub solve_part_one {
	my @monkeys = @_;
	
	my $round = 1;
	while ($round <= 20) {
		for my $monkey (@monkeys) {
			while (scalar @{$monkey->{'items'}} > 0) {
				$monkey->{'count'} += 1;
				my $worry = shift(@{$monkey->{'items'}});
				#say "Item with worry $worry.";
				my $val = $monkey->{'val'};
				$val = $worry if $val eq 'old';
				if ($monkey->{'op'} eq '+') {
					$worry += $val;
				}
				else {
					$worry *= $val;
				}
				#say "Worry increased to $worry.";
				# Relief
				$worry = int($worry / 3);
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
		
# 		say "After round $round:";
# 		for (my $m = 0; $m <= $#monkeys; $m++) {
# 			my $monkey = $monkeys[$m];
# 			say "Monkey $m is holding: " . join(', ', @{$monkey->{'items'}});
# 		}		
		$round++;
	}
	
	my @counts = ();
	for my $monkey (@monkeys) {
		push(@counts, $monkey->{'count'});
		#say "Monkey $monkey->{'id'} inspected items $monkey->{'count'} times.";
	}
	@counts = sort {$b <=> $a} @counts;
	
	my $mb = $counts[0] * $counts[1];
	say "Part One: the monkey business is $mb.";
}

sub solve_part_two {
	my @monkeys = @_;
	
	my $lcm = 1;
	for my $monkey (@monkeys) {
		$lcm *= $monkey->{'test'};
	}
	say "LCM is $lcm";
	
	my $round = 1;
	while ($round <= 10000) {
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
				
				# Modified Relief
				$worry = $worry % $lcm;
				
				if ($worry % $monkey->{'test'} == 0) {
					#say "% $monkey->{'id'} throwing to $monkey->{'if'}.";
					push(@{$monkeys[$monkey->{'if'}]{'items'}}, $worry);
				}
				else {
					#say "$monkey->{'id'} throwing to $monkey->{'else'}.";
					push(@{$monkeys[$monkey->{'else'}]{'items'}}, $worry);
				}
			}
		}
		
# 		if ($round == 20 || $round % 1000 == 0) {
# 			say "After round $round:";
# 			for (my $m = 0; $m <= $#monkeys; $m++) {
# 				my $monkey = $monkeys[$m];
# 				say "Monkey $m inspected items $monkey->{'count'} times.";
# 			}
# 			#print Dumper(@monkeys);
# 		}	
		$round++;
	}
	
	my @counts = ();
	for my $monkey (@monkeys) {
		push(@counts, $monkey->{'count'});
	}
	@counts = sort {$b <=> $a} @counts;
	
	my $mb = $counts[0] * $counts[1];
	say "Part Two: the monkey business is $mb.";
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