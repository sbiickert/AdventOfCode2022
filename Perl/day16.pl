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
use Math::Combinatorics;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day16_test.txt';
my $INPUT_FILE = 'day16_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 16: Proboscidea Volcanium";

my %graph = parse_graph(@input);

# (find_graph_distance(\%graph, 'AA', 'DD') == 1) or die;
# (find_graph_distance(\%graph, 'AA', 'CC') == 2) or die;
# (find_graph_distance(\%graph, 'AA', 'FF') == 3) or die;
# 
#die "Stopping";

solve_part_one(%graph);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my %graph = @_;
	my $current = 'AA';
	
	my @non_zero = ();
	for my $key (keys %graph) {
		if ($graph{$key}{'rate'} > 0 && !$graph{$key}{'open'}) {
			push(@non_zero, $key);
		}
	}
	
	my @order = ();
	my @others = @non_zero;
	my $max_pressure_relieved = 0;
	my @max_order;
	
	while (scalar(@non_zero) > 0) {
		my $least = 100000000;
		my $least_key = '';
		for my $key (@non_zero) {
			my $total_pressure_relieved = 0;
			my @others = grep { !/$key/ } @non_zero;
			my $append_order = 0;
			my $combo_length = scalar(@order);
			if ($combo_length > scalar(@others)) {
				$combo_length = scalar(@others);
				$append_order = 1;
			}
			my @combinations = combine($combo_length, @others);
			if (scalar @combinations > 0) {
				for my $combo (@combinations) {
					for my $arr (permute @{$combo}) {
						my @to_check = @{$arr};
						push( @to_check, @order ) if $append_order;
						my $pressure_relieved = calc_for_order($current, $key, @to_check);
						$total_pressure_relieved += $pressure_relieved;
						if ($pressure_relieved > $max_pressure_relieved) {
							@max_order = @to_check;
							unshift(@max_order, $key);
							$max_pressure_relieved = $pressure_relieved;
							say "Best $max_pressure_relieved";
						}
					}
				}
			}
			else {
				$total_pressure_relieved += calc_for_order($current, $key, @order);
			}
			if ($total_pressure_relieved < $least) {
				$least = $total_pressure_relieved;
				$least_key = $key;
			}
		}
		@non_zero = grep { !/$least_key/ } @non_zero;
		unshift(@order, $least_key);
		say join(', ', @order);
	}
	
	# Try topping up with small ones
	# This worked. By adding "CU" (an insignificant one) back in.
	# AA, OJ, XF, OK, WY, YV, JM, MQ, CU... after this we're over 30 mins
	for (my $i = -1; $i > -7; $i--) { 
		calc_for_order($current, @max_order, $order[$i]);
	}

	#my @right_order = ('AA', 'DD', 'BB', 'JJ', 'HH', 'EE', 'CC');
	
	
	#1268 too low
	#1701 too low
	#1981 too low
	#1984 too low
	#2011 wrong
	#right answer 1986
	
	say "Part One: the max pressure release is $max_pressure_relieved.";
}

sub calc_for_order {
	my $current = shift;
	my @order = @_;
	my $time_remaining = 30;
	my $pressure_relieved = 0;
	
	for my $other (@order) {
		my ($time, $value) = calc_value(\%graph, $current, $other, $time_remaining);
		#say "Moving from $current to $other in $time to gain $value.";
		$current = $other;
		last if ($value <= 0);
		$time_remaining -= $time;
		$pressure_relieved += $value;
	}
	
	say "calc_for_order " . join(', ', @order) . " -> $pressure_relieved";
	return $pressure_relieved;
}

sub solve_part_two {
	my @input = @_;
}

sub calc_value {
	my ($graph, $start, $end, $time_remaining) = @_;
	# +1 because opening the valve takes one minute
	my $time_to_reach = find_graph_distance($graph, $start, $end) + 1;
	# The amount of pressure released over the rest of the time
	# Can be negative if you can't reach before time expires
	my $p = ($time_remaining - $time_to_reach) * $graph->{$end}{'rate'};
	return ($time_to_reach, $p);
}

our %graph_distances = ();
sub find_graph_distance {
	my ($graph, $start, $end) = @_;
	#say "finding distance from $start to $end.";
	my $result = 0;
	if ($start eq $end) { return $result; }
	
	if (!exists $graph_distances{"$start $end"}) {
		my @to_visit = @{$graph->{$start}{'leads_to'}};
		my $safety = 0;
		while(++$safety < 10000) {
			$result++;
			my @next_to_visit = ();
			die if scalar @to_visit == 0;
			for my $neighbor (@to_visit) {
				if ( $neighbor eq $end ) {
					$graph_distances{"$start $end"} = $result;
					$graph_distances{"$end $start"} = $result;
					return $result;
				}
				push(@next_to_visit, @{$graph->{$neighbor}{'leads_to'}});
			}
			@to_visit = @next_to_visit;
		}
		die if $safety >= 9999;
	}
	return $graph_distances{"$start $end"};
}

sub parse_graph {
	my @input = @_;
	my %graph = ();
	
	# Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
	for my $line (@input) {
		$line =~ m/Valve (\w+) .+ rate=(\d+); .+ valves? (.+)/;
		my @other_names = split(', ', $3);
		my $node = {'name' => $1, 'rate' => $2, 'open' => 0, 'leads_to' => \@other_names};
		
		$graph{$1} = $node;
	}
	
	return %graph;
}