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
my $INPUT_FILE = 'day16_test.txt';
#my $INPUT_FILE = 'day16_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 16: Proboscidea Volcanium";

my %graph = parse_graph(@input);

# (find_graph_distance(\%graph, 'AA', 'DD') == 1) or die;
# (find_graph_distance(\%graph, 'AA', 'CC') == 2) or die;
# (find_graph_distance(\%graph, 'AA', 'FF') == 3) or die;
# 
# die "Stopping";

solve_part_one(%graph);
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my %graph = @_;
	my $current = 'AA';
	my $time_remaining = 30;
	my $pressure_relieved = 0;
	
	while ($time_remaining > 0) {
		my %non_zero = ();
		for my $key (keys %graph) {
			if ($graph{$key}{'rate'} > 0 && !$graph{$key}{'open'}) {
				$non_zero{$key} = $graph{$key}{'rate'};
			}
		}
	
		my %target = ('key' => '', 'time' => 100, 'value' => 0);
		for my $other (keys %non_zero) {
			my ($time, $value) = calc_value(\%graph, $current,
											$other, $non_zero{$other},
											$time_remaining);
			if ($value > $target{'value'}) {
				$target{'key'} = $other;
				$target{'time'} = $time;
				$target{'value'} = $value;
			}
		}
		
		if ($target{'key'} ne '') {
			say "Moving from $current to $target{'key'} in $target{'time'} to gain $target{'value'}.";
			$current = $target{'key'};
			$time_remaining -= $target{'time'};
			$pressure_relieved += $target{'value'};
			$graph{$target{'key'}}{'open'} = 1;
		}
		else {
			$time_remaining = 0;
		}
	}
	
	say "Part One: the max pressure release is $pressure_relieved.";
}

sub solve_part_two {
	my @input = @_;
}

sub calc_value {
	my ($graph, $start, $end, $rate, $time_remaining) = @_;
	# +1 because opening the valve takes one minute
	my $time_to_reach = find_graph_distance($graph, $start, $end) + 1;
	# The amount of pressure released over the rest of the time
	# Can be negative if you can't reach before time expires
	my $p = ($time_remaining - $time_to_reach) * $rate;
	return ($time_to_reach, $p);
}

sub find_graph_distance {
	my ($graph, $start, $end) = @_;
	#say "finding distance from $start to $end.";
	my $result = 0;
	if ($start eq $end) { return $result; }
	
	my $found = 0;
	my @to_visit = @{$graph->{$start}{'leads_to'}};
	while(!$found) {
		$result++;
		my @next_to_visit = ();
		for my $neighbor (@to_visit) {
			if ( $neighbor eq $end ) {
				return $result;
			}
			push(@next_to_visit, @{$graph->{$neighbor}{'leads_to'}});
		}
		@to_visit = @next_to_visit;
	}
	
	return -1;
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