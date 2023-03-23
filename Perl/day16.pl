#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2022;
use autodie;
use Data::Dumper;
use Math::Combinatorics qw(combine);
#use Storable 'dclone';
use List::Util qw( max );

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day16_test.txt';
my $INPUT_FILE = 'day16_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 16: Proboscidea Volcanium";

our %GRAPH = parse_graph(@input);

#solve_part_one();
solve_part_two();

exit( 0 );

our $TIME_LIMIT;
our $MAX_RELIEF;
our %CACHE;

sub solve_part_one {
	my $start = 'AA';
	%CACHE = ();
	$TIME_LIMIT = 30;
	$MAX_RELIEF = 0;

	my $max_pressure_relieved = dfs1('', $start, 0, 0);
	
	say "Part One: the max pressure release is $max_pressure_relieved.";
}

sub dfs1 {
	my $came_from = shift;
	my ($node_id, $time, $relief, @open_valves) = @_;
	my $cache_key = join('-', @_);
	
	if (exists($CACHE{$cache_key}))	{ return $CACHE{$cache_key}; }

	$time++;
	if ($time > $TIME_LIMIT) {
		if ($relief > $MAX_RELIEF) {
			say $relief;
			$MAX_RELIEF = $relief;
		}
		return $relief;
	}
	
	# Add rates of open valves to $relief
	for my $id (@open_valves) {
		$relief += $GRAPH{$id}{'rate'};
	}
	
	my $node = $GRAPH{$node_id};
	
	# Can either open valve or move
	my @reliefs = ();
	if ( $node->{'rate'} > 0 && (!grep( /^$node_id$/, @open_valves )) ) {
		push(@reliefs, dfs1($node_id, $node_id, $time, $relief, @open_valves, $node_id));
	}
	if ( $time < $TIME_LIMIT) { # Only move if time isn't up
		for my $neighbor (@{$GRAPH{$node_id}{'leads_to'}}) {
			if ($neighbor ne $came_from) { # Never backtrack
				push(@reliefs, dfs1($node_id, $neighbor, $time, $relief, @open_valves));
			}
		}
	}
	
	if (scalar @reliefs > 0) {
		$relief = max(@reliefs);
	}
	
	$CACHE{$cache_key} = $relief;
	return $relief;
}

our @ASSIGNED_JOBS;

sub solve_part_two {
	my $start = 'AA';
	$TIME_LIMIT = 26;
	
	my @non_zero = ();
	for my $key (keys %GRAPH) {
		push(@non_zero, $key) if ($GRAPH{$key}{'rate'} > 0);
	}
	my $count = scalar(@non_zero);
	
	my $job_size = ($count % 2 == 0) ? $count / 2 : int($count/2) + 1;
	my @human_assignments = combine($job_size, @non_zero);
	my $max_pressure_relieved = 0;
	for my $iter (0..$#human_assignments) {
		say "$iter / $#human_assignments ($max_pressure_relieved)";
		@ASSIGNED_JOBS = @{$human_assignments[$iter]};
		$MAX_RELIEF = 0;
		%CACHE = ();
		my $h_pressure_relieved = dfs3($start, $start, 0, 0);
		
		# Elephant does the rest
		my @e_assignments = ();
		for my $nz (@non_zero) {
			push(@e_assignments, $nz) if !grep( /^$nz$/, @ASSIGNED_JOBS );
		}
		@ASSIGNED_JOBS = @e_assignments;
		$MAX_RELIEF = 0;
		%CACHE = ();
		my $e_pressure_relieved = dfs3($start, $start, 0, 0);
		
		my $sum_pressure_relieved = $h_pressure_relieved + $e_pressure_relieved;
		say "sum: $sum_pressure_relieved = human: $h_pressure_relieved + elephant: $e_pressure_relieved";
		
		if ($sum_pressure_relieved > $max_pressure_relieved) {
			$max_pressure_relieved = $sum_pressure_relieved;
		}
	}
	
	say "Part Two: the max pressure release is $max_pressure_relieved.";
}

sub dfs3 {
	my $came_from = shift;
	my ($node_id, $time, $relief, @open_valves) = @_;
	my $cache_key = join('-', @_);
	
	if (exists($CACHE{$cache_key}))	{ return $CACHE{$cache_key}; }

	$time++;
	if ($time > $TIME_LIMIT) {
		if ($relief > $MAX_RELIEF) {
			#say $relief;
			$MAX_RELIEF = $relief;
		}
		return $relief;
	}
	
	# Add rates of open valves to $relief
	for my $id (@open_valves) {
		$relief += $GRAPH{$id}{'rate'};
	}
	
	my $node = $GRAPH{$node_id};
	
	# Can either open valve or move
	my @reliefs = ();
	if ( $node->{'rate'} > 0 && (!grep( /^$node_id$/, @open_valves )) ) {
		push(@reliefs, dfs3($node_id, $node_id, $time, $relief, @open_valves, $node_id)) if grep( /^$node_id$/, @ASSIGNED_JOBS );
	}
	if ( $time < $TIME_LIMIT) { # Only move if time isn't up
		for my $neighbor (@{$GRAPH{$node_id}{'leads_to'}}) {
			if ($neighbor ne $came_from) { # Never backtrack
				push(@reliefs, dfs3($node_id, $neighbor, $time, $relief, @open_valves));
			}
		}
	}
	
	if (scalar @reliefs > 0) {
		$relief = max(@reliefs);
	}
	
	$CACHE{$cache_key} = $relief;
	return $relief;
}

sub parse_graph {
	my @input = @_;
	my %graph = ();
	
	# Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
	for my $line (@input) {
		$line =~ m/Valve (\w+) .+ rate=(\d+); .+ valves? (.+)/;
		my @other_names = split(', ', $3);
		my $node = {'name' => $1, 'rate' => $2, 'leads_to' => \@other_names};
		
		$graph{$1} = $node;
	}
	
	return %graph;
}