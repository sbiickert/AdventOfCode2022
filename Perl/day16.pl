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
#use Math::Combinatorics;
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

solve_part_one();
#solve_part_two(@input);

exit( 0 );

our $TIME_LIMIT;
our $MAX_RELIEF;
our %CACHE;
sub solve_part_one {
	my $start = 'AA';
	%CACHE = ();
	$TIME_LIMIT = 30;
	$MAX_RELIEF = 0;

	my $max_pressure_relieved = dfs('', $start, 0, 0);
	
	say "Part One: the max pressure release is $max_pressure_relieved.";
}

sub dfs {
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
		push(@reliefs, dfs($node_id, $node_id, $time, $relief, @open_valves, $node_id));
	}
	if ( $time < $TIME_LIMIT) { # Only move if time isn't up
		for my $neighbor (@{$GRAPH{$node_id}{'leads_to'}}) {
			if ($neighbor ne $came_from) { # Never backtrack
				push(@reliefs, dfs($node_id, $neighbor, $time, $relief, @open_valves));
			}
		}
	}
	
	if (scalar @reliefs > 0) {
		$relief = max(@reliefs);
	}
	
	$CACHE{$cache_key} = $relief;
	return $relief;
}

sub solve_part_two {
	my @input = @_;
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