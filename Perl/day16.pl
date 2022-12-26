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

sub solve_part_two {
	my $start = 'AA';
	%CACHE = ();
	$TIME_LIMIT = 26;
	$MAX_RELIEF = 0;

	my $max_pressure_relieved = dfs2('', '', $start, $start, 0, 0);
	
	say "Part Two: the max pressure release is $max_pressure_relieved.";
}

sub dfs2 {
	my $human_came_from = shift;
	my $elephant_came_from = shift;
	my ($h_node_id, $e_node_id, $time, $relief, %open_valves) = @_;
	
	# No difference if it's the human at the spot or elephant
	my $cache_key1 = join('-', $h_node_id, $e_node_id, $time, $relief, sort keys %open_valves);
	my $cache_key2 = join('-', $e_node_id, $h_node_id, $time, $relief, sort keys %open_valves);
	if (exists($CACHE{$cache_key1}))	{ return $CACHE{$cache_key1}; }
	if (exists($CACHE{$cache_key2}))	{ return $CACHE{$cache_key2}; }

	$time++;
	if ($time > $TIME_LIMIT) {
		if ($relief > $MAX_RELIEF) {
			say $relief;
			$MAX_RELIEF = $relief;
		}
		return $relief;
	}
	
	# Add rates of open valves to $relief
	for my $id (keys %open_valves) {
		$relief += $GRAPH{$id}{'rate'};
	}
	
	my $h_node = $GRAPH{$h_node_id};
	my $e_node = $GRAPH{$e_node_id};
	
	# The human and elephant can either open valve or move
	my @h_moves = ();
	if ( $h_node->{'rate'} > 0 && (!grep( /^$h_node_id$/, keys %open_valves )) ) {
		push(@h_moves, "open $h_node_id");
	}
	if ( $time < $TIME_LIMIT) { # Only move if time isn't up
		for my $neighbor (@{$GRAPH{$h_node_id}{'leads_to'}}) {
			if ($neighbor ne $human_came_from) { # Never backtrack
				push(@h_moves, "move $neighbor");
			}
		}
	}
	if (scalar @h_moves == 0) { @h_moves = ("stay $h_node_id"); }
	
	my @e_moves = ();
	if ( $e_node->{'rate'} > 0 && (!grep( /^$e_node_id$/, keys %open_valves )) ) {
		push(@e_moves, "open $e_node_id");
	}
	if ( $time < $TIME_LIMIT) { # Only move if time isn't up
		for my $neighbor (@{$GRAPH{$e_node_id}{'leads_to'}}) {
			if ($neighbor ne $elephant_came_from) { # Never backtrack
				push(@e_moves, "move $neighbor");
			}
		}
	}
	if (scalar @e_moves == 0) { @e_moves = ("stay $e_node_id"); }
	
	my @reliefs = ();
	for my $h_move (@h_moves) {
		my ($h_action, $h_destination_id) = split(' ', $h_move);
		for my $e_move (@e_moves) {
			my ($e_action, $e_destination_id) = split(' ', $e_move);
			my %new_open_valves = %open_valves;
			if ($h_action eq 'open') { $new_open_valves{$h_node_id} = 1; }
			if ($e_action eq 'open') { $new_open_valves{$e_node_id} = 1; }
			
			push(@reliefs, dfs2($h_node_id, $e_node_id, 
								$h_destination_id, $e_destination_id,
								$time, $relief, %new_open_valves));
		}
	}
	
	if (scalar @reliefs > 0) {
		$relief = max(@reliefs);
	}
	
	$CACHE{$cache_key1} = $relief;
	$CACHE{$cache_key2} = $relief;
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