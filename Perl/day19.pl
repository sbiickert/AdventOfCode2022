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
use List::Util qw( max );

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day19_test.txt';
my $INPUT_FILE = 'day19_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 19: Not Enough Minerals";

our @blueprints = parse_blueprints(@input);
our @RT = ('geode', 'obsidian', 'clay', 'ore');

#solve_part_one();
solve_part_two();

exit( 0 );

sub solve_part_one {
	my %quality = ();
	for my $bp (@blueprints) {
		#if ($bp->{'id'} == 1) {
			my $geode_count = eval_blueprint($bp, 24);
			$quality{$bp->{'id'}} = $bp->{'id'} * $geode_count;
			say "Q level of blueprint $bp->{'id'} is $quality{$bp->{'id'}}.";
		#}
	}
	
	my $sum = 0;
	for my $id (keys %quality) {
		$sum += $quality{$id};
	}
	say "Part One: the sum of quality levels is $sum.";
}


sub solve_part_two {
	my %count = ();
	for my $bp (@blueprints) {
		$count{$bp->{'id'}} = eval_blueprint($bp, 32) if $bp->{'id'} <= 3;
	}
	
	my $score = 1;
	for my $id (keys %count) {
		$score *= $count{$id};
	}
	say "Part Two: the product of geode counts is $score.";
}

our %cache;
our @cache_keys;
our $best_geode_count;

sub eval_blueprint {
	my ($bp, $time) = @_;
	my %args = ('build' => '', 'bp' => $bp, 'time' => $time);
	for my $r_type (@RT) {
		$args{$r_type} = 0;
		$args{"r:$r_type"} = 0;
	}
	$args{'r:ore'} = 1;
	
	%cache = ();
	@cache_keys = grep { !/bp/ } sort keys %args;
	$best_geode_count = 0;
	
	my $geode_count = process(%args);
	
	say "Geodes produced in $time by blueprint $bp->{'id'} is $geode_count.";
	return $geode_count;
}

sub process {
	my %args = @_;
	$args{'time'} --;
	
	if ($args{'time'} < 0) {
		#say join('|', map { $args{$_} } @cache_keys);
		if ($args{'geode'} > $best_geode_count) {
			$best_geode_count = $args{'geode'};
			say "Best is $best_geode_count.";
		}
		return $args{'geode'};
	}
	
	# read cache result here
	my $cache_key = join('|', map { $args{$_} } @cache_keys);
	if (exists( $cache{$cache_key} )) {
		#say $cache_key;
		return $cache{$cache_key};
	}
	
	# Optimization: can this branch ever exceed the best_geode_count?
	my $t = $args{'time'} + 1;
	my $theoretical_best = $args{'geode'} + ($args{'r:geode'} * $t) + (($t * ($t - 1)) / 2);
	if ($theoretical_best < $best_geode_count) {
		return $args{'geode'};
	}
	
	# Would theoretically start the build here, but since it
	# comes online _after_ production, build happens after.
	
	# Produce
	for my $r_type (@RT) {
		my $robot_type = "r:$r_type";
		for my $i (1..$args{$robot_type}) {
			$args{$r_type}++;
		}
	}
	
	# Do any passed build action (we will have the resources for it)
	if ($args{'build'}) {
		my $robot_type = $args{'build'};
		my $recipe = $args{'bp'}{$robot_type};
		for my $resource_quantity (@{$recipe}) {
			my $resource_amt = $resource_quantity->[0];
			my $resource_type = $resource_quantity->[1];
			$args{$resource_type} -= $resource_amt;
			($args{$resource_type} >= 0) or die "Negative $resource_type";
		}
		$args{$robot_type}++;	
	}
	
	my @avail_robots = what_can_i_build(\%args);
	
	my @results = ();
	for my $build (@avail_robots) {
		$args{'build'} = $build;
		push(@results, process(%args));
	} 
	
	my $max_geodes = max(@results);
	
	# write cache here
	if ($args{'time'} > 1) { # limiting cache size, was hitting 16 GB.
		$cache{$cache_key} = $max_geodes;
	}
	
	return $max_geodes;
}

sub what_can_i_build {
	my $args_ref = shift;
		
	# No point in building robot, won't produce anything before end
	return ('') if $args_ref->{'time'} == 0;

	my @buildable = ('');
	
	for my $r_type (@RT) {
		my $robot_type = "r:$r_type";
		
		# Don't need more ore/clay/obsidian robots, if we have more than can be consumed in a build
		next if ($r_type eq 'ore' && $args_ref->{$robot_type} >= $args_ref->{'bp'}{'max_ore'});
		next if ($r_type eq 'clay' && $args_ref->{$robot_type} >= $args_ref->{'bp'}{'max_clay'});
		next if ($r_type eq 'obsidian' && $args_ref->{$robot_type} >= $args_ref->{'bp'}{'max_obsidian'});
		
		my $recipe = $args_ref->{'bp'}{$robot_type};
		my $have_resources = 1;
		for my $resource_quantity (@{$recipe}) {
			my $resource_amt = $resource_quantity->[0];
			my $resource_type = $resource_quantity->[1];
			$have_resources &= ($args_ref->{$resource_type} >= $resource_amt);
		}
		push(@buildable, $robot_type) if ($have_resources);
	}
	
	# If we are at time = 1, there are only 2 turns left
	# There is no point to building anything except a geode cracker
	if ($args_ref->{'time'} == 1) {
		@buildable = grep { /geode/ } @buildable;
	}
	
	if (scalar(@buildable) == 0) {
		# Do nothing as a last resort
		push(@buildable, '');
	}
	
	return @buildable;
}

sub parse_blueprints {
	my @input = @_;
	my @blueprints = ();
	
	
	for my $line (@input) {
		my %bp = ();
		my $max_ore = 0;
		my $max_clay = 0;
		my $max_obs = 0;
		
		$line =~ m/(\d+): (.+)/;
		$bp{'id'} = $1;
		
		for my $cost_desc (split(' Each', $2)) {
			$cost_desc =~ m/(\w+) robot costs (.+)/;
			my $r_type = $1;
			my @ingredients = ();
			for my $i_desc (split(' and ', $2)) {
				$i_desc =~ m/(\d+) (\w+)/;
				push(@ingredients, [$1, $2]);
				$max_clay = $1 if $2 eq 'clay' && $1 > $max_clay;
				$max_ore = $1 if $2 eq 'ore' && $1 > $max_ore;
				$max_obs = $1 if $2 eq 'obsidian' && $1 > $max_obs;
			}
			$bp{"r:$r_type"} = \@ingredients;
		}
		
		$bp{'max_ore'} = $max_ore;
		$bp{'max_clay'} = $max_clay;
		$bp{'max_obsidian'} = $max_obs;
		
		push(@blueprints, \%bp);
	}
	
	#print Dumper(@blueprints);
	return @blueprints;
}