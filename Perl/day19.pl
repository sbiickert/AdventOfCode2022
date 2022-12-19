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
my $INPUT_FILE = 'day19_test.txt';
#my $INPUT_FILE = 'day19_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 19: Not Enough Minerals";

our @blueprints = parse_blueprints(@input);
our @RT = ('ore', 'clay', 'obsidian', 'geode');

solve_part_one();
#solve_part_two(@input);

exit( 0 );

sub solve_part_one {
	my %quality = ();
	for my $bp (@blueprints) {
		$quality{$bp->{'id'}} = eval_blueprint($bp);
	}
	
	my $sum = 0;
	for my $id (keys %quality) {
		$sum += $quality{$id};
	}
	say "Part One: the sum of quality levels is $sum.";
}


sub solve_part_two {
	my @input = @_;
}

our %cache;
our @cache_keys;

sub eval_blueprint {
	my $bp = shift;
	my %args = ('build' => '', 'bp' => $bp, 'time' => 24);
	for my $r_type (@RT) {
		$args{$r_type} = 0;
		$args{"r:$r_type"} = 0;
	}
	$args{'r:ore'} = 1;
	
	%cache = ();
	@cache_keys = grep { !/bp/ } sort keys %args;
	say join(' ', @cache_keys);
	my $geode_count = process(%args);
	
	my $quality_level = $bp->{'id'} * $geode_count;
	say "Quality Level for $bp->{'id'} is $quality_level.";
	return $quality_level;
}

sub process {
	my %args = @_;
	$args{'time'} --;
	
	if ($args{'time'} < 0) {
		return 0;
	}
	
	# read cache result here
	my $cache_key = join('|', map { $args{$_} } @cache_keys);
	if (exists( $cache{$cache_key} )) {
		#say $cache_key;
		return $cache{$cache_key};
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
	
	# Produce
	for my $r_type (@RT) {
		my $robot_type = "r:$r_type";
		for my $i (1..$args{$robot_type}) {
			$args{$r_type}++;
		}
	}
	
	my @avail_robots = what_can_i_build(%args);
	push( @avail_robots, '' ); # not building is an option
	#say "Can do one of: " . join(', ', @avail_robots);
	
	my @results = ();
	
	for my $build (@avail_robots) {
		$args{'build'} = $build;
		push(@results, process(%args));
	} 
	
	my $max_geodes = max(@results) + $args{'geode'};
	
	# write cache here
	$cache{$cache_key} = $max_geodes;
	
	return $max_geodes;
}

sub what_can_i_build {
	my %args = @_;
	
	my @buildable = ();
	for my $r_type (@RT) {
		my $robot_type = "r:$r_type";
		my $recipe = $args{'bp'}{$robot_type};
		my $have_resources = 1;
		for my $resource_quantity (@{$recipe}) {
			my $resource_amt = $resource_quantity->[0];
			my $resource_type = $resource_quantity->[1];
			$have_resources &= ($args{$resource_type} >= $resource_amt);
		}
		push(@buildable, $robot_type) if ($have_resources);
	}
	
	return @buildable;
}

sub parse_blueprints {
	my @input = @_;
	my @blueprints = ();
	
	for my $line (@input) {
		my %bp = ();
		$line =~ m/(\d+): (.+)/;
		$bp{'id'} = $1;
		for my $cost_desc (split(' Each', $2)) {
			$cost_desc =~ m/(\w+) robot costs (.+)/;
			my $r_type = $1;
			my @ingredients = ();
			for my $i_desc (split(' and ', $2)) {
				$i_desc =~ m/(\d+) (\w+)/;
				push(@ingredients, [$1, $2]);
			}
			$bp{"r:$r_type"} = \@ingredients;
		}
		push(@blueprints, \%bp);
	}
	
	#print Dumper(@blueprints);
	return @blueprints;
}