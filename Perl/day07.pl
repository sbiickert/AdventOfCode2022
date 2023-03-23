#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
}

use lib $directory;

use Modern::Perl 2018;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day07_test.txt';
my $INPUT_FILE = 'day07_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 07: No Space Left On Device";

my $tree = build_listing(@input);
my $dirs = [];
calc_size($tree, $dirs);

solve_part_one($dirs);
solve_part_two($tree, $dirs);

exit( 0 );

sub solve_part_one {
	my $dirs = shift;
	my $sum = 0; 
	
	for my $dir (@{$dirs}) {
		$sum += $dir->{'.'} if $dir->{'.'} <= 100000;
	}
	
	say "Part One: the sum of directory sizes under 100000 is $sum.";
}

sub solve_part_two {
	my ($tree, $dirs) = @_;
	my $TOTAL_SPACE = 70000000;
	my $REQ_SPACE = 30000000;
	my $avail_space = $TOTAL_SPACE - $tree->{'.'};
	my $minimum_to_clear = $REQ_SPACE - $avail_space;
	my $smallest = $TOTAL_SPACE; # just a large number

	for my $dir (@{$dirs}) {
		if ($dir->{'.'} < $smallest && $dir->{'.'} >= $minimum_to_clear) {
			$smallest = $dir->{'.'};
		}
	}
	
	say "Part Two: the smallest dir to delete has size $smallest.";
}

sub build_listing {
	my @input = @_;
	my $dir_tree = {};
	my $ptr;
	
	for my $line (@input) {
		#say $line;
		if ( $line =~ m/^\$ ?(.*)/ ) {
			# command
			if ($1 =~ m/cd (.+)/) {
				# change dir
				#say "Change dir to $1";
				if ( $1 eq '/' )		{ $ptr = $dir_tree; }
				else					{ $ptr = $ptr->{$1}; }
			}
			else {
				# ls - the following lines will be files and dirs
			}
		}
		elsif ( $line =~ m/^(\d+) (.+)/ ) {
			# file size and name
			#say "File $2 has size $1";
			$ptr->{$2} = $1;
		}
		elsif ( $line =~ m/dir (.+)/ ) {
			# dir a
			#say "Directory named $1";
			$ptr->{$1} = {'..' => $ptr};
		}
	}
	return $dir_tree;
}

sub calc_size {
	my ($dir, $out_dirs) = @_;
	my $size = 0;
	
	for my $key (keys %{$dir}) {
		if ( $key =~ m/^\./ ) {} # either . or .. --> ignore
		elsif ( ref $dir->{$key} ) {
			$size += calc_size($dir->{$key}, $out_dirs);
		}
		else {
			$size += $dir->{$key};
		}
	}
	$dir->{'.'} = $size;
	push( @{$out_dirs}, $dir );
	return $size;
}