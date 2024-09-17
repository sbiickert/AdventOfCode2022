#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../Input';
#my $INPUT_FILE = 'day07_test.txt';
my $INPUT_FILE = 'day07_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 7: No Space Left On Device";

class FSReader {...};

my $fsr = FSReader.new;
for @input -> $line {
	if $line ~~ m/ cd \s (<[ \w . / ]>+)/ {
		#say "$line --> $0";
		$fsr.cd($0.Str);
	}
	elsif $line ~~ m/^(\d+)/ {
		#say "$line --> $0";
		$fsr.add_file($0.Int);
	}
}

solve_part_one($fsr);
solve_part_two($fsr);

exit( 0 );

sub solve_part_one(FSReader $fsr) {
	my $sum = 0;
	for $fsr.dir_summary.keys -> $dir_path {
		my $size = $fsr.dir_summary{$dir_path};
		#say "$dir_path: " ~ $size;
		$sum += $size if $size <= 100000;
	}

	say "Part One: the total size of dirs <= 100000 is $sum.";
}

sub solve_part_two(FSReader $fsr) {
	my $TOTAL_SPACE := 70000000;
	my $NEEDED_SPACE := 30000000;

	my $avail_space = $TOTAL_SPACE - $fsr.dir_summary{'/'};
	my $to_clear = $NEEDED_SPACE - $avail_space;

	my $smallest_dir = '';
	my $smallest_size = $TOTAL_SPACE;
	
	for $fsr.dir_summary.keys -> $dir_path {
		my $size = $fsr.dir_summary{$dir_path};
		#say "$dir_path: " ~ $size;
		if $fsr.dir_summary{$dir_path} < $smallest_size &&
			$fsr.dir_summary{$dir_path} >= $to_clear {
			$smallest_dir = $dir_path;
			$smallest_size = $fsr.dir_summary{$dir_path};
		}
	}

	say "Part Two: the smallest dir that would clear enough space is $smallest_dir at $smallest_size.";
}

class FSReader {
	has Str @.current_path;
	has %.dir_summary;

	submethod TWEAK {
		@!current_path = ();
		%!dir_summary = Hash.new;
	}

	method joined_path(--> Str) {
		return @.current_path.join('/');
	}
	
	method current_and_all_parent_paths(--> Array) {
		my @paths = ();
		loop (my $i = @.current_path.end; $i >= 0; $i--) {
			my @subpath = @.current_path[0..$i];
			@paths.push(@subpath.join('/'));
		}
		return @paths;
	}

	method cd(Str $arg) {
		given $arg {
			when '/'  { @!current_path = ('/');  }
			when '..' { @!current_path.pop }
			default   { @!current_path.push($arg) }
		}
		my $p = self.joined_path;		
		if %.dir_summary{$p}:exists == False {
			%.dir_summary{$p} = 0;
		}
	}

	method add_file(Int $file_size) {
		my @paths = self.current_and_all_parent_paths;
		for @paths -> $p {
			%.dir_summary{$p} += $file_size;
		}
	}
}
