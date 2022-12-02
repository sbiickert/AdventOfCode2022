#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2018;
use autodie;
use Data::Dumper;
#use Storable 'dclone';

use AOC::Util;
#use AOC::SpatialUtil;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day02_test.txt';
my $INPUT_FILE = 'day02_challenge.txt';
my @input = read_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 02: Rock Paper Scissors";

my @moves = parse_input(@input);

solve_part_one(@moves);
solve_part_two(@moves);

exit( 0 );

sub solve_part_one {
	my @moves = @_;
	
	my %score_lookup = ('X' => 1, 'Y' => 2, 'Z' => 3);
	my $score = 0;
	
	for my $move (@moves) {
		my $move_score = $score_lookup{$move->[1]};
		my $result_score = round_score($move);
		$score += ($move_score + $result_score);
	}
	
	say "Part One: your score is $score.";
}

sub solve_part_two {
	my @input = @_;
}

sub round_score {
	my $move = shift;
	
	# Draw
	if ($move->[0] eq $move->[1]) { return 3; }
	
	if ($move->[1] eq 'X') {
		# Rock X beats Scissors Z
		return $move->[0] eq 'Z' ? 6 : 0;
	}
	elsif ($move->[1] eq 'Y') {
		# Paper Y beats Rock X
		return $move->[0] eq 'X' ? 6 : 0;
	}
	# Scissors Z beats Paper Y
	return $move->[0] eq 'Y' ? 6 : 0;
}

sub parse_input {
	my @lines = @_;
	
	my %translation = ('A' => 'X', 'B' => 'Y', 'C' => 'Z');
	
	my @moves = ();
	for my $line (@lines) {
		my @move = split(' ', $line);
		# Making A,B,C => X,Y,Z
		$move[0] = $translation{$move[0]};
		push(@moves, \@move);
	}
	
	return @moves;
}