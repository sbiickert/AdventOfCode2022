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
#my $INPUT_FILE = 'day05_test.txt';
my $INPUT_FILE = 'day05_challenge.txt';
my @stack_input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 0);
my @instr_input = read_grouped_input("$INPUT_PATH/$INPUT_FILE", 1);

say "Advent of Code 2022, Day 05: Supply Stacks";

my @stacks = parse_stacks(@stack_input);
my @instructions = parse_instructions(@instr_input);

solve_part_one(\@stacks, \@instructions);

# Reset stacks
@stacks = parse_stacks(@stack_input);
solve_part_two(\@stacks, \@instructions);

exit( 0 );

sub solve_part_one {
	my ($s_ref, $i_ref) = @_;
	my @stacks = @{$s_ref};
	my @instructions = @{$i_ref};
	
	for my $instr (@instructions) {
		for my $count (1..$instr->{'num'}) {
			my $crate = pop(@{$stacks[$instr->{'from'}]});
			push(@{$stacks[$instr->{'to'}]}, $crate);
		}
	}
	
	my $result = get_result(@stacks);	
	say "Part One: the crates at the tops of the stacks are $result.";
}

sub solve_part_two {
	my ($s_ref, $i_ref) = @_;
	my @stacks = @{$s_ref};
	my @instructions = @{$i_ref};
	
	for my $instr (@instructions) {
		my @crates = ();
		for my $count (1..$instr->{'num'}) {
			my $crate = pop(@{$stacks[$instr->{'from'}]});
			unshift( @crates, $crate) if defined($crate);
		}
		push(@{$stacks[$instr->{'to'}]}, @crates);
	}
	
	my $result = get_result(@stacks);	
	say "Part Two: the crates at the tops of the stacks are $result.";
}

sub get_result {
	my @stacks = @_;
	my $result = "";
	for my $s (@stacks) {
		next if !defined($s);
		my @stack = @{$s};
		$result .= $stack[$#stack];
	}
	return $result;	
}

sub parse_stacks {
	my @input = @_;
	my $cols_line = pop(@input); # number labels
	my @stack_lines = reverse @input; # stack contents, bottom first

	my $ncols = () = $cols_line =~ /(\d)/gi;
	my @stacks = ();
	
	for my $line (@stack_lines) {
		for my $i (1..$ncols) {
			my $prefix = substr($line, 0, 4);
			$line = substr($line, 4) if length($line) >= 4;
			if ($prefix =~ m/(\w)/) {
				push( @{$stacks[$i]}, $1 );
			}
		}
	}
	
	return @stacks;
}

sub parse_instructions {
	my @input = @_;
	my @instructions = ();
	
	for my $line (@input) {
		$line =~ m/move (\d+) from (\d+) to (\d+)/;
		my %i = ();
		$i{'num'} = $1;
		$i{'from'} = $2;
		$i{'to'} = $3;
		push(@instructions, \%i);
	}
	
	return @instructions;
}