#!/usr/bin/env raku

use lib $*PROGRAM.dirname ~ '/lib';
use AOC::Util;
#use AOC::Geometry;
#use AOC::Grid;

my $INPUT_PATH = '../input';
#my $INPUT_FILE = 'day05_test.txt';
my $INPUT_FILE = 'day05_challenge.txt';
my @input = read_grouped_input("$INPUT_PATH/$INPUT_FILE");

say "Advent of Code 2022, Day 5: Supply Stacks";

my @stacks = parse_stacks(@input[0]);
my @commands = parse_commands(@input[1]);

solve_part_one(@stacks, @commands);
solve_part_two(@input);

exit( 0 );

sub solve_part_one(@stacks, @commands) {
	for @commands -> $cmd {
		#say $cmd.Str;
		my $from_idx = 0; my $to_idx = 0;
		for 0..@stacks.end -> $i {
			$from_idx = $i if @stacks[$i].id eq $cmd.from;
			$to_idx = $i if @stacks[$i].id eq $cmd.to;
		}
		my @boxes = @stacks[$from_idx].remove($cmd.count);
		@stacks[$to_idx].add(@boxes);
		#for @stacks -> $stack { say $stack.Str; }
	}
	my $message = '';
	for @stacks -> $stack {
		$message ~= $stack.top;
	}
	say "Part One: the message is $message.";
}

sub solve_part_two(@input) {
	
}


class Stack {
	has Str $.id;
	has Str @.data;

	method remove(Int $count --> Array) {
		my @result = ();
		for 1..$count -> $i {
			@result.push(@.data.pop);
		}
		return @result;
	}

	method add(@values) {
		for @values -> $value { @.data.push($value); }
	}

	method Str(--> Str) {
		return "$.id [" ~ @.data.join('] [') ~ ']';
	}

	method top(--> Str) {
		return @.data.tail;
	}
}


class Command {
	has Int $.count;
	has Str $.from;
	has Str $.to;

	method Str(--> Str) {
		return "Move $.count boxes from $.from to $.to";
	}
}


sub parse_stacks(@input --> Array) {
	my @stacks = ();
	
	my @id_row = @input.tail.split('', :skip-empty);
	for 0..@id_row.end -> $i {
		if @id_row[$i] ne ' ' {
			my @stack_data = ();
			for 0..@input.end-1 -> $j {
				my $value = @input[$j].split('', :skip-empty)[$i];
				@stack_data.unshift($value) if $value ne ' ';
			}
			@stacks.push(Stack.new(id => @id_row[$i], data => @stack_data));
			#say @stacks.tail.Str;
		}
	}
	return @stacks;
}

sub parse_commands(@input --> Array) {
	my @commands = ();

	for @input -> $line {
		$line ~~ /(\d+) ' from ' (\d+) ' to ' (\d+)/;
		my $cmd = Command.new(count => $0.Int, from => $1.Str, to => $2.Str);
		@commands.push($cmd);
		#say $cmd.Str;
	}

	return @commands;
}
