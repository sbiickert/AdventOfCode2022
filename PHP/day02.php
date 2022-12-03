<?php

require 'AoC/Util.php';
#require 'AoC/SpatialUtil.php';

echo "Advent of Code 2022, Day 02: Rock, Paper, Scissors\n";

$INPUT_DIR = '../input/';
#$INPUT_FILE = 'day02_test.txt';
$INPUT_FILE = 'day02_challenge.txt';
$input = read_input($INPUT_DIR . $INPUT_FILE);

solvePartOne($input);
solvePartTwo($input);


function solvePartOne(array $input) {
	$DRAW = 3; $WIN = 6; $LOSS = 0;
	$score = array( 'A X' => $DRAW + 1,
					'A Y' => $WIN  + 2,
					'A Z' => $LOSS + 3,
					'B X' => $LOSS + 1,
					'B Y' => $DRAW + 2,
					'B Z' => $WIN  + 3,
					'C X' => $WIN + 1,
					'C Y' => $LOSS + 2,
					'C Z' => $DRAW + 3 );
	 $sum = 0;
	 foreach ($input as $line) {
	 	$sum += $score[$line];
	 }
	 
	 print "Part One: score is $sum";
}

function solvePartTwo(array $input) {
	$DRAW = 3; $WIN = 6; $LOSS = 0;
	$score = array( 'A X' => $LOSS + 3,
					'A Y' => $DRAW + 1,
					'A Z' => $WIN  + 2,
					'B X' => $LOSS + 1,
					'B Y' => $DRAW + 2,
					'B Z' => $WIN  + 3,
					'C X' => $LOSS + 2,
					'C Y' => $DRAW + 3,
					'C Z' => $WIN  + 1 );
	 $sum = 0;
	 foreach ($input as $line) {
	 	$sum += $score[$line];
	 }
	 
	 print "Part Two: score is $sum";
}
