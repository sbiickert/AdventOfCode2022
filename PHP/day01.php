<?php

require 'AoC/Util.php';

echo "Advent of Code 2022, Day 1: Calorie Counting\n";

$INPUT_DIR = '../input/';
#$INPUT_FILE = 'day01_test.txt';
$INPUT_FILE = 'day01_challenge.txt';
$input = read_grouped_input($INPUT_DIR . $INPUT_FILE);

$elfCalories = solvePartOne($input);

$topThree = $elfCalories[0] + $elfCalories[1] + $elfCalories[2];
echo "Part Two: The sum of the three elves with the most calories is $topThree\n";


function solvePartOne(array $elves): array {
	$elfCalories = array();
	
	foreach ($elves as $elf) {
		array_push($elfCalories, array_sum($elf));
	}
	
	rsort($elfCalories, SORT_NUMERIC);
	
	echo "Part One: The elf with the most calories has $elfCalories[0]\n";
	return $elfCalories;
}
