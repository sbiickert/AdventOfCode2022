<?php

require 'AoC/Util.php';

echo "Running _test.php\n";


test_util();

function test_util() {
	echo "test_util\n";
	$test_input_file = '../input/day00_test.txt';
	
	echo "Test reading all lines.\n";
	$lines = read_input($test_input_file);
	echo implode("\n", $lines) . "\n";
	assert(count($lines) == 10, "Number of lines should have been 10.");
	
	echo "Test reading all lines removing empty lines.\n";
	$lines = read_input($test_input_file, true);
	echo implode("\n", $lines) . "\n";
	assert(count($lines) == 8, "Number of lines should have been 8.");
	
	echo "Test reading grouped input.\n";
	$groups = read_grouped_input($test_input_file);
	assert(count($groups) == 3, "Number of groups should have been 3.");
	foreach ($groups as $i => $group) {
		echo "Group $i has " . count($group) . " lines.\n";
	}
	
	echo "Test reading grouped input with valid index.\n";
	$lines = read_grouped_input($test_input_file, 1);
	assert(count($lines) == 2, "Expected 2 lines in group 1.");
	
	echo "Test reading grouped input with invalid index.\n";
	$lines = read_grouped_input($test_input_file, 10);
	assert(count($lines) == 0, "Expected 0 lines in group 10.");
}
