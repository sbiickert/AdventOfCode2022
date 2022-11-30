<?php

require 'AoC/Util.php';
require 'AoC/SpatialUtil.php';

echo "Running _test.php\n";

test_util();
test_spatial_util();

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

function test_spatial_util() {
	echo "test_spatial_util\n";
	
	test_coord2D();
}

function test_coord2D() {
	echo "test_coord2D\n";
	$c2d = new Coord2D(10, 30);
	echo $c2d->toString() . "\n";
	
	$other = new Coord2D(10, 30);
	echo 'other: ' . $other->toString() . "\n";
	assert( $c2d->equalTo($other), "c2d and other were not equal." );
	
	$other->setX(5); $other->setY(20);
	echo 'other: ' . $other->toString() . "\n";
	$delta = $c2d->deltaTo($other);
	echo 'delta: ' . $delta->toString() . "\n";
	assert( $delta->getX() == -5 && $delta->getY() == -10 );
	
	$md = $c2d->manhattanDistanceTo($other);
	echo "Manhattan distance: $md\n";
	assert( $md == 15 );
	
	$dist = $c2d->distanceTo($other);
	echo "Distance: $dist\n";
	
	$c2dString = $c2d->toString();
	$clone = Coord2D::fromString($c2dString);
	echo 'clone: ' . $clone->toString() . "\n";
	assert($c2d->equalTo($clone), "c2d and clone were not equal." );
}