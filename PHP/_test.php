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
	test_extent2D();
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

function test_extent2D() {
	echo "test_extent2D\n";
	$c1 = new Coord2D(-1,1);
	$c2 = new Coord2D(2,8);
	$c3 = new Coord2D(3,3);
	$c4 = new Coord2D(4,4);
	$e1 = new Extent2D($c1, $c2);
	echo $e1->toString() . "\n";
	assert($e1->getMin()->getX() == -1);
	assert($e1->getMin()->getY() == 1);
	assert($e1->getMax()->getX() == 2);
	assert($e1->getMax()->getY() == 8);
	$c_list = array($c3, $c2, $c1);
	$e2 = Extent2D::build($c_list);
	echo $e2->toString() . "\n";
	assert($e2->getMin()->getX() == -1);
	assert($e2->getMin()->getY() == 1);
	assert($e2->getMax()->getX() == 3);
	assert($e2->getMax()->getY() == 8);
	echo 'The width of e2 is ' . $e2->getWidth() . "\n";
	echo 'The height of e2 is ' . $e2->getHeight() . "\n";
	echo 'The area of e2 is ' . $e2->getArea() . "\n";
	assert($e2->getWidth() == 5);
	assert($e2->getHeight() == 8);
	assert($e2->getArea() == 40);
	echo ($e2->contains($c2) ? 'c2 is contained by e2' : 'c2 is outside e2') . "\n";
	echo ($e2->contains($c4) ? 'c4 is contained by e2' : 'c4 is outside e2') . "\n";
	assert($e2->contains($c2));
	assert($e2->contains($c4) == false);
}