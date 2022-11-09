#!/usr/bin/env perl

BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2018;
use AOC::Util;
use AOC::SpatialUtil;
use Data::Dumper;

#test_coord2d();
#test_coord3d();
test_extent2d();

sub test_coord2d {
	say "Testing Coord2D";
	my $c2d = C2D_create(10, 30);
	say C2D_to_str($c2d);
	
	my $other = C2D_create(10,30);
	say 'other: ' . C2D_to_str($other);
	print "Other is equal? " . C2D_equals($c2d, $other) . "\n";
	$other = C2D_create(5,20);
	say 'other: ' . C2D_to_str($other);
	print "Other is equal? " . C2D_equals($c2d, $other) . "\n";
	
	my $delta = C2D_delta($c2d, $other);
	say "Delta from c2d to other is " . C2D_to_str($delta);
	say "Distance from c2d to other is " . C2D_distance($c2d, $other);
	say "Manhattan dist from c2d to other is " . C2D_manhattan($c2d, $other);
	
	my $clone = C2D_from_str( C2D_to_str($c2d) );
	say 'The clone ' . C2D_to_str($clone) . ' is the same as the original.';
}

sub test_coord3d {
	say "Testing Coord3D";
	my $c3d = C3D_create(10, 30, -5);
	say C3D_to_str($c3d);
	
	my $other = C3D_create(10,30,-5);
	say 'other: ' . C3D_to_str($other);
	print "Other is equal? " . C3D_equals($c3d, $other) . "\n";
	$other = C3D_create(5,20,15);
	say 'other: ' . C3D_to_str($other);
	print "Other is equal? " . C3D_equals($c3d, $other) . "\n";
	
	my $delta = C3D_delta($c3d, $other);
	say "Delta from c3d to other is " . C3D_to_str($delta);
	say "Distance from c3d to other is " . C3D_distance($c3d, $other);
	say "Manhattan dist from c3d to other is " . C3D_manhattan($c3d, $other);
	
	my $clone = C3D_from_str( C3D_to_str($c3d) );
	say 'The clone ' . C3D_to_str($clone) . ' is the same as the original.';
}

sub test_extent2d {
	say "Testing Extent2D";
	my $c1 = C2D_create(1,1);
	my $c2 = C2D_create(2,2);
	my $c3 = C2D_create(3,3);
	my $c4 = C2D_create(4,4);
	my $e1 = E2D_create($c1, $c2);
	say E2D_to_str($e1);
	my @c_list = ($c3, $c2, $c1);
	my $e2 = E2D_build(@c_list);
	say E2D_to_str($e2);
	say 'The width of e2 is ' . E2D_width($e2);
	say 'The height of e2 is ' . E2D_height($e2);
	say 'The area of e2 is ' . E2D_area($e2);
	say (E2D_contains($e2, $c2) ? 'c2 is contained by e2' : 'c2 is outside e2');
	say (E2D_contains($e2, $c4) ? 'c4 is contained by e2' : 'c4 is outside e2');
}