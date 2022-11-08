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

test_coord2d();
test_coord3d();


sub test_coord2d {
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
	my $c3d = C3D_create(10, 30, -5);
	print Dumper($c3d);
	
	my $other = C3D_create(10,30,-5);
	print "Other is equal? " . C3D_equals($c3d, $other) . "\n";
	$other = C3D_create(10,20,0);
	print "Other is equal? " . C3D_equals($c3d, $other) . "\n";
}