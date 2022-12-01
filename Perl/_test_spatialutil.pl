#!/usr/bin/env perl

BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2022;
use AOC::Util;
use AOC::SpatialUtil;
use Data::Dumper;

test_coord2d();
#test_coord3d();
test_extent2d();
#test_grid2d();

sub test_coord2d {
	say "\nTesting Coord2D";
	my $c2d = C2D_create(10, 30);
	say C2D_to_str($c2d);
	
	my $other = C2D_create(10,30);
	say 'other: ' . C2D_to_str($other);
	(C2D_equal($c2d, $other)) or die "Coordinates were not equal.";
	
	$other = C2D_create(5,20);
	say 'other: ' . C2D_to_str($other);
	(!C2D_equal($c2d, $other)) or die "Coordinates were equal.";
	
	my $delta = C2D_delta($c2d, $other);
	say "Delta from c2d to other is " . C2D_to_str($delta);
	($delta->[0] == -5 && $delta->[1] == -10) or die "delta was wrong.";
	
	say "Distance from c2d to other is " . C2D_distance($c2d, $other);
	say "Manhattan dist from c2d to other is " . C2D_manhattan($c2d, $other);
	(C2D_manhattan($c2d, $other) == 15) or die "Manhattan distance was wrong.";
	
	my $clone = C2D_from_str( C2D_to_str($c2d) );
	say 'The clone ' . C2D_to_str($clone) . ' is the same as the original.';
	(C2D_equal($c2d, $clone)) or die "The original and the clone are not equal.";
}

sub test_coord3d {
	say "\nTesting Coord3D";
	my $c3d = C3D_create(10, 30, -5);
	say C3D_to_str($c3d);
	
	my $other = C3D_create(10,30,-5);
	say 'other: ' . C3D_to_str($other);
	print "Other is equal? " . C3D_equal($c3d, $other) . "\n";
	$other = C3D_create(5,20,15);
	say 'other: ' . C3D_to_str($other);
	print "Other is equal? " . C3D_equal($c3d, $other) . "\n";
	
	my $delta = C3D_delta($c3d, $other);
	say "Delta from c3d to other is " . C3D_to_str($delta);
	say "Distance from c3d to other is " . C3D_distance($c3d, $other);
	say "Manhattan dist from c3d to other is " . C3D_manhattan($c3d, $other);
	
	my $clone = C3D_from_str( C3D_to_str($c3d) );
	say 'The clone ' . C3D_to_str($clone) . ' is the same as the original.';
}

sub test_extent2d {
	say "\nTesting Extent2D";
	my $c1 = C2D_create(-1,1);
	my $c2 = C2D_create(2,8);
	my $c3 = C2D_create(3,3);
	my $c4 = C2D_create(4,4);
	my $e1 = E2D_create($c1, $c2);
	say E2D_to_str($e1);
	($e1->[0] == -1) or die;
	($e1->[1] == 1) or die;
	($e1->[2] == 2) or die;
	($e1->[3] == 8) or die;
	my @c_list = ($c3, $c2, $c1);
	my $e2 = E2D_build(@c_list);
	say E2D_to_str($e2);
	($e2->[0] == -1) or die;
	($e2->[1] == 1) or die;
	($e2->[2] == 3) or die;
	($e2->[3] == 8) or die;
	say 'The width of e2 is ' . E2D_width($e2);
	say 'The height of e2 is ' . E2D_height($e2);
	say 'The area of e2 is ' . E2D_area($e2);
	(E2D_width($e2) == 5) or die;
	(E2D_height($e2) == 8) or die;
	(E2D_area($e2) == 40) or die;
	say (E2D_contains($e2, $c2) ? 'c2 is contained by e2' : 'c2 is outside e2');
	say (E2D_contains($e2, $c4) ? 'c4 is contained by e2' : 'c4 is outside e2');
	(E2D_contains($e2, $c2)) or die;
	(!E2D_contains($e2, $c4)) or die;
	my @all_coords = E2D_all_coords($e2);
	(scalar(@all_coords) == E2D_area($e2)) or die;
	
	test_e2d_intersect([1,1,10,10],[5,5,12,12]);
	test_e2d_intersect([1,1,10,10],[5,5,7,7]);
	test_e2d_intersect([1,1,10,10],[1,1,12,2]);
	test_e2d_intersect([1,1,10,10],[11,11,12,12]);
	test_e2d_intersect([1,1,10,10],[1,10,10,20]);
	
	test_e2d_union([1,1,10,10],[5,5,12,12]);
	test_e2d_union([1,1,10,10],[5,5,7,7]);
	test_e2d_union([1,1,10,10],[1,1,12,2]);
	test_e2d_union([1,1,10,10],[11,11,12,12]);
	test_e2d_union([1,1,10,10],[1,10,10,20]);
}

sub test_e2d_intersect {
	my ($e1, $e2) = @_;
	say 'Intersection of ' . E2D_to_str($e1) . ' and ' . E2D_to_str($e2);
	my $e_int = E2D_intersect($e1, $e2);
	say E2D_to_str($e_int);
}

sub test_e2d_union {
	my ($e1, $e2) = @_;
	say 'Union of ' . E2D_to_str($e1) . ' and ' . E2D_to_str($e2);
	my @products = E2D_union($e1, $e2);
	for my $e (@products) {
		say E2D_to_str($e);
	}
}

sub test_grid2d {
	say "\nTesting Grid2D";
	my $g2d = G2D_create('.', 'rook');
	print Dumper($g2d);
	
	my @coords = (C2D_create(1,1), C2D_create(2,2), C2D_create(3,3), C2D_create(4,4));
	G2D_set($g2d, $coords[0], 'A');
	G2D_set($g2d, $coords[1], 'B');
	G2D_set($g2d, $coords[3], 'D');
	G2D_print($g2d);
	print Dumper($g2d);
	say 'The value at ' . C2D_to_str($coords[1]) . ' is ' . G2D_get($g2d, $coords[1]);
	say 'The value at ' . C2D_to_str($coords[2]) . ' is ' . G2D_get($g2d, $coords[2]);
	my @all = G2D_coords($g2d);
	say 'All coords in grid:';
	for my $c (@all) { say C2D_to_str($c); }
	my @ds = G2D_coords_with_value($g2d, 'D');
	say "All D's in grid:";
	for my $c (@ds) { say C2D_to_str($c); }
	my @xs = G2D_coords_with_value($g2d, 'X');
	say "All X's in grid:";
	for my $c (@xs) { say C2D_to_str($c); }
	say 'Histogram of values:';
	print Dumper(G2D_histogram( $g2d ));
	my @neighbors = G2D_neighbors( $g2d, $coords[1] );
	for my $n (@neighbors) {
		G2D_set($g2d, $n, '*');
	}
	G2D_print($g2d);
}