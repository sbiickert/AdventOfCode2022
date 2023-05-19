#!/usr/bin/env raku

use lib $*PROGRAM.dirname;
use AOC::SpatialUtil;

test_coord2d();
test_extent2d();

exit(0);

sub test_coord2d() {
	say 'Testing ' ~ Coord2D.^name;
	my $c1 = Coord2D.new(x => 10, y => 30);
	say "c1: a new coord with x=10 and y=30 ", $c1;
	my $c2 = Coord2D.from_ints: <4 5>;
	say $c2;
	my $str = $c2.Str;
	my $c3 = Coord2D.from_str: $str;
	say $c3;
	my $c4 = $c2.clone;
	say $c4;
	say $c3 eqv $c4;
	my $c5 = $c1.add: $c2;
	say $c5;
	my $c6 = $c2.delta: $c5;
	say $c6;
	$c6 eqv $c1 or die "$c6 should be equal to $c1";
	my $dist = $c1.distanceTo($c2);
	say $dist;
	my $md = $c1.manhattanDistanceTo($c2);
	say $md;
	$md == 31 or die "Expected the MD from $c1 to $c2 to be 31.";
	#my $c7 = $c6.clone;
	#$c7.x = 1;
	#$c7.y = 2;
	#say $c7;
}

sub test_extent2d() {
	say 'Testing ' ~ Extent2D.^name;
	my $e1 = Extent2D.new(min => Coord2D.new( x => 1, y => 3 ), max => Coord2D.new( x => 4, y => 7 ));
	say "e1: a new extent ", $e1;
	my $e2 = Extent2D.from_ints: < 9 8 7 6 >;
	say "e2: created from ints 9 8 7 6 ", $e2;
	$e1.width == 4 or die "Expected width to be 4";
	$e1.height == 5 or die "Expected height to be 5";
	$e1.area == 20 or die "Expected area to be 20";
	my @coords = $e1.all_coords();
	say @coords;
	@coords.elems == $e1.area or die "Should be one coord for every square unit area";
	my $e3 = $e1.clone;
	say $e1 eqv $e3;
	my $e4 = $e1.inset(1);
	say $e4;
	$e1.contains( Coord2D.new( x => 2, y => 3 ) ) or die "$e1 should contain point 2,3";
	!$e1.contains( Coord2D.new( x => 20, y => 30 ) ) or die "$e1 should NOT contain point 20, 30";
}