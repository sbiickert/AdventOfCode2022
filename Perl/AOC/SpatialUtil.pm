#!/usr/bin/env perl
BEGIN {
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $local_lib;

package AOC::SpatialUtil;
use Modern::Perl 2018;
use Exporter;
use feature 'signatures';

our @ISA = qw( Exporter );
#our @EXPORT_OK = qw(C2D_create C3D_create);
our @EXPORT = qw(C2D_create C2D_to_str C2D_from_str 
				 C2D_equals C2D_delta C2D_distance C2D_manhattan
				 C3D_create C3D_equals);

# -------------------------------------------------------
# Coord2D
#
# Data model: array reference [x,y]
# -------------------------------------------------------
sub C2D_create($x, $y) {
	return [$x, $y];
}

sub C2D_to_str($c2d) {
	return '[' . $c2d->[0] . ',' . $c2d->[1] . ']';
}

sub C2D_from_str($val) {
	if ($val =~ m/\[(-?\d+),(-?\d+)\]/) {
		return C2D_create($1, $2);
	}
	return 0;
}

sub C2D_equals($c1, $c2) {
	return $c1->[0] == $c2->[0] && $c1->[1] == $c2->[1];
}

sub C2D_delta($c1, $c2) {
	return C2D_create($c2->[0] - $c1->[0], $c2->[1] - $c1->[1]);
}

sub C2D_distance($c1, $c2) {
	my $delta = C2D_delta($c1, $c2);
	return sqrt($delta->[0]**2 + $delta->[1]**2);
}

sub C2D_manhattan($c1, $c2) {
	my $delta = C2D_delta($c1, $c2);
	return abs($delta->[0]) + abs($delta->[1]);
}


# -------------------------------------------------------
# Coord3D
#
# Data model: array reference [x,y,z]
# -------------------------------------------------------
sub C3D_create($x, $y, $z) {
	return [$x, $y, $z];
}

sub C3D_equals($c1, $c2) {
	return $c1->[0] == $c2->[0] &&
			$c1->[1] == $c2->[1] &&
			$c1->[2] == $c2->[2];
}


# -------------------------------------------------------
# Extent2D
# -------------------------------------------------------


# -------------------------------------------------------
# Grid2D
# -------------------------------------------------------


1;