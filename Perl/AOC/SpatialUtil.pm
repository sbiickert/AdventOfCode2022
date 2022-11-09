#!/usr/bin/env perl
BEGIN {
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $local_lib;

package AOC::SpatialUtil;
use Modern::Perl 2018;
use Exporter;
use List::Util qw(min max);
use feature 'signatures';

our @ISA = qw( Exporter );
#our @EXPORT_OK = qw(C2D_create C3D_create);
our @EXPORT = qw(C2D_create C2D_to_str C2D_from_str 
				 C2D_equals C2D_delta C2D_distance C2D_manhattan
				 C3D_create C3D_to_str C3D_from_str 
				 C3D_equals C3D_delta C3D_distance C3D_manhattan
				 E2D_create E2D_build E2D_to_str
				 E2D_min E2D_max E2D_width E2D_height E2D_area E2D_contains);

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

sub C3D_to_str($c2d) {
	return '[' . $c2d->[0] . ',' . $c2d->[1] . ',' . $c2d->[2] . ']';
}

sub C3D_from_str($val) {
	if ($val =~ m/\[(-?\d+),(-?\d+),(-?\d+)\]/) {
		return C3D_create($1, $2, $3);
	}
	return 0;
}

sub C3D_equals($c1, $c2) {
	return $c1->[0] == $c2->[0] &&
			$c1->[1] == $c2->[1] &&
			$c1->[2] == $c2->[2];
}

sub C3D_delta($c1, $c2) {
	return C3D_create($c2->[0] - $c1->[0],
					  $c2->[1] - $c1->[1],
					  $c2->[2] - $c1->[2]);
}

sub C3D_distance($c1, $c2) {
	my $delta = C3D_delta($c1, $c2);
	return sqrt($delta->[0]**2 + $delta->[1]**2 + $delta->[2]**2);
}

sub C3D_manhattan($c1, $c2) {
	my $delta = C3D_delta($c1, $c2);
	return abs($delta->[0]) + abs($delta->[1]) + abs($delta->[2]);
}


# -------------------------------------------------------
# Extent2D
#
# Data model: array reference [xmin,ymin,xmax,ymax]
# -------------------------------------------------------
sub E2D_create($c_min, $c_max) {
	return [$c_min->[0], $c_min->[1], $c_max->[0], $c_max->[1]];
}

sub E2D_build(@c_list) {
	my @data = (0) x 4;
	if (scalar(@c_list) > 0) {
		my $c = $c_list[0];
		@data = ($c->[0], $c->[1], $c->[0], $c->[1]);
	}
	for my $c ( @c_list ) {
		$data[0] = min($data[0], $c->[0]);
		$data[1] = min($data[1], $c->[1]);
		$data[2] = max($data[2], $c->[0]);
		$data[3] = max($data[3], $c->[1]);
	}
	return \@data;
}

sub E2D_to_str($e2d) {
	return '{min: [' . $e2d->[0] . ',' . $e2d->[1] . '], max: [' . $e2d->[2] . ',' . $e2d->[3] . ']}';
}

sub E2D_min($e2d) {
	return C2D_create($e2d->[0], $e2d->[1]);
}

sub E2D_max($e2d) {
	return C2D_create($e2d->[2], $e2d->[3]);
}

sub E2D_width($e2d) {
	return $e2d->[2] - $e2d->[0]
}

sub E2D_height($e2d) {
	return $e2d->[3] - $e2d->[1]
}

sub E2D_area($e2d) {
	return E2D_width($e2d) * E2D_height($e2d);
}

sub E2D_contains($e2d, $c2d) {
	return $e2d->[0] <= $c2d->[0] && $c2d->[0] <= $e2d->[2] &&
			$e2d->[1] <= $c2d->[1] && $c2d->[1] <= $e2d->[3];
}

# -------------------------------------------------------
# Grid2D
#
# Data model: array reference [data hashref, default, rule]
# -------------------------------------------------------


1;