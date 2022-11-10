#!/usr/bin/env perl
BEGIN {
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $local_lib;

package AOC::SpatialUtil;
use Modern::Perl 2018;
use Exporter;
use feature 'signatures';
use List::Util qw(min max);

our @ISA = qw( Exporter );
#our @EXPORT_OK = qw(C2D_create C3D_create);
our @EXPORT = qw(
	C2D_create C2D_to_str C2D_from_str 
	C2D_equal C2D_add C2D_delta C2D_distance C2D_manhattan

	C3D_create C3D_to_str C3D_from_str 
	C3D_equal C3D_add C3D_delta C3D_distance C3D_manhattan

	E2D_create E2D_build E2D_to_str
	E2D_min E2D_max E2D_width E2D_height E2D_area E2D_all_coords
	E2D_equal E2D_contains E2D_intersect E2D_union

	G2D_create G2D_get G2D_set G2D_extent 
	G2D_coords G2D_coords_with_value G2D_histogram 
	G2D_offsets G2D_neighbors G2D_print);

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

sub C2D_equal($c1, $c2) {
	return $c1->[0] == $c2->[0] && $c1->[1] == $c2->[1];
}

sub C2D_add($c1, $c2) {
	return C2D_create($c2->[0] + $c1->[0], $c2->[1] + $c1->[1]);
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

sub C3D_equal($c1, $c2) {
	return $c1->[0] == $c2->[0] &&
			$c1->[1] == $c2->[1] &&
			$c1->[2] == $c2->[2];
}

sub C3D_add($c1, $c2) {
	return C3D_create($c2->[0] + $c1->[0],
					  $c2->[1] + $c1->[1],
					  $c2->[2] + $c1->[2]);
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
	my @data = ();
	for my $c ( @c_list ) {
		E2D_expand_to_fit(\@data, $c);
	}
	return \@data;
}

sub E2D_expand_to_fit($e2d, $c2d) {
	if (E2D_is_empty($e2d)) {
		$e2d->[0] = $c2d->[0];
		$e2d->[1] = $c2d->[1];
		$e2d->[2] = $c2d->[0];
		$e2d->[3] = $c2d->[1];
	}
	else {
		$e2d->[0] = min($e2d->[0], $c2d->[0]);
		$e2d->[1] = min($e2d->[1], $c2d->[1]);
		$e2d->[2] = max($e2d->[2], $c2d->[0]);
		$e2d->[3] = max($e2d->[3], $c2d->[1]);
	}
}

sub E2D_is_empty($e2d) {
	return scalar(@{$e2d}) == 0;
}

sub E2D_to_str($e2d) {
	if (E2D_is_empty($e2d)) {
		return '{empty}';
	}
	return '{min: [' . $e2d->[0] . ',' . $e2d->[1] . '], max: [' . $e2d->[2] . ',' . $e2d->[3] . ']}';
}

sub E2D_min($e2d) {
	return C2D_create($e2d->[0], $e2d->[1]);
}

sub E2D_max($e2d) {
	return C2D_create($e2d->[2], $e2d->[3]);
}

sub E2D_width($e2d) {
	if (E2D_is_empty($e2d)) { return 0; }
	return $e2d->[2] - $e2d->[0] + 1;
}

sub E2D_height($e2d) {
	if (E2D_is_empty($e2d)) { return 0; }
	return $e2d->[3] - $e2d->[1] + 1;
}

sub E2D_area($e2d) {
	return E2D_width($e2d) * E2D_height($e2d);
}

sub E2D_all_coords($e2d) {
	my @coords = ();
	if (E2D_is_empty($e2d)) { return @coords; }
	for (my $x = $e2d->[0]; $x <= $e2d->[2]; $x++) {
		for (my $y = $e2d->[1]; $y <= $e2d->[3]; $y++) {
			push( @coords, C2D_create($x, $y) );
		}
	}
	return @coords;
}

sub E2D_equal($e1, $e2) {
	return $e1->[0] == $e2->[0] &&
			$e1->[1] == $e2->[1] &&
			$e1->[2] == $e2->[2] &&
			$e1->[3] == $e2->[3];
}

sub E2D_contains($e2d, $c2d) {
	if (E2D_is_empty($e2d)) { return 0; }
	return $e2d->[0] <= $c2d->[0] && $c2d->[0] <= $e2d->[2] &&
			$e2d->[1] <= $c2d->[1] && $c2d->[1] <= $e2d->[3];
}

sub E2D_intersect($e1, $e2) {
	if (E2D_is_empty($e1) || E2D_is_empty($e2)) { return []; }
	my $common_min_x = max($e1->[0], $e2->[0]);
	my $common_max_x = min($e1->[2], $e2->[2]);
	if ($common_max_x < $common_min_x) { return []; }
	my $common_min_y = max($e1->[1], $e2->[1]);
	my $common_max_y = min($e1->[3], $e2->[3]);
	if ($common_max_y < $common_min_y) { return []; }
	
	return [$common_min_x, $common_min_y, $common_max_x, $common_max_y];
}

sub E2D_union($e1, $e2) {
	my @results = ();
	if (E2D_equal($e1, $e2)) { return ($e1); }
	
	my $e_int = E2D_intersect($e1, $e2);
	if (E2D_is_empty($e_int)) {
		if (!E2D_is_empty($e1)) { push(@results, $e1); }
		if (!E2D_is_empty($e2)) { push(@results, $e2); }
		return @results;
	}
	
	push( @results, $e_int );
	for my $e ($e1, $e2) {
		if (E2D_equal($e, $e_int)) { next; }
		
		if ($e->[0] < $e_int->[0]) { # xmin
			if ($e->[1] < $e_int->[1]) { # ymin
				push( @results, [$e->[0], $e->[1], $e_int->[0]-1, $e_int->[1]-1] );
			}
			if ($e->[3] > $e_int->[3]) { # ymax
				push( @results, [$e->[0], $e_int->[3]+1, $e_int->[0]-1, $e->[3]] );
			}
			push( @results, [$e->[0], $e_int->[1], $e_int->[0]-1, $e_int->[3]] );
		}
		if ($e_int->[2] < $e->[2]) {
			if ($e->[1] < $e_int->[1]) { # ymin
				push( @results, [$e_int->[2]+1, $e->[1], $e->[2], $e_int->[1]-1] );
			}
			if ($e->[3] > $e_int->[3]) { # ymax
				push( @results, [$e_int->[2]+1, $e_int->[3]+1, $e->[2], $e->[3]] );
			}
			push( @results, [$e_int->[2]+1, $e_int->[1], $e->[2], $e_int->[3]] );
		}
		if ($e->[1] < $e_int->[1]) { #ymin
			push( @results, [$e_int->[0], $e->[1], $e_int->[2], $e_int->[1]-1] );
		}
		if ($e_int->[3] < $e->[3]) { #ymax
			push( @results, [$e_int->[0], $e_int->[3]+1, $e_int->[2], $e->[3]] );		
		}
	}
	return @results;
}

# -------------------------------------------------------
# Grid2D
#
# Data model: array reference [data hashref, default, rule, extent]
# -------------------------------------------------------

our @RULES = ('rook', 'bishop', 'queen');

sub G2D_create($default, $adj_rule) {
	if ( !grep( /^$adj_rule$/, @RULES ) ) {
		die "$adj_rule is not a valid adjacency rule: @RULES";
	}
	my $g2d = [{}, $default, $adj_rule, []];
}

sub G2D_get($g2d, $c2d) {
	my $key = C2D_to_str($c2d);
	my $val = $g2d->[0]{$key} || $g2d->[1];
	return $val;
}

sub G2D_set($g2d, $c2d, $val) {
	my $key = C2D_to_str($c2d);
	$g2d->[0]{$key} = $val;
	E2D_expand_to_fit( G2D_extent($g2d), $c2d );
}

sub G2D_extent($g2d) {
	return $g2d->[3];
}

sub G2D_coords($g2d) {
	my @coords = ();
	for my $key (keys(%{$g2d->[0]})) {
		push( @coords, C2D_from_str($key) );
	}
	return @coords;
}

sub G2D_coords_with_value($g2d, $val) {
	my @coords = ();
	for my $key (keys(%{$g2d->[0]})) {
		if ($g2d->[0]{$key} eq $val) {
			push( @coords, C2D_from_str($key) );
		}
	}
	return @coords;
}

sub G2D_histogram($g2d) {
	my $hist = {};
	for my $c ( E2D_all_coords( $g2d->[3] ) ) {
		my $val = G2D_get($g2d, $c);
		$hist->{$val} ++;
	}
	return $hist;
}

sub G2D_offsets($g2d) {
	my @offsets = ();
	
	my $rule = $g2d->[2];
	if ($rule eq 'rook' || $rule eq 'queen') {
		push( @offsets, ([-1,0], [1,0], [0,-1], [0,1]) );
	}
	if ($rule eq 'bishop' || $rule eq 'queen') {
		push( @offsets, ([-1,-1], [1,-1], [-1,1], [1,1]) );
	}	
	return @offsets;
}

sub G2D_neighbors($g2d, $c2d) {
	my @offsets = G2D_offsets($g2d);
	my @neighbors = ();
	
	for my $o (@offsets) {
		push( @neighbors, C2D_add( $c2d, $o ));
	}
	return @neighbors;
}

sub G2D_print($g2d) {
	my $e2d = G2D_extent($g2d);
	for (my $y = $e2d->[1]; $y <= $e2d->[3]; $y++) {
		my @row = ();
		for (my $x = $e2d->[0]; $x <= $e2d->[2]; $x++) {
			push( @row, G2D_get($g2d, C2D_create($x, $y)) );
		}
		say join(' ', @row);
	}
}


1;