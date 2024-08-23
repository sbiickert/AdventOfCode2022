unit module AOC::Util;

# Read Input

# Reads the specified file and returns an array of strings.

# If $remove_empty_lines is true, will remove any zero-length lines
# (after chomp)
sub read_input(Str $filename, Bool $remove_empty_lines = False) is export {
	my @content = $filename.IO.lines;
	
	if $remove_empty_lines {
		@content = grep { .Str.chars > 0 }, @content;
	}
	
	return @content;
}

# Read Grouped Input

# If $group_index is not given or negative, then all
# groups are returned as an array of array refs.

# If $group_index is a valid index for a group, then
# only the lines of that group are returned as an array of strings.

# If $group_index is out of range, then an empty array is returned.
sub read_grouped_input(Str $filename, Int $group_index = -1) is export {
	my @content = read_input($filename);
	my @groups = [];
	my @group = [];
	
	for @content -> $line {
		if ($line eq '') {
			@groups.push(@group.clone);
			@group = [];
		}
		else {
			@group.push($line);
		}
	}
	
	if @group.elems > 0 {
		@groups.push(@group);
	}
	
	if $group_index >= 0 {
		#say @groups;
		return $group_index < @groups.elems ?? @groups[$group_index].flat !! [];
	}
	
	return @groups;
}

# Approx Equal

# Avoids the issues with floating point numbers that are close
# to being equal but are not exactly equal.

# $threshold is the maximum allowable difference between $float1
# and $float2 for them to be considered equal.
sub approx_equal(Rat $float1, Rat $float2, Rat $threshold = 0.001) returns Bool is export {
	my $difference = abs($float1 - $float2);
	return $difference < $threshold;
}


# DON'T NEED: lcm and gcd are built-in operators!!
# reduce: numerator and denominator are methods on the Rat type

# Greatest Common Divisor
# Takes two integers and returns the GCD
# sub gcd(Int $x, Int $y) returns Int is export {
# 	my $a = 0;
# 	my $b = max($x,$y);
# 	my $r = min($x,$y);
# 	while ($r != 0) {
# 		$a = $b;
# 		$b = $r;
# 		$r = $a % $b;
# 	}
# 	return $b;
# }


# Least Common Multiple
# Takes an array of integers and returns the LCM
# sub lcm(@values) returns Int is export {
# 	return 0 if @values.elems == 0;
# 
# 	my $running = shift @values;
# 	while (@values.elems > 0) {
# 		my $next = shift @values;
# 		$running = $running / gcd($running.Int, $next) * $next;
# 	}
# 	return $running.Int
# }
