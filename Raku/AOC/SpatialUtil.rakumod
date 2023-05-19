unit module AOC::SpatialUtil;

class Coord2D is export {
	has Int $.x = 0;
	has Int $.y = 0;
	
	# class method: construct from a list of Int 
	method from_ints(::?CLASS:U $c2d: @ints) {
		$c2d.new( x => @ints[0].Int, y => @ints[1].Int );
	}
	
	# class method: construct from a Str representation 
	method from_str(::?CLASS:U $c2d: $str) {
		my $x = 0; my $y = 0;
		if $str ~~ rx:s/\[(\d+)\, ?(\d+)\]/ {
			$x = +~$0; $y = +~$1;
		}
		$c2d.new( x => $x, y => $y );
	}
	
	method Str() { "[$.x,$.y]" }
	
	multi method gist(Coord2D:U:) { self.^name }
	multi method gist(Coord2D:D:) { "•[$.x, $.y]" }
	
	multi infix:<eqv>(Coord2D $l, Coord2D $r) { $l.x == $r.x && $l.y == $r.y }

	method add(Coord2D $other --> Coord2D) {
		Coord2D.new( x => $.x + $other.x, y => $.y + $other.y )
	}

	method delta(Coord2D $other --> Coord2D) {
		Coord2D.new( x => $other.x - $.x, y => $other.y - $.y )
	}
	
	method distanceTo(Coord2D $other --> Num) {
		my $delta = $.delta($other);
		return sqrt($delta.x**2 + $delta.y**2);
	}
	
	method manhattanDistanceTo(Coord2D $other --> Int) {
		my $delta = $.delta($other);
		return abs($delta.x) + abs($delta.y);
	}
}

class Extent2D is export {
	has Coord2D $.min;
	has Coord2D $.max;
	
	# class method: construct from a list of Int 
	method from_ints(::?CLASS:U $e2d: @ints) {
		my $min = Coord2D.new( x => @ints[0].Int, y => @ints[1].Int );
		my $max = Coord2D.new( x => @ints[2].Int, y => @ints[3].Int );
		$e2d.from_coords( [$min, $max] );
	}
	
	# class method: construct from a list of Coord2D 
	method from_coords(::?CLASS:U $e2d: @coords) {
		my $ext = Extent2D.new();
		
		for @coords -> $coord {
			$ext = $ext.expand_to_fit($coord);
		}
		return $ext;
	}
	
	method expand_to_fit( Coord2D $coord --> Extent2D ) {
		my Coord2D $new_min;
		my Coord2D $new_max;
		
		if $.is_empty {
			$new_min = $coord.clone;
			$new_max = $coord.clone;
		}
		else {
			$new_min = Coord2D.new( x => min($.min.x, $coord.x), y => min($.min.y, $coord.y) );
			$new_max = Coord2D.new( x => max($.max.x, $coord.x), y => max($.max.y, $coord.y) );
		}
		
		return Extent2D.new( min => $new_min, max => $new_max )
	}
	
	method is_empty(--> Bool) {
		return !$.min && !$.max;
	}
	
	method Str() { $.is_empty() ?? '[EMPTY]' !! "[$.min,$.max]" }
	
	multi method gist(Extent2D:U:) { self.^name }
	multi method gist(Extent2D:D:) { $.is_empty() ?? "▭[EMPTY]" !! "▭[$.min, $.max]" }
	
	multi infix:<eqv>(Extent2D $l, Extent2D $r) { $l.min eqv $r.min && $l.max eqv $r.max }
	
	method width(--> Int) {
		return $.is_empty() ?? 0 !! $.max.x - $.min.x + 1;
	}
	
	method height(--> Int) {
		return $.is_empty() ?? 0 !! $.max.y - $.min.y + 1;
	}
	
	method area(--> Int) {
		return $.width * $.height;
	}
	
	method all_coords(--> Array of Coord2D) {
		my Coord2D @coords = ();
		
		for $.min.x .. $.max.x X $.min.y .. $.max.y -> $pair {
			@coords.push( Coord2D.new( x => $pair[0], y => $pair[1] ) );
		}
		return @coords;
	}
	
	method contains( Coord2D $coord --> Bool ) {
		if $.is_empty() { return False }
		return ($.min.x <= $coord.x && $coord.x <= $.max.x)
			&& ($.min.y <= $coord.y && $coord.y <= $.max.y);
	}
	
	# intersect
	
	# union 
	
	method inset( Int $inset --> Extent2D ) {
		if $.is_empty() { return Extent2D.new(); }
		return Extent2D.from_ints: [$.min.x + $inset, $.min.y + $inset,
									$.max.x - $inset, $.max.y - $inset];
	}
}

class Grid2D is export {
	has %.data;
	has $.default;
	has $.rule;
	has Extent2D $.extent;
}