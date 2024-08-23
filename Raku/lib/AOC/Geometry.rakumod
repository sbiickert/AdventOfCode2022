unit module AOC::Geometry;

class Coord2D {...}
class Extent2D {...}

my %OFFSET_DIRS = ('N' 	=> Coord2D.new(x => 0, y => -1),
					'NE' 	=> Coord2D.new(x => 1, y =>-1),
					'E' 	=> Coord2D.new(x => 1, y => 0),
					'SE' 	=> Coord2D.new(x => 1, y => 1),
					'S' 	=> Coord2D.new(x => 0, y => 1),
					'SW' 	=> Coord2D.new(x =>-1, y => 1),
					'W' 	=> Coord2D.new(x =>-1, y => 0),
					'NW' 	=> Coord2D.new(x =>-1, y =>-1));
					
					
my %OFFSET_ALIASES = ('UP'	=> 'N', 'RIGHT' => 'E', 'DOWN' 	=> 'S', 'LEFT' 	=> 'W',
						'^' => 'N', '>' => 'E', 'v' => 'S', '<' => 'W');

sub resolve_alias(Str $dir --> Str) {
	if %OFFSET_DIRS{$dir}:exists {
		return $dir;
	}
	return %OFFSET_ALIASES{$dir} // Nil;
}

constant ROOK is export = 'ROOK';
constant BISHOP is export = 'BISHOP';
constant QUEEN is export = 'QUEEN';
my %ADJACENCY_RULES = ('ROOK'    => ['N','E','S','W'],
						'BISHOP' => ['NE','SE','SW','NW'],
						'QUEEN'  => ['N','NE','E','SE','S','SW','W','NW']);
						
class Coord2D is export {
	has Int $.x = 0;
	has Int $.y = 0;
	
	# class method: construct from two Ints
	method from_ints(::?CLASS:U $c2d: Int $x, Int $y) {
		$c2d.new( x => $x, y => $y );
	}
	
	# class method: origin
	method origin(::?CLASS:U $c2d:) {
		$c2d.new(x => 0, y => 0);
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

	method col() returns Int { $.x; }
	method row() returns Int { $.y; }
	
	method add(Coord2D $other --> Coord2D) {
		Coord2D.new( x => $.x + $other.x, y => $.y + $other.y )
	}

	method offset(Str $direction --> Coord2D) {
		my $off = resolve_alias($direction);
		if $off {
			return %OFFSET_DIRS{$off};
		}
		return Coord2D.origin;
	}
	
	method is_adjacent(Coord2D $other, Str $rule = ROOK --> Bool) {
		if ($rule eq ROOK) {
			return self.manhattanDistanceTo($other) == 1;
		}
		elsif ($rule eq BISHOP) {
			return abs($!x - $other.x) == 1 &&
					abs($!y - $other.y) == 1;
		}
		#QUEEN
		return (self.manhattanDistanceTo($other) == 1) ||
				(abs($!x - $other.x) == 1 &&
				 abs($!y - $other.y) == 1)
	}
	
	
	method get_adjacent_coords(Str $rule = ROOK) {
		my @result = ();
		if (%ADJACENCY_RULES{$rule}:exists) {
			my @dirs = %ADJACENCY_RULES{$rule}.flat;
			for @dirs -> $dir {
				@result.push(self.offset($dir));
			}
		}
		return @result;
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