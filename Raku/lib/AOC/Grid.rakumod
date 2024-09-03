unit module AOC::Grid;

use AOC::Geometry;

role GridGlyph is export {
	method glyph( --> Str) { ... }
}

class Grid is export {
	has %!data = Hash.new;
	has $.default = '.';
	has AOC::Geometry::AdjacencyRule $.rule is required;
	has Extent $!extent;
	
	submethod TWEAK {
		%!data = Hash.new;
		$!extent = Extent.new;
	}
	
	method get(Coord $key --> Any) {
		if %!data{$key.Str}:exists {
			return %!data{$key.Str};
		}
		return $.default;
	}
	
	method get_glyph(Coord $key --> Str) {
		my $value = self.get($key);
		if $value.isa(List) {
			return $value[0].Str;
		}
		elsif $value.isa(Map) {
			return $value{'glyph'}.Str;
		}
		elsif $value.does(GridGlyph) {
			return $value.glyph;
		}
		return $value.Str;
	}

	method set(Coord $key, Any $value) {
		%!data{$key.Str} = $value;
		
		# Update the extent to include the $coord
		if ! $.extent.contains($key) {
			my $temp = $.extent.expand_to_fit($key);
			$!extent = $temp;
		}
	}
	
	method clear(Coord $key, Bool $reset_extent = False) {
		%!data{$key.Str}:delete;
		self.reset_extent;
	}
	
	method extent(--> Extent) { return $!extent; }
	
	method !reset_extent() {
		$!extent = Extent.from_coords( self.coords );
	}
	
	method coords(Str $with_value = '') {
		my Coord @result = ();
		for %!data.keys -> $key {
			if $with_value.length > 0 && self.get_glyph($key) eq $with_value {
				@result.push($key);
			}
		}
		return @result;
	}
}