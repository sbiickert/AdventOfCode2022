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
	
	multi method get(Coord $key --> Any) {
		self.get($key.Str);
	}
	
	multi method get(Str $key --> Any) {
		if %!data{$key}:exists {
			return %!data{$key};
		}
		return $.default;
	}
	
	multi method get_glyph(Str $key --> Str) {
		my $value = self.get($key);
		return self!glyph_from_value($value);
	}
	
	multi method get_glyph(Coord $key --> Str) {
		my $value = self.get($key);
		return self!glyph_from_value($value);
	}
	
	method !glyph_from_value(Any $value --> Str) {
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
	
	method clear(Coord $key, Bool :$reset_extent = False) {
		%!data{$key.Str}:delete;
		self!reset_extent;
	}
	
	method extent(--> Extent) { return $!extent; }
	
	method !reset_extent() {
		$!extent = Extent.from_coords( self.coords );
	}
	
	method coords(Str $with_value = '') {
		my Coord @result = ();
		for %!data.keys -> $key {
			if $with_value.chars == 0 || self.get_glyph($key) eq $with_value {
				@result.push(Coord.from_str($key));
			}
		}
		return @result;
	}
	
	method histogram(Bool $include_unset = False --> Hash) {
		my Int %hist is default(0);
		
		my Coord @coords_to_summarize = ();
		if ($include_unset) {
			@coords_to_summarize = self.extent.all_coords;
		}
		else {
			@coords_to_summarize = self.coords;
		}
		
		for @coords_to_summarize -> $c {
			my $val = self.get_glyph($c);
			%hist{$val} = %hist{$val} + 1;
		}
		
		return %hist;
	}
	
	method neighbors(Coord $c --> Array) {
		return $c.get_adjacent_coords($.rule);
	}
	
	method print(:%markers = {}, Bool :$invert_y = False) {
		print self.sprint(markers => %markers, invert_y => $invert_y);
	}
	
	method sprint(:%markers = {}, Bool :$invert_y = False --> Str) {
		my $str = '';
		return $str if $!extent.is_empty;
		
		my $xmin = $!extent.min.x;
		my $xmax = $!extent.max.x;
		my $ymin = $!extent.min.y;
		my $ymax = $!extent.max.y;
		
		my @y_indexes = ();
		for ($ymin..$ymax) -> $y {
			@y_indexes.push($y);
		}
		@y_indexes = @y_indexes.reverse if $invert_y;

		for @y_indexes -> $y {
			my @row = ();
			for ($xmin..$xmax) -> $x {
				my $c = Coord.from_ints($x, $y);
				my $c_str = $c.Str;
				my $glyph = self.get_glyph($c_str);
				if %markers.keys.elems > 0 && (%markers{$c_str}:exists) {
					$glyph = %markers{$c_str};
				}
				@row.push($glyph);
			}
			@row.push("\n");
			$str ~= @row.join(' ');
		}
		
		return $str;
	}
}