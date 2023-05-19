unit module AOC::Util;

sub read_input(Str $filename, Bool $remove_empty_lines = False) is export {
	my @content = $filename.IO.lines;
	
	if $remove_empty_lines {
		@content = grep { .Str.chars > 0 }, @content;
	}
	
	return @content;
}

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
		say @groups;
		return $group_index < @groups.elems ?? @groups[$group_index].flat !! [];
	}
	
	return @groups;
}