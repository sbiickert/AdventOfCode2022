#!/usr/bin/env raku

constant TEMPLATE = 'aoc_template.raku';
my $tag := '<##>';

sub MAIN(Int $day, Str $challenge_name) {
	my @lines = TEMPLATE.IO.lines;
	my $pad_day = '%02s'.sprintf($day);
	@lines = @lines.map(-> $line {
		my $temp = $line.subst("$tag, Day $tag: $tag", "2022, Day $day: $challenge_name", :g);
		$temp = $temp.subst($tag, $pad_day);
	});
	
	my $filename = "day$pad_day.raku";
	!$filename.IO.e or die "File $filename already exists. Exiting.";
	
	my $fh = open $filename, :w;
	for @lines -> $line {
		$fh.say($line);
	}
	$fh.close;
	shell("chmod +x $filename; open $filename");
	
}
