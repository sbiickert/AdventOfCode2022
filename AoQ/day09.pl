#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2022;
use autodie;

# https://adventofqode.org
say "Advent of Qode 2022, Day 09: Ham Radio";

open my $train_en, '<', '1984.txt' or die "Failed to open input: $!";
our @TRAIN_EN = <$train_en>;
close $train_en;

open my $train_pig, '<', '1984.pig.txt' or die "Failed to open input: $!";
our @TRAIN_PIG = <$train_pig>;
close $train_pig;

our %sep = ('hufff' => 1, 'huff' => 1, 'wheez' => 1, 'wheeze' => 1, 'squee' => 1);
our %dict;
build_dictionary();

my $input = "ooo squeeee ooink sque squeeee eeee huff shooort ooink ooo snort ooo sque ooo hufff ooo ooo oink ooink huff squeeee sque oink eee shooort eee ooink huff eeee shooort sque eyyyee shooort eeee snort wheez eieie sque ooink snort sque wheeze sque ooo eieie ooink shooort oink eeee hufff squeeee oink sque shooort eeee sque snort wheeze squeeee ooo ooink ee oink eyyyee shooort hufff ooo eieie eee squeeee ooink ooo eyyyee huff shooort squeeee ooo eieie eieie huff snort eieie ee snort eieie eee eieie wheeze snort eee shooort shooort sque squeeee";

solve($input);

exit( 0 );

sub build_dictionary {
	
}

sub solve {
	my $pig_phrase = shift;
}
