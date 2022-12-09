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
use Digest::MD5 qw(md5 md5_hex md5_base64);
use Data::Dumper;

# https://adventofqode.org
say "Advent of Qode 2022, Day 09: Ham Radio";

open my $train_en_file, '<', '1984.txt' or die "Failed to open input: $!";
our @TRAIN_EN = <$train_en_file>;
close $train_en_file;

open my $train_pig_file, '<', '1984.pig.txt' or die "Failed to open input: $!";
our $TRAIN_PIG = <$train_pig_file>;
close $train_pig_file;

our %separators = ('hufff' => 1, 'huff' => 1, 'wheez' => 1, 'wheeze' => 1, 'squee' => 1);

my $input = "ooo squeeee ooink sque squeeee eeee huff shooort ooink ooo snort ooo sque ooo hufff ooo ooo oink ooink huff squeeee sque oink eee shooort eee ooink huff eeee shooort sque eyyyee shooort eeee snort wheez eieie sque ooink snort sque wheeze sque ooo eieie ooink shooort oink eeee hufff squeeee oink sque shooort eeee sque snort wheeze squeeee ooo ooink ee oink eyyyee shooort hufff ooo eieie eee squeeee ooink ooo eyyyee huff shooort squeeee ooo eieie eieie huff snort eieie ee snort eieie eee eieie wheeze snort eee shooort shooort sque squeeee";

our %dict;
build_dictionary();

solve($input);

exit( 0 );

sub build_dictionary {
	my @pig_training_words = parse_pig_words($TRAIN_PIG);
	my $temp = join(' ', @TRAIN_EN);
	my @english_training_words = parse_english_words($temp);
	
	die unless scalar(@pig_training_words) == scalar(@english_training_words);
	
	for my $i (0..$#pig_training_words) {
		#say "$pig_training_words[$i] = $english_training_words[$i]";
		if (exists($dict{$pig_training_words[$i]}) && $dict{$pig_training_words[$i]} ne $english_training_words[$i]) {
			die "$pig_training_words[$i] was already set to $dict{$pig_training_words[$i]}";
		}
		$dict{$pig_training_words[$i]} = $english_training_words[$i];
	}
}

sub parse_pig_words {
	my $pig_phrase = shift;
	
	for my $sep (keys %separators) {
		$pig_phrase =~ s/\b$sep\b/ /g;
	}
	my @pig_words = split(/ {2,}/, $pig_phrase);
	return @pig_words;
}

sub parse_english_words {
	my $text = shift;
	
	$text =~ s/\n/ /g;
	$text =~ s/\*{5}/XXXXX/g;
	$text =~ s/[^\w\s]//g;
	$text = lc($text);
	my @words = split(/ +/, $text);
	
	return @words;
}

sub solve {
	my $pig_phrase = shift;
	my @pig_words = parse_pig_words($pig_phrase);
	#say join('|', @pig_words);
	
	my @english_words = ();
	
	for my $word (@pig_words) {
		die "Pig word $word not in dict.\n" if not exists($dict{$word});
		push(@english_words, $dict{$word});
	}
	
	my $english = join(' ', @english_words);
	say $english;
	
	my $digest = md5_hex($english);
	#say $digest;
	($digest eq '137d640d9381389ce14c8c7cb872b853') or die "Incorrect.";
}
