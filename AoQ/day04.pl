#!/usr/bin/env perl
BEGIN {
    use Cwd;
    our $directory = cwd;
    our $local_lib = $ENV{"HOME"} . '/perl5/lib/perl5';
}

use lib $directory;
use lib $local_lib;

use Modern::Perl 2018;
use autodie;

# https://adventofqode.org
say "Advent of Qode 2022, Day 04: The Mars Code, More or Less";

#my $INPUT = "15836633945";
my $INPUT = "1758967813926954834739387556316877261673955457138785159694522215194946156673115191343322931592871189241819578934387926931386644638441741623361289577724598377583189635724779479387814434729691746974184498361779829931574221636858981264166417433311117525895219896764368595546422113452736292466441426881932791417222797761517396137674898773265159268962182559956838642449938482567441129944149453549762538217565743164556249883335746865912395644262547168978982517671739951769327357847346272394311591372268361687326519363449365844191889225836849732695982825934416437893137516437128571662893347398688221364925211788565227646422649574165589884549645597318828163788481269455728331732892474553172362558333948738737157556894578727681737716429996735128436234916847753242229329349159613127651411547649277122137595247462134486679554622372968149192811883973211684439127146627925998422876632697276735228656469732576714131643729191814427242974255156512949847933622335883267849658249211134387664747769414715135272757256742";

solve($INPUT);

exit( 0 );

sub solve {
	my $input = shift;
	my @nums = split('', $input);
	my $result = 0;
	my $factor = 1; # Flips between 1 and -1
	
	for my $num (@nums) {
		if ($num == 3) { $factor *= -1; $num = 0; }
		$result += $num * $factor;
	}

	say "AoQ: the number is " . $result;
}