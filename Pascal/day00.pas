Program AOC_2022_Day00;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils;

Const
    IN_FILE = '../Input/day00_test.txt';

Var
    input: AoCStringArray;

Procedure SolvePart1(values: AoCStringArray);
Var
    x: Integer;
Begin
    WriteLn('Part 1: DESCRIPTION');
    WriteLn(Format('Part One Solution: %d', [13]));
End;

Procedure SolvePart2(values: AoCStringArray);
Var
    a, b, c: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
End;


Begin
    input := ReadInput(IN_FILE);
    SolvePart1(input);
    SolvePart2(input);
End.
