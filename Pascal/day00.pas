Program AOC_2022_Day00;
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Uses SysUtils, StrUtils, AoCUtils, Classes;

Const
    IN_FILE = '../Input/day00_test.txt';

Procedure SolvePart1(values: TStringList);
Var
    x: Integer;
Begin
    WriteLn('Part 1: DESCRIPTION');
    WriteLn(Format('Part One Solution: %d', [13]));
End;

Procedure SolvePart2(values: TStringList);
Var
    a, b, c: Integer;
Begin
    WriteLn('Part 2: DESCRIPTION');
    WriteLn(Format('Part Two Solution: %d', [13]));
End;

Var
    input: TStringList;
Begin
    input := ReadInput(IN_FILE);
    SolvePart1(input);
    SolvePart2(input);
End.
