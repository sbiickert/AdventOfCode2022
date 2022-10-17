// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit aocutils;

Interface

Uses SysUtils, StrUtils, fgl;


Type
    AoCStringArray =   array Of String;
    AoCGStringArray =  array Of AoCStringArray;
    AoCIntArray =      array of Integer;
    AoCStringMap =     specialize TFPGMap<String, String>;

Function ReadInput(inputFilename: String):   AoCStringArray;
Function ReadGroupedInput(inputFilename: String):   AoCGStringArray;

Implementation

Function ReadInput(inputFilename: String):   AoCStringArray;

Var
    gString:   AoCGStringArray;
Begin
    gString := ReadGroupedInput(inputFilename);
    result := gString[0];
End;

Function ReadGroupedInput(inputFilename: String):   AoCGStringArray;

Var
    inputFile:   TextFile;
    line:   String;
    group:   AoCStringArray;
Begin
	result := [];
    WriteLn('Will read data from: ', inputFilename);
    Assign(inputFile, inputFilename);
    Reset(inputFile);
    SetLength(result, 0);
    SetLength(group, 0);

    // Make sure the ANSI string compiler flag is on, or will trunc at 255!
    While Not Eof(inputFile) Do
        Begin
            ReadLn(inputFile, line);
            If Length(line) = 0 Then
                Begin
                    SetLength(result, Length(result)+1);
                    result[Length(result)-1] := group;
                    SetLength(group, 0);
                    continue;
                End;
            SetLength(group, Length(group)+1);
            group[Length(group)-1] := line;
        End;

    If Length(group) > 0 Then
        Begin
            SetLength(result, Length(result)+1);
            result[Length(result)-1] := group;
        End;

    Close(inputFile);
End;

{ Utility function to transform an array of strings to ints }
Function StrArrayToIntArray(var input: AoCStringArray): AoCIntArray;
Var
    i: Integer;
Begin
	result := [];
    SetLength(result, Length(input));
    For i := 0 To Length(input)-1 Do
    Begin
        result[i] := StrToInt(input[i]);
    End;
End;

End.
