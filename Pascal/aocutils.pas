// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit aocutils;

Interface

Uses SysUtils, StrUtils, fgl, Classes;


Type
    AoCStringArray =     array Of String;
    AoCIntArray =        array of Integer;
    AoCStringListGroup = array of TStringList;
    AoCStringMap =       specialize TFPGMap<String, String>;

Function ReadInput(inputFilename: String):   TStringList;
Function ReadGroupedInput(inputFilename: String):   AoCStringListGroup;

Function StrListToIntArray(input: TStringList): AoCIntArray;
Function StrArrayToIntArray(var input: AoCStringArray): AoCIntArray;
Function SumIntArray(var input: AoCIntArray): Integer;


Implementation

Function ReadInput(inputFilename: String): TStringList;
Begin
    WriteLn('Will read data from: ', inputFilename);
	result := TStringList.Create;
    result.LoadFromFile(inputFilename);
End;

Function ReadGroupedInput(inputFilename: String): AoCStringListGroup;
Var
    allInput, group: TStringList;
    i: Integer;
Begin
	result := [];
    SetLength(result, 0);
    
    allInput := ReadInput(inputFilename);
    group := TStringList.Create;
    
	For i := 0 To allInput.Count-1 Do
	Begin
		If Length(allInput[i]) = 0 Then
		Begin
			SetLength(result, Length(result)+1);
			result[Length(result)-1] := group;
			group := TStringList.Create;
			continue;
		End;
		group.Add(allInput[i]);
	End;

	If group.Count > 0 Then
	Begin
		SetLength(result, Length(result)+1);
		result[Length(result)-1] := group;
	End;
End;

{ Utility function to transform an list of strings to array of ints }
Function StrListToIntArray(input: TStringList): AoCIntArray;
Var
	i: Integer;
Begin
	result := [];
	SetLength(result, input.Count);
	For i := 0 To input.Count-1 Do
		result[i] := StrToInt(input[i]);
End;

{ Utility function to transform an array of strings to array of ints }
Function StrArrayToIntArray(var input: AoCStringArray): AoCIntArray;
Var
    i: Integer;
Begin
	result := [];
    SetLength(result, Length(input));
    For i := 0 To Length(input)-1 Do
        result[i] := StrToInt(input[i]);
End;

Function SumIntArray(var input: AoCIntArray): Integer;
Var
	i: Integer;
Begin
	result := 0;
	For i := 0 To Length(input)-1 Do
		result := result + input[i];
End;

End.
