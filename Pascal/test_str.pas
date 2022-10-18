program StringTesting;

uses
    StrUtils, SysUtils, RegExpr, Classes;

Procedure TestParsing();
Var
    testString: String;
    x: String;
    whitespace: SysUtils.TSysCharSet;
    words: TStringArray;
    i: Integer;
    wc: Integer;
    word: String;
Begin
    testString := 'I wish I were an Oscar Meyer wiener.';
    WriteLn(testString);
    WriteLn(StartsStr('I', testString));
    x := Copy2Space(testString);
    WriteLn(x);
    whitespace := [' ', ','];
    x := ExtractWord(6, testString, whitespace);
    WriteLn(x);

    testString := '1,2,3,4,5,6,7,8,9,0';
    x := ReplaceText(testString, ',', ' ');
    WriteLn(x);
    wc := WordCount(x, whitespace);
    SetLength(words, wc);
    for i := 1 to wc do
    begin
        word := ExtractWord(i, x, whitespace);
        WriteLn(word);
        words[i-1] := word;
    end;
    WriteLn(words[5]);

    testString := 'One,two,three,four five';
    words := SplitString(testString, ',');
    WriteLn('SplitString: ', Length(words));
    WriteLn('SplitString: ', words[3]);
    WriteLn('The second letter is ', words[3][2]); 
End;

Procedure TestRegex();
Var
    re: TRegExpr;
    testString: String;	
Begin
    testString := 'One,two,three,four five';
    
    // https://regex.sorokin.engineer/en/latest/tregexpr.html
    re := TRegExpr.Create(',\w+ \w+');
    If re.Exec(testString) Then WriteLn(re.Match[0]);
    re.Free;
    
    re := TRegExpr.Create('\b(t\w+)\b');
    If re.Exec(testString) Then
    Begin
    	WriteLn(re.Match[1]);
		While re.ExecNext Do
		Begin
		  WriteLn(re.Match[1]);
		End;
    End;
    re.Free;
End;

Procedure TestList();
Var
	l: TStringList;
	i: Integer;
Begin
	// https://wiki.freepascal.org/TStringList-TStrings_Tutorial
	l := TStringList.Create;
	l.Add('First string');
	l.Add('Second string');
	l.Add('Third string');
	
	For i := 0 To l.Count-1 Do
		WriteLn(l[i]);
		
	l.Free;
End;

begin
    TestParsing;
    TestRegex;
    TestList;
end.
