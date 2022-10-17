program StringTesting;

uses
    StrUtils, SysUtils, RegExpr;

var
    testString: String;
    x: String;
    whitespace: SysUtils.TSysCharSet;
    words: TStringArray;
    i: Integer;
    wc: Integer;
    word: String;
    re: TRegExpr;
begin
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
end.
