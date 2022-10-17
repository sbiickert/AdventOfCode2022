program HashTesting;

uses SysUtils, fgl, ContNrs;

type
    TMap = specialize TFPGMap<String, Integer>;
    PSBRec = ^SBRec;
    SBRec = record
        a: String;
        b: Integer;
    end;

var
    x: TMap;
    i: Integer;
    hlist: TFPHashList;
    ptr: Pointer;
    val: Integer;
    strval: String;
    idx: Integer;
    rec: SBRec;
begin
    // Simpler to use, but slow
    x := TMap.Create;
    x['one'] := 1;
    x['two'] := 2;
    WriteLn(x['one']);
    For i := 0 to x.Count-1 do
    	WriteLn(x.Keys[i]);
    i := x.IndexOf('three'); # -1
    WriteLn(IntToStr(i));
    
    x.Free;

    // High performance
{
    hlist := TFPHashList.Create;
    val := 1;
    ptr := @val;
    hlist.Add('one', ptr);
    idx := hlist.FindIndexOf('one');
    WriteLn(PInteger(hlist[idx])^);

    strval := 'bob';
    ptr := @strval;
    hlist.Add('two', ptr);
    idx := hlist.FindIndexOf('two');
    WriteLn(PString(hlist[idx])^);

    rec.a := 'sam';
    rec.b := 42;
    ptr := @rec;
    hlist.Add('three', ptr);
    idx := hlist.FindIndexOf('three');
    rec := PSBRec(hlist[idx])^;
    WriteLn(rec.a);

    hlist.Free;
}
end.