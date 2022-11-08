Program AocTest;

Uses
    AoCUtils, AoCSpatialUtils, Classes;

Const
    IN_FILE = '../Input/day00_test.txt';

Procedure TestSpatial();
Var
    c1, c2: Coord2D;
    ext, extOut: Extent2D;
    extPtr: Extent2DPtr;
	grid: Grid2D;
	neighbours: Coord2DArray;
	c3: Coord3D;
	key: String;
	i: Integer;
Begin
    // Testing Spatial Functions
    c1 := Coord2D.Create(0, 0);
    c2 := Coord2D.Create(5, 10);
    WriteLn(c1.Equals(c2));
    c1.DeltaTo(c2).Print;
    c2.DeltaTo(c1).Print;
    c1.Print;
    WriteLn(c1.DistanceTo(c2):4:2);
    WriteLn(c1.MDistanceTo(c2));
    
    ext := Extent2D.Create([c1,c2]);
    ext.Print();
	WriteLn('This extent has an area of ', ext.GetArea());

    c1 := Coord2D.Create('0|4');
    c1.Print;
    c2 := Coord2D.Create(c2.AsKey);
    c2.Print;
    
    WriteLn('Grid testing');
    
    grid := Grid2D.Create('.', queen);
    grid.SetValue('X', c1);
    grid.SetValue('*', c2);
    neighbours := grid.GetNeighbourCoords(c2);
    For i := 0 To Length(neighbours)-1 Do
    Begin
    	grid.SetValue('N', neighbours[i]);
    	neighbours[i].Print;
    End;
    grid.Print;
    
    ext.Print;
    New(extPtr);
    extPtr^ := ext;
    grid.SetPtr(extPtr, c1);
    extPtr := grid.GetPtr(c1);
    extOut := extPtr^;
    extOut.Print;

	c3 := Coord3D.create(1,2,3);
	c3.Print;
	key := c3.AsKey;
	WriteLn(key);
	key := '20|30|35';
	c3 := Coord3D.Create(key);
	c3.Print;
	c3.Z := 40;
	c3.Print;
End;

Procedure TestInputParsing();
Var
    input: TStringList;
    gInput: AoCStringListGroup;
    i, j: Integer;
Begin
    // Testing Input Parsing
    input := ReadInput(IN_FILE);
    For i := 0 To input.Count-1 Do
	Begin
		WriteLn(input[i]);
	End;

    gInput := ReadGroupedInput(IN_FILE);
    WriteLn(Length(gInput));
    For i := 0 To Length(gInput)-1 Do
	Begin
		WriteLn('Group ', i);
		input := gInput[i];
		For j := 0 To input.Count-1 Do
		Begin
			WriteLn(input[j]);
		End;
	End;
End;

Procedure TestUtil;
Var
	sArr: AoCStringArray;
	iArr: AoCIntArray;
	sList: TStringList;
Begin
	sArr := ['1', '2', '3'];
	iArr := StrArrayToIntArray(sArr);
	WriteLn('The sum is ', SumIntArray(iArr));
	
	sList := TStringList.Create;
	sList.Delimiter := ',';
	sList.DelimitedText := '4,5,6';
	iArr := StrListToIntArray(sList);
	WriteLn('The sum is ', SumIntArray(iArr));
	sList.Free;
End;

Begin
	TestSpatial;
	TestInputParsing;
	TestUtil;
End.
