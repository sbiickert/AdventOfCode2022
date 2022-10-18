Program AocTest;

Uses
    AoCUtils, AoCSpatialUtils;

Const
    IN_FILE = '../Input/day00_test.txt';

Procedure TestSpatial();
Var
    c1, c2: Coord2D;
    ext: Extent2D;
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
    input: AoCStringArray;
    ginput: AoCGStringArray;
    i, j: Integer;
Begin
    // Testing Input Parsing
    input := ReadInput(IN_FILE);
    For i := 0 To Length(input)-1 Do
        Begin
            WriteLn(input[i]);
        End;

    ginput := ReadGroupedInput(IN_FILE);
    WriteLn(Length(ginput));
    WriteLn(ginput[0][0]);
    For i := 0 To Length(ginput)-1 Do
        Begin
            WriteLn('Group ', i);
            input := ginput[i];
            For j := 0 To Length(input)-1 Do
                Begin
                    WriteLn(input[j]);
                End;
        End;
End;

Procedure TestUtil;
Var
	sList: AoCStringArray;
	iList: AoCIntArray;
Begin
	sList := ['1', '2', '3'];
	iList := StrArrayToIntArray(sList);
	WriteLn('The sum is ', SumIntArray(iList));
End;

Begin
	TestSpatial;
	TestInputParsing;
	TestUtil;
End.
