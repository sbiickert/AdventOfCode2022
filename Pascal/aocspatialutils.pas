// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit aocspatialutils;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils, ContNrs;

Type
	Coord2D = Class
        Private
            _x, _y:   Integer;
            Procedure SetX(val: Integer);
            Procedure SetY(val: Integer);
        Public
            Constructor Create(ix, iy: Integer);
            Constructor Create(key: String); Virtual;
            Property X: Integer Read _x Write SetX;
            Property Y: Integer Read _y Write SetY;
            Function IsEqualTo(other: Coord2D): Boolean;
            Function DeltaTo(other: Coord2D): Coord2D;
            Function DistanceTo(other: Coord2D): Double;
            Function MDistanceTo(other: Coord2D): Integer;
            Procedure Print(); Virtual;
            Function AsKey(): String; Virtual;
    End;
    Coord2DArray =   array Of Coord2D;
    Coord2DPtr = ^Coord2D;
    
    Coord3D = class(Coord2D)
    	Private
    		_z: Integer;
            Procedure SetZ(val: Integer);
    	Public
            Constructor Create(ix, iy, iz: Integer);
            Constructor Create(key: String); Override;
            Property Z: Integer Read _z Write SetZ;
            Procedure Print(); Override;
            Function AsKey(): String; Override;
    End;
    Coord3DArray = array of Coord2D;
    Coord3DPtr = ^Coord3D;
    
    Extent2D = Class
    	Private
    		_min, _max:	Coord2D;
    	Public
    		Constructor Create(coords: Coord2DArray);
    		Function GetMin(): Coord2D;
    		Function GetMax(): Coord2D;
    		Function GetWidth(): Integer;
    		Function GetHeight(): Integer;
    		Function GetArea(): Integer;
    		Function Contains(coord: Coord2D): Boolean;
    		Function AllContainedCoords(): Coord2DArray;
    		Procedure Print();
    End;
    Extent2DPtr = ^Extent2D;
    
    Direction = (up, down, left, right);
    MapDirection = (n, s, e, w);
    Adjacency = (rook, bishop, queen);
    AoCStrPtr = ^String;
    
    Grid2D = Class
    	Private
    		_defaultValue: String;
    		_aRule: Adjacency;
    		_data: TFPHashList;
    	Public
    		Constructor Create(default: String; adjacency: Adjacency = rook);
    		Function GetValue(coord: Coord2D): String;
    		Procedure SetValue(v: String; coord: Coord2D);
    		Function GetPtr(coord: Coord2D): Pointer;
    		Procedure SetPtr(ptr: Pointer; coord: Coord2D);
    		Function GetExtent(): Extent2D;
    		Function GetCoords(): Coord2DArray;
    		Function GetCoords(withValue: String): Coord2DArray;
    		Function GetHistogram(): AoCIntegerMap;
    		Function GetNeighbourOffsets(): Coord2DArray;
    		Function GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
    		Procedure Print();
    End;

Implementation

// -------------------------------------------------------
// Coord2D
// -------------------------------------------------------

Constructor Coord2D.Create(ix, iy: Integer);
Begin
    _x := ix;
    _y := iy;
End;

Constructor Coord2D.Create(key: String);
Var
	numbers: TStringArray;
Begin
    numbers := SplitString(key, '|');
    _x := StrToInt(numbers[0]);
    _y := StrToInt(numbers[1]);
End;

Procedure Coord2D.SetX(val: Integer);
Begin
    _x := val;
End;

Procedure Coord2D.SetY(val: Integer);
Begin
    _y := val;
End;

Function Coord2D.IsEqualTo(other: Coord2D):   Boolean;
Begin
    If (X = other.X) And (Y = other.Y) Then
        result := true
    Else
        result := false;
End;

Function Coord2D.DeltaTo(other: Coord2D): Coord2D;
Begin
    result := Coord2D.Create(other.X - X, other.Y - Y);
End;

Function Coord2D.DistanceTo(other: Coord2D): Double;
Var
    delta: Coord2D;
Begin
    delta := DeltaTo(other);
    result := Sqrt(Sqr(delta.X) + Sqr(delta.Y));
End;

Function Coord2D.MDistanceTo(other: Coord2D): Integer;
Var
    delta: Coord2D;
Begin
    delta := DeltaTo(other);
    result := Abs(delta.X) + Abs(delta.Y);
End;

Procedure Coord2D.Print;
Begin
    WriteLn('Coord2D(', _x, ',', _y, ')');
End;

Function Coord2D.AsKey(): String;
Begin
	result := IntToStr(X) + '|' + IntToStr(Y);
End;

// -------------------------------------------------------
// Coord2D Utility Functions
// -------------------------------------------------------

Procedure PushCoord(coord: Coord2D; var arr: Coord2DArray);
Var
	len: Integer;
Begin
	len := Length(arr)+1;
	SetLength(arr, len);
	arr[len-1] := coord;
End;


// -------------------------------------------------------
// Coord3D
// -------------------------------------------------------

Constructor Coord3D.Create(ix, iy, iz: Integer);
Begin
    _x := ix;
    _y := iy;
    _z := iz;
End;

Constructor Coord3D.Create(key: String);
Var
	numbers: TStringArray;
Begin
    numbers := SplitString(key, '|');
    _x := StrToInt(numbers[0]);
    _y := StrToInt(numbers[1]);
    _z := StrToInt(numbers[2]);
End;

Procedure Coord3D.SetZ(val: Integer);
Begin
    _z := val;
End;

Procedure Coord3D.Print(); 
Begin
    WriteLn('Coord3D(', _x, ',', _y, ',', _z, ')');
End;

Function Coord3D.AsKey(): String;
Begin
	result := IntToStr(X) + '|' + IntToStr(Y) + '|' + IntToStr(Z);
End;

// -------------------------------------------------------
// Coord3D Utility Functions
// -------------------------------------------------------

Procedure PushCoord(coord: Coord3D; var arr: Coord3DArray);
Var
	len: Integer;
Begin
	len := Length(arr)+1;
	SetLength(arr, len);
	arr[len-1] := coord;
End;


// -------------------------------------------------------
// Extent2D
// -------------------------------------------------------

Constructor Extent2D.Create(coords: Coord2DArray);
Var
	xmin, xmax, ymin, ymax: Integer;
	i: Integer;
Begin
	xmin := coords[0].X;
	xmax := coords[0].X;
	ymin := coords[0].Y;
	ymax := coords[0].Y;
	for i := 1 to Length(coords)-1 do
	Begin
		xmin := Min(xmin, coords[i].X);
		xmax := Max(xmax, coords[i].X);
		ymin := Min(ymin, coords[i].Y);
		ymax := Max(ymax, coords[i].Y);
	End;
	_min := Coord2D.Create(xmin, ymin);
	_max := Coord2D.Create(xmax, ymax);
End;

Function Extent2D.GetMin(): Coord2D;
Begin
	result := _min;
End;

Function Extent2D.GetMax(): Coord2D;
Begin
	result := _max;
End;

Function Extent2D.GetWidth(): Integer;
Begin
	result := _max.X - _min.X + 1;
End;

Function Extent2D.GetHeight(): Integer;
Begin
	result := _max.Y - _min.Y + 1;
End;

Function Extent2D.GetArea(): Integer;
Begin
	result := GetWidth * GetHeight;
End;

Function Extent2D.Contains(coord: Coord2D): Boolean;
Begin
	result := (GetMin.X <= coord.X)
		and (coord.X <= GetMax.X)
		and (GetMin.Y <= coord.Y)
		and (coord.Y <= GetMax.Y);
End;

Function Extent2D.AllContainedCoords(): Coord2DArray;
Var
	i, x, y: Integer;
Begin
	result := [];
	SetLength(result, GetArea);
	i := 0;
	
	For x := GetMin.X To GetMax.X Do
		For y := GetMin.Y To GetMax.Y Do
		Begin
			result[i] := Coord2D.Create(x,y);
			inc(i);
		End;
End;

Procedure Extent2D.Print();
Begin
	WriteLn('Extent2D(');
	Write('  Min: ');
	GetMin.Print;
	Write('  Max: ');
	GetMax.Print;
	WriteLn(')');
End;


// -------------------------------------------------------
// Grid2D
// -------------------------------------------------------

Constructor Grid2D.Create(default: String; adjacency: Adjacency = rook);
Begin
	_defaultValue := default;
	_aRule := adjacency;
	_data := TFPHashList.Create;
End;

Function Grid2D.GetValue(coord: Coord2D): String;
Var
	key: String;
	strPtr: AoCStrPtr;
	idx: Integer;
Begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := _defaultValue
	Else
	Begin
		strPtr := _data[idx];
		result := strPtr^;
	End;
End;

Procedure Grid2D.SetValue(v: String; coord: Coord2D);
Var
	key: String;
	idx: Integer;
	ptr: ^String;
Begin
	// https://www.tutorialspoint.com/pascal/pascal_memory.htm
	New(ptr);
	If Not Assigned(ptr) Then
	Begin
		key := coord.AsKey;
		WriteLn('Error - Unable to allocate memory to set ', v, ' at ', key);
		Exit;
	End;
	
	ptr^ := v;
	SetPtr(ptr, coord);
End;

Function Grid2D.GetPtr(coord: Coord2D): Pointer;
Var
	key: String;
	idx: Integer;
Begin
	key := coord.AsKey;
	idx := _data.FindIndexOf(key);
	If idx = -1 Then
		result := @_defaultValue
	Else
	Begin
		result := _data[idx];
	End;
End;

Procedure Grid2D.SetPtr(ptr: Pointer; coord: Coord2D);
Var
	key: String;
	idx: Integer;
Begin
	key := coord.AsKey;
	// There doesn't seem to be a function to replace the value for a key
	idx := _data.FindIndexOf(key);
	If idx <> -1 Then
		_data.Delete(idx);

	_data.Add(key, ptr);
End;

Function Grid2D.GetExtent(): Extent2D;
Begin
	result := Extent2D.Create(GetCoords);
End;

Function Grid2D.GetCoords(): Coord2DArray;
Var
	i: Integer;
	c: Coord2D;
Begin
	result := [];
    For i := 0 To _data.Count-1 Do
    Begin
    	c := Coord2D.Create(_data.NameOfIndex(i));
    	PushCoord(c, result);
    End;
End;

Function Grid2D.GetCoords(withValue: String): Coord2DArray;
Var
	key: String;
	idx: Integer;
	strPtr: AoCStrPtr;
	val: String;
	c: Coord2D;
Begin
	result := [];
    For idx := 0 To _data.Count-1 Do
    Begin
    	key := _data.NameOfIndex(idx);
    	strPtr := _data[idx];
    	val := strPtr^;
    	If val = withValue Then
    	Begin
			c := Coord2D.Create(key);
			PushCoord(c, result);
    	End;
    End;
End;

Function Grid2D.GetHistogram(): AoCIntegerMap;
Var
	idx: Integer;
	key: String;
	strPtr: AoCStrPtr;
	val: String;
Begin
	result := AoCIntegerMap.Create;
    For idx := 0 To _data.Count-1 Do
    Begin
    	key := _data.NameOfIndex(idx);
    	strPtr := _data[idx];
    	val := strPtr^;
    	If (result.IndexOf(val) = -1) Then
    		result[val] := 0;
    	result[val] := result[val] + 1;
	End;	
End;

Function Grid2D.GetNeighbourOffsets(): Coord2DArray;
Begin
	result := [];
	If (_aRule = queen) Or (_aRule = rook) Then
	Begin
		PushCoord(Coord2D.Create(-1,  0), result);
		PushCoord(Coord2D.Create( 1,  0), result);
		PushCoord(Coord2D.Create( 0, -1), result);
		PushCoord(Coord2D.Create( 0,  1), result);
	End;
	If (_aRule = queen) Or (_aRule = bishop) Then
	Begin
		PushCoord(Coord2D.Create(-1, -1), result);
		PushCoord(Coord2D.Create( 1,  1), result);
		PushCoord(Coord2D.Create(-1,  1), result);
		PushCoord(Coord2D.Create( 1, -1), result);
	End;
End;

Function Grid2D.GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
Var
	offsets: Coord2DArray;
	i: Integer;
	c: Coord2D;
Begin
	result := [];
	offsets := GetNeighbourOffsets;
	For i := 0 To Length(offsets)-1 Do
	Begin
		c := offsets[i];
		PushCoord(Coord2D.Create(fromCoord.X+c.X, fromCoord.Y+c.Y), result);
	End;
End;

Procedure Grid2D.Print();
Var
	r, c: Integer;
	ext: Extent2D;
Begin
	ext := GetExtent;
	For r := ext.GetMin.Y To ext.GetMax.Y Do
	Begin
		For c := ext.GetMin.X To ext.GetMax.X Do
		Begin
			Write(GetValue(Coord2D.Create(c, r)));
		End;
		WriteLn;
	End;
End;



End.