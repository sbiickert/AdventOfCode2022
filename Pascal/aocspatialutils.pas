// Utility module for Advent of Code
{$mode objfpc} // directive to be used for defining classes
{$m+}          // directive to be used for using constructor

Unit aocspatialutils;

Interface

Uses SysUtils, StrUtils, Math, AoCUtils;

Type
	Coord2D = Class
        Private
            _x, _y:   Integer;
        Public
            Constructor Create(x, y: Integer);
            Constructor Create(key: String); Virtual;
            Function GetX(): Integer;
            Function GetY(): Integer;
            Function IsEqualTo(other: Coord2D): Boolean;
            Function DeltaTo(other: Coord2D): Coord2D;
            Function DistanceTo(other: Coord2D): Double;
            Function MDistanceTo(other: Coord2D): Integer;
            Procedure Print(); Virtual;
            Function AsKey(): String; Virtual;
    End;
    Coord2DArray =   array Of Coord2D;
    
    Coord3D = class(Coord2D)
    	Private
    		_z: Integer;
    	Public
            Constructor Create(x, y, z: Integer);
            Constructor Create(key: String); Override;
            Function GetZ(): Integer;
            Procedure Print(); Override;
            Function AsKey(): String; Override;
    End;
    
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
    		Procedure Print();
    End;
    
    Direction = (up, down, left, right);
    MapDirection = (n, s, e, w);
    Adjacency = (rook, bishop, queen);
    
    Grid2D = Class
    	Private
    		_defaultValue: String;
    		_aRule: Adjacency;
    		_data: AoCStringMap;
    	Public
    		Constructor Create(default: String; adjacency: Adjacency = rook);
    		Function GetValue(coord: Coord2D): String;
    		Function SetValue(v: String; coord: Coord2D): String;
    		Function GetExtent(): Extent2D;
    		Function GetCoords(): Coord2DArray;
    		Function GetCoords(withValue: String): Coord2DArray;
    		Function GetNeighbourOffsets(): Coord2DArray;
    		Function GetNeighbourCoords(fromCoord: Coord2D): Coord2DArray;
    		Procedure Print();
    End;

Implementation

// -------------------------------------------------------
// Coord2D
// -------------------------------------------------------

Constructor Coord2D.Create(x, y: Integer);
Begin
    _x := x;
    _y := y;
End;

Constructor Coord2D.Create(key: String);
Var
	words: TStringArray;
Begin
    words := SplitString(key, '|');
    _x := StrToInt(words[0]);
    _y := StrToInt(words[1]);
End;

Function Coord2D.GetX(): Integer;
Begin
    result := _x;
End;

Function Coord2D.GetY(): Integer;
Begin
    result := _y;
End;

Function Coord2D.IsEqualTo(other: Coord2D):   Boolean;
Begin
    If (GetX = other.GetX) And (GetY = other.GetY) Then
        result := true
    Else
        result := false;
End;

Function Coord2D.DeltaTo(other: Coord2D): Coord2D;
Begin
    result := Coord2D.Create(other.GetX - GetX, other.GetY - GetY);
End;

Function Coord2D.DistanceTo(other: Coord2D): Double;
Var
    delta: Coord2D;
Begin
    delta := DeltaTo(other);
    result := Sqrt(Sqr(delta.GetX) + Sqr(delta.GetY));
End;

Function Coord2D.MDistanceTo(other: Coord2D): Integer;
Var
    delta: Coord2D;
Begin
    delta := DeltaTo(other);
    result := Abs(delta.GetX) + Abs(delta.GetY);
End;

Procedure Coord2D.Print;
Begin
    WriteLn('Coord2D(', _x, ',', _y, ')');
End;

Function Coord2D.AsKey(): String;
Begin
	result := IntToStr(GetX) + '|' + IntToStr(GetY);
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


Constructor Coord3D.Create(x, y, z: Integer);
Begin
    _x := x;
    _y := y;
    _z := z;
End;

Constructor Coord3D.Create(key: String);
Var
	words: TStringArray;
Begin
    words := SplitString(key, '|');
    _x := StrToInt(words[0]);
    _y := StrToInt(words[1]);
    _z := StrToInt(words[2]);
End;

Function Coord3D.GetZ(): Integer;
Begin
	result := _z
End;

Procedure Coord3D.Print(); 
Begin
    WriteLn('Coord3D(', _x, ',', _y, ',', _z, ')');
End;

Function Coord3D.AsKey(): String;
Begin
	result := IntToStr(GetX) + '|' + IntToStr(GetY) + '|' + IntToStr(GetZ);
End;



// -------------------------------------------------------
// Extent2D
// -------------------------------------------------------

Constructor Extent2D.Create(coords: Coord2DArray);
Var
	xmin, xmax, ymin, ymax: Integer;
	i: Integer;
Begin
	xmin := coords[0].GetX;
	xmax := coords[0].GetX;
	ymin := coords[0].GetY;
	ymax := coords[0].GetY;
	for i := 1 to Length(coords)-1 do
	Begin
		xmin := Min(xmin, coords[i].GetX);
		xmax := Max(xmax, coords[i].GetX);
		ymin := Min(ymin, coords[i].GetY);
		ymax := Max(ymax, coords[i].GetY);
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
	result := _max.GetX - _min.GetX;
End;

Function Extent2D.GetHeight(): Integer;
Begin
	result := _max.GetY - _min.GetY;
End;

Function Extent2D.GetArea(): Integer;
Begin
	result := GetWidth * GetHeight;
End;

Function Extent2D.Contains(coord: Coord2D): Boolean;
Begin
	result := (GetMin.GetX <= coord.GetX)
		and (coord.GetX <= GetMax.GetX)
		and (GetMin.GetY <= coord.GetY)
		and (coord.GetY <= GetMax.GetY);
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
	_data := AoCStringMap.Create;
End;

Function Grid2D.GetValue(coord: Coord2D): String;
Var
	key: String;
	i: Integer;
Begin
	key := coord.AsKey;
	i := _data.IndexOf(key);
	If i >= 0 Then
		result := _data[key]
	Else
		result := _defaultValue;
End;

Function Grid2D.SetValue(v: String; coord: Coord2D): String;
Var
	key: String;
Begin
	key := coord.AsKey;
	_data[key] := v;
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
    	c := Coord2D.Create(_data.Keys[i]);
    	PushCoord(c, result);
    End;
End;

Function Grid2D.GetCoords(withValue: String): Coord2DArray;
Var
	key: String;
	i: Integer;
	c: Coord2D;
Begin
	result := [];
    For i := 0 To _data.Count-1 Do
    Begin
    	key := _data.Keys[i];
    	If _data[key] = withValue Then
    	Begin
			c := Coord2D.Create(key);
			PushCoord(c, result);
    	End;
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
		PushCoord(Coord2D.Create(fromCoord.GetX+c.GetX, fromCoord.GetY+c.GetY), result);
	End;
End;

Procedure Grid2D.Print();
Var
	r, c: Integer;
	ext: Extent2D;
Begin
	ext := GetExtent;
	For r := ext.GetMin.GetY To ext.GetMax.GetY Do
	Begin
		For c := ext.GetMin.GetX To ext.GetMax.GetX Do
		Begin
			Write(GetValue(Coord2D.Create(c, r)));
		End;
		WriteLn;
	End;
End;



End.