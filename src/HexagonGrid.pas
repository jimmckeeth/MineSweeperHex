unit HexagonGrid;

interface

uses
	System.Types, System.Math, System.UITypes, System.Generics.Collections,  System.Skia;

type
  THexagonInfo = record
    Center: TPointF;
    Points: array[0..5] of TPointF;
    Column, Row: Integer;
    HasMine: Boolean;
    IsRevealed: Boolean;
    IsFlagged: Boolean;
		NearbyMines: Integer;
		Path: ISkPath;
    Size: Single;
    procedure CalculatePoints(const ACellSize: Single);
  end;
  THexagonInfoArray = array of THexagonInfo;

  THexagonGrid = class
  private
    FCellSize: Single;
    FGridSize: Integer;
    FXOffset, FYOffset: Single;
    FHexagons: THexagonInfoArray;
    FMineCount: Integer;
    FFirstClick: Boolean;
    procedure CalculateHexagons;
    procedure CalculateNearbyMines;
    function GetNeighbors(const AIndex: Integer): TArray<Integer>;
    procedure PlaceMines(const AFirstClickIndex: Integer);
    function GetFlagCount: Integer;
  public
    constructor Create(const AGridSize: Integer);
    procedure UpdateSize(const AWidth, AHeight: Single);
    function FindHexagonAt(const X, Y: Single): Integer;
    function RevealCell(const AIndex: Integer): Boolean;
      // Returns True if mine hit
    procedure FlagCell(const AIndex: Integer);
    function CheckWin: Boolean;
    property GridSize: Integer read FGridSize;
    property CellSize: Single read FCellSize;
    property XOffset: Single read FXOffset;
    property YOffset: Single read FYOffset;
    property FlagCount: Integer read GetFlagCount;
    property Hexagons: THexagonInfoArray read FHexagons;
    property MineCount: Integer read FMineCount write FMineCount;
  end;

implementation

{ THexagonInfo }

procedure THexagonInfo.CalculatePoints(const ACellSize: Single);
var
  I: Integer;
  Angle: Single;
begin
  Self.Size := ACellSize;
  for I := 0 to 5 do
  begin
    Angle := I * 60 * Pi / 180;
    Points[I].X := Center.X + ACellSize * Cos(Angle);
    Points[I].Y := Center.Y + ACellSize * Sin(Angle);
  end;
end;

{ THexagonGrid }

constructor THexagonGrid.Create(const AGridSize: Integer);
begin
  inherited Create;
  FGridSize := AGridSize;
  FMineCount := (AGridSize * AGridSize) div 6;
  FFirstClick := True;
  SetLength(FHexagons, AGridSize * AGridSize);
end;

procedure THexagonGrid.CalculateHexagons;
var
  X, Y: Integer;
  Index: Integer;
begin
  Index := 0;
  for Y := 0 to FGridSize - 1 do
    for X := 0 to FGridSize - 1 do
    begin
      if (X mod 2 = 1) and (Y >= FGridSize - 1) then
        Continue;

      FHexagons[Index].Column := X;
      FHexagons[Index].Row := Y;
      FHexagons[Index].Center.X := X * FCellSize * 1.5 + FXOffset;
      FHexagons[Index].Center.Y := Y * FCellSize * Sqrt(3) + FYOffset + FCellSize
        * Sqrt(3) / 2;
      if X mod 2 = 1 then
        FHexagons[Index].Center.Y := FHexagons[Index].Center.Y + FCellSize *
          Sqrt(3) / 2;

      FHexagons[Index].CalculatePoints(FCellSize);
      Inc(Index);
    end;
  SetLength(FHexagons, Index);
end;

procedure THexagonGrid.UpdateSize(const AWidth, AHeight: Single);
var
  HexWidth, HexHeight: Single;
begin
  HexWidth := (AWidth - 2) / (FGridSize * 1.5 + 0.5);
  HexHeight := (AHeight - 2) / ((FGridSize * 2) * 0.866);
  FCellSize := Min(HexWidth, HexHeight);

  FXOffset := (AWidth - (FGridSize * FCellSize * 1.5)) / 2 + FCellSize / 4 * 3;
  FYOffset := (AHeight - (FGridSize * FCellSize * Sqrt(3))) / 2;

  CalculateHexagons;
end;

function THexagonGrid.FindHexagonAt(const X, Y: Single): Integer;
var
	I: Integer;
	PathBuilder: ISkPathBuilder;
begin
	Result := -1;
	for I := 0 to High(FHexagons) do
	begin
		if not assigned(FHexagons[i].Path) then
		begin
			PathBuilder := TSkPathBuilder.Create;
			PathBuilder.MoveTo(FHexagons[I].Points[0].X, FHexagons[I].Points[0].Y);
			for var J := 1 to 5 do
				PathBuilder.LineTo(FHexagons[I].Points[J].X, FHexagons[I].Points[J].Y);
			PathBuilder.Close;
			FHexagons[i].Path := PathBuilder.Detach;
		end;

		if FHexagons[i].Path.Contains(X, Y) then
      Exit(I);
  end;
end;

function THexagonGrid.GetFlagCount: Integer;
begin
  Result := 0;
  for var i := 0 to High(FHexagons) do
    if FHexagons[i].IsFlagged then
      Inc(Result);
end;

function THexagonGrid.GetNeighbors(const AIndex: Integer): TArray<Integer>;
var
  Col, Row: Integer;
  I, NewCol, NewRow: Integer;
  DirX, DirY: array[0..5] of Integer;
begin
  Col := FHexagons[AIndex].Column;
  Row := FHexagons[AIndex].Row;
  SetLength(Result, 0);

  // For even columns
  if Col mod 2 = 0 then
  begin
    DirX[0] := 0;  DirX[1] := 1;  DirX[2] := 1;
    DirX[3] := 0;  DirX[4] := -1; DirX[5] := -1;
    DirY[0] := -1; DirY[1] := -1; DirY[2] := 0;
    DirY[3] := 1;  DirY[4] := 0;  DirY[5] := -1;
  end
  // For odd columns
  else
  begin
    DirX[0] := 0;  DirX[1] := 1;  DirX[2] := 1;
    DirX[3] := 0;  DirX[4] := -1; DirX[5] := -1;
    DirY[0] := -1; DirY[1] := 0;  DirY[2] := 1;
    DirY[3] := 1;  DirY[4] := 1;  DirY[5] := 0;
  end;

  for I := 0 to 5 do
  begin
    NewCol := Col + DirX[I];
    NewRow := Row + DirY[I];

    if (NewCol >= 0) and (NewCol < FGridSize) and
      (NewRow >= 0) and (NewRow < FGridSize) and
      not ((NewCol mod 2 = 1) and (NewRow >= FGridSize - 1)) then
    begin
      // Find the index in the FHexagons array that matches these coordinates
      for var J := 0 to High(FHexagons) do
        if (FHexagons[J].Column = NewCol) and (FHexagons[J].Row = NewRow) then
        begin
          Result := Result + [J];
          Break;
        end;
    end;
  end;
end;

procedure THexagonGrid.PlaceMines(const AFirstClickIndex: Integer);
var
  AvailableCells: TArray<Integer>;
  ExcludedIndices: TArray<Integer>;
  I, RandIndex, CellIndex: Integer;
  Neighbors: TArray<Integer>;
begin
  // First, be sure the clicked cell and its neighbors aren't mines
  FHexagons[AFirstClickIndex].HasMine := False;
  Neighbors := GetNeighbors(AFirstClickIndex);
  for I := 0 to High(Neighbors) do
    FHexagons[Neighbors[I]].HasMine := False;

  // Create list of cells to exclude
  ExcludedIndices := Neighbors + [AFirstClickIndex];
  TArray.Sort<Integer>(ExcludedIndices);

  // Initialize available cells excluding protected ones
  SetLength(AvailableCells, Length(FHexagons) - Length(ExcludedIndices));
  CellIndex := 0;
  for I := 0 to High(FHexagons) do
    if not TArray.BinarySearch<Integer>(ExcludedIndices, I, RandIndex) then
    begin
      AvailableCells[CellIndex] := I;
      Inc(CellIndex);
    end;

  // Place mines randomly in remaining cells
  for I := 1 to FMineCount do
  begin
    if Length(AvailableCells) = 1 then
      Break; // Safety check in case we run out of available cells

    RandIndex := Random(Length(AvailableCells));
    CellIndex := AvailableCells[RandIndex];
    FHexagons[CellIndex].HasMine := True;
    Delete(AvailableCells, RandIndex, 1);
  end;

  CalculateNearbyMines;
end;

procedure THexagonGrid.CalculateNearbyMines;
var
  I: Integer;
  Neighbors: TArray<Integer>;
  J: Integer;
begin
  for I := 0 to High(FHexagons) do
  begin
    if not FHexagons[I].HasMine then
    begin
      Neighbors := GetNeighbors(I);
      FHexagons[I].NearbyMines := 0;
      for J := 0 to High(Neighbors) do
        if FHexagons[Neighbors[J]].HasMine then
          Inc(FHexagons[I].NearbyMines);
    end;
  end;
end;

function THexagonGrid.RevealCell(const AIndex: Integer): Boolean;
var
  Neighbors: TArray<Integer>;
  I: Integer;
begin
  if FFirstClick then
  begin
    FFirstClick := False;
    PlaceMines(AIndex);
  end;

  if not FHexagons[AIndex].IsFlagged then
  begin
    Result := FHexagons[AIndex].HasMine;
    FHexagons[AIndex].IsRevealed := True;

    // Auto-reveal neighbors if no nearby mines
    if (not Result) and (FHexagons[AIndex].NearbyMines = 0) then
    begin
      Neighbors := GetNeighbors(AIndex);
      for I := 0 to High(Neighbors) do
        if not FHexagons[Neighbors[I]].IsRevealed then
          RevealCell(Neighbors[I]);
    end;
  end
  else
    Result := False;
end;

procedure THexagonGrid.FlagCell(const AIndex: Integer);
begin
  if not FHexagons[AIndex].IsRevealed then
    FHexagons[AIndex].IsFlagged := not FHexagons[AIndex].IsFlagged;
end;

function THexagonGrid.CheckWin: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(FHexagons) do
    if not FHexagons[I].HasMine then
      Result := Result and FHexagons[I].IsRevealed;
end;

end.

