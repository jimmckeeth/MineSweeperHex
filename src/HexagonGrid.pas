unit HexagonGrid;

interface

uses
  System.Types, System.Math, System.UITypes;

type
  THexagonInfo = record
    Center: TPointF;
    Points: array[0..5] of TPointF;
    Column, Row: Integer;
    HasMine: Boolean;
    IsRevealed: Boolean;
    IsFlagged: Boolean;
    NearbyMines: Integer;
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
  public
    constructor Create(const AGridSize: Integer);
    procedure UpdateSize(const AWidth, AHeight: Single);
    function FindHexagonAt(const X, Y: Single): Integer;
    function RevealCell(const AIndex: Integer): Boolean; // Returns True if mine hit
    procedure FlagCell(const AIndex: Integer);
    function CheckWin: Boolean;
    property CellSize: Single read FCellSize;
    property XOffset: Single read FXOffset;
    property YOffset: Single read FYOffset;
    property Hexagons: THexagonInfoArray read FHexagons;
    property MineCount: Integer read FMineCount;
  end;

implementation

{ THexagonInfo }

uses 
  System.Skia;

procedure THexagonInfo.CalculatePoints(const ACellSize: Single);
var
  I: Integer;
  Angle: Single;
begin
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
  FMineCount := (AGridSize * AGridSize) div 6; // About 17% mines
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
      if (X mod 2 = 1) and (Y >= FGridSize-1) then
        Continue;
        
      FHexagons[Index].Column := X;
      FHexagons[Index].Row := Y;
      FHexagons[Index].Center.X := X * FCellSize * 1.5 + FXOffset;
      FHexagons[Index].Center.Y := Y * FCellSize * Sqrt(3) + FYOffset + FCellSize * Sqrt(3)/2;
      if X mod 2 = 1 then
        FHexagons[Index].Center.Y := FHexagons[Index].Center.Y + FCellSize * Sqrt(3)/2;
        
      FHexagons[Index].CalculatePoints(FCellSize);
      Inc(Index);
    end;
  SetLength(FHexagons, Index);
end;

procedure THexagonGrid.UpdateSize(const AWidth, AHeight: Single);
const 
  PADDING = 0;
var
  HexWidth, HexHeight: Single;
begin
  HexWidth := (AWidth - 2 * PADDING) / (FGridSize * 1.5 + 0.5);
  HexHeight := (AHeight - 2 * PADDING) / ((FGridSize * 2) * 0.866);
  FCellSize := Min(HexWidth, HexHeight);
  
  FXOffset := (AWidth - (FGridSize * FCellSize * 1.5)) / 2 + FCellSize/4*3;
  FYOffset := (AHeight - (FGridSize * FCellSize * Sqrt(3))) / 2;
  
  CalculateHexagons;
end;

function THexagonGrid.FindHexagonAt(const X, Y: Single): Integer;
var
  I: Integer;
  Path: ISkPath;
  PathBuilder: ISkPathBuilder;
begin
  Result := -1;
  for I := 0 to Length(FHexagons) - 1 do
  begin
    PathBuilder := TSkPathBuilder.Create;
    PathBuilder.MoveTo(FHexagons[I].Points[0].X, FHexagons[I].Points[0].Y);
    for var J := 1 to 5 do
      PathBuilder.LineTo(FHexagons[I].Points[J].X, FHexagons[I].Points[J].Y);
    PathBuilder.Close;
    Path := PathBuilder.Detach;
    
    if Path.Contains(X, Y) then
      Exit(I);
  end;
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
       not ((NewCol mod 2 = 1) and (NewRow >= FGridSize-1)) then
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
  I, RandIndex, CellIndex: Integer;
  Neighbors: TArray<Integer>;
begin
  SetLength(AvailableCells, Length(FHexagons));
  for I := 0 to High(FHexagons) do
    AvailableCells[I] := I;
    
  // Remove first click and its neighbors from available cells
  Neighbors := GetNeighbors(AFirstClickIndex);
  for I := High(Neighbors) downto 0 do
    Delete(AvailableCells, Neighbors[I], 1);
  Delete(AvailableCells, AFirstClickIndex, 1);
  
  // Place mines randomly
  for I := 1 to FMineCount do
  begin
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

  Result := FHexagons[AIndex].HasMine;
  if not FHexagons[AIndex].IsFlagged then
  begin
    FHexagons[AIndex].IsRevealed := True;
    
    // Auto-reveal neighbors if no nearby mines
    if (not Result) and (FHexagons[AIndex].NearbyMines = 0) then
    begin
      Neighbors := GetNeighbors(AIndex);
      for I := 0 to High(Neighbors) do
        if not FHexagons[Neighbors[I]].IsRevealed then
          RevealCell(Neighbors[I]);
    end;
  end;
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
