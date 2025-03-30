unit MineSweeperBoard;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Math,
  System.Skia, FMX.Skia, System.UIConsts, HexagonGrid,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm31 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    lblStatus: TLabel;
    SkAnimatedPaintBox1: TSkAnimatedPaintBox;
    aniExplosion: TSkAnimatedImage;
    aniFlag: TSkAnimatedImage;
    svgBomb: TSkSvg;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SkPaintBox1Resize(Sender: TObject);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure CalculateCellSize;
    procedure SkPaintBox1DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF;
      const AProgress: Double; const AOpacity: Single);

  private
    const
      GRID_SIZE = 11;
    procedure RunShader;
    var
    FHexGrid: THexagonGrid;
    FHotHexIndex: Integer;
    FGameOver: Boolean;
    FPressing: Boolean;
    FShaderBuilder: ISkRuntimeShaderBuilder;
    FPaint: ISkPaint;
    procedure DrawHexagon(const ACanvas: ISkCanvas; const AHexInfo: THexagonInfo);
    procedure RevealAllMines;
    procedure RestartGame;
  public
    { Public declarations }
  end;

var
  Form31: TForm31;

implementation

{$R *.fmx}

uses
  IOUtils;

procedure TForm31.FormCreate(Sender: TObject);
begin
  SkAnimatedPaintBox1.Animation.Duration := MaxSingle;
  RunShader;

  RestartGame;
  CalculateCellSize;
end;

procedure TForm31.RunShader;
var
  LEffect: ISkRuntimeEffect;
begin
  SkAnimatedPaintBox1.Animation.StopAtCurrent;
  FShaderBuilder := nil;
  FPaint := nil;
  var AErrorText := '';
  LEffect := TSkRuntimeEffect.MakeForShader(
    TFile.ReadAllText('..\..\background.sksl'), AErrorText);
  if AErrorText <> '' then
    raise Exception.Create(AErrorText);

  FShaderBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  FPaint := TSkPaint.Create;
  FPaint.Shader := FShaderBuilder.MakeShader;
  SkAnimatedPaintBox1.Animation.Start;
end;

procedure TForm31.FormDestroy(Sender: TObject);
begin
  FHexGrid.Free;
end;

procedure TForm31.FormShow(Sender: TObject);
begin
  SkPaintBox1.Redraw;
end;

procedure TForm31.DrawHexagon(const ACanvas: ISkCanvas; const AHexInfo: THexagonInfo);
var
  PathBuilder: ISkPathBuilder;
  Path: ISkPath;
  Paint: ISkPaint;
  TextPaint: ISkPaint;
  Text: string;
  Font: ISkFont;
begin
  PathBuilder := TSkPathBuilder.Create;
  PathBuilder.MoveTo(AHexInfo.Points[0].X, AHexInfo.Points[0].Y);
  for var I := 1 to 5 do
    PathBuilder.LineTo(AHexInfo.Points[I].X, AHexInfo.Points[I].Y);
  PathBuilder.Close;
  Path := PathBuilder.Detach;

  Paint := TSkPaint.Create;
  if AHexInfo.IsRevealed then
  begin
    if AHexInfo.HasMine then
      Paint.Color := TAlphaColors.Red
    else
      Paint.Color := TAlphaColors.Lightgray
  end
  else if (FHotHexIndex >= 0) and (AHexInfo.Column = FHexGrid.Hexagons[FHotHexIndex].Column) and
          (AHexInfo.Row = FHexGrid.Hexagons[FHotHexIndex].Row) then
  begin
    if FPressing then
      Paint.Color := TAlphaColors.Blueviolet
    else
      Paint.Color := TAlphaColors.Yellow
  end
  else
    Paint.Color := TAlphaColors.Null;
  Paint.Alpha := 128;

  Paint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawPath(Path, Paint);

  Paint := TSkPaint.Create;
  Paint.Color := TAlphaColors.White;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  ACanvas.DrawPath(Path, Paint);

  if AHexInfo.IsFlagged then
  begin
    Text := 'F';
    aniFlag.Visible := True;
		aniFlag.Height := AHexInfo.Size;
		aniFlag.Width := AHexInfo.Size;

		aniFlag.Position.X := AHexInfo.Center.X - AHexInfo.Size / 2;
		aniFlag.Position.Y := AHexInfo.Center.Y + AHexInfo.Size / 2;
	end
	else if AHexInfo.IsRevealed then
	begin
		if AHexInfo.HasMine then
		begin
			Text := 'M';
			svgBomb.Visible := True;
			svgBomb.Height := AHexInfo.Size;
			svgBomb.Width := AHexInfo.Size;
			svgBomb.Position.X := AHexInfo.Center.X - AHexInfo.Size / 2;
			svgBomb.Position.Y := AHexInfo.Center.Y + AHexInfo.Size / 2;
		end
		else if AHexInfo.NearbyMines > 0 then
      Text := AHexInfo.NearbyMines.ToString
    else
      Text := '';
  end
  else
    Text := '';

  if Text <> '' then
  begin
    TextPaint := TSkPaint.Create;
    TextPaint.Color := TAlphaColors.Orange;
    Font := TSkFont.Create(nil, 14);
    var TextWidth := Font.MeasureText(Text, TextPaint);
    var TextBounds: TRectF;
    Font.MeasureText(Text, TextBounds, TextPaint);
    ACanvas.DrawSimpleText(Text,
      AHexInfo.Center.X - TextWidth/2,
      AHexInfo.Center.Y + TextBounds.Height/2,
      Font, TextPaint);
  end;
end;

procedure TForm31.SkAnimatedPaintBox1AnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
begin
  if Assigned(FShaderBuilder) and Assigned(FPaint) then
  begin
    if FShaderBuilder.Effect.UniformExists('iResolution') then
    begin
      if FShaderBuilder.Effect.UniformTypeByName['iResolution'] in [TSkRuntimeEffectUniformType.Float3,
          TSkRuntimeEffectUniformType.Int3] then
        FShaderBuilder.SetUniform('iResolution', [ADest.Width, ADest.Height, 0])
      else
        FShaderBuilder.SetUniform('iResolution', [ADest.Width, ADest.Height]);
    end;
    if FShaderBuilder.Effect.UniformExists('iTime') then
      FShaderBuilder.SetUniform('iTime', AProgress * SkAnimatedPaintBox1.Animation.Duration);
    FPaint.Shader := FShaderBuilder.MakeShader;
    ACanvas.DrawRect(ADest, FPaint);
  end;
end;

procedure TForm31.SkPaintBox1DblClick(Sender: TObject);
begin
  if FGameOver then
    RestartGame;
end;

procedure TForm31.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  I: Integer;
begin
  ACanvas.Clear(TAlphaColors.Null);
  for I := 0 to Length(FHexGrid.Hexagons) - 1 do
		DrawHexagon(ACanvas, FHexGrid.Hexagons[I]);
  SkAnimatedPaintBox1.Redraw;
end;

procedure TForm31.SkPaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not FPressing then
  begin
    FPressing := True;
    SkPaintBox1.Redraw;
  end;
end;

procedure TForm31.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  NewIndex: Integer;
begin
  NewIndex := FHexGrid.FindHexagonAt(X, Y);
  if NewIndex <> FHotHexIndex then
  begin
    FHotHexIndex := NewIndex;
    FPressing := False;
    SkPaintBox1.Redraw;
  end;
end;

procedure TForm31.SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not FPressing then exit;
  FPressing := False;
  if (FHotHexIndex >= 0) and not FGameOver then
  begin
    case Button of
      TMouseButton.mbLeft:
        begin
          if FHexGrid.RevealCell(FHotHexIndex) then
          begin
            RevealAllMines;
            FGameOver := True;
            SkPaintBox1.Redraw;
          end
          else if FHexGrid.CheckWin then
          begin
            RevealAllMines;
            FGameOver := True;
            SkPaintBox1.Redraw;
          end;
        end;
      TMouseButton.mbRight:
        FHexGrid.FlagCell(FHotHexIndex);
    end;
    SkPaintBox1.Redraw;
  end;
  lblStatus.Text := Format('Hex Mine: %d found out of %d',[FHexGrid.FlagCount, FHexGrid.MineCount]);
end;

procedure TForm31.SkPaintBox1Resize(Sender: TObject);
begin
  CalculateCellSize;
  SkPaintBox1.Redraw;
end;

procedure TForm31.CalculateCellSize;
begin
  if Assigned(FHexGrid) then
    FHexGrid.UpdateSize(SkPaintBox1.Width, SkPaintBox1.Height);
end;

procedure TForm31.RestartGame;
begin
  FHexGrid.Free;
  FHotHexIndex := -1;
  FGameOver := False;
  FPressing := False;
  FHexGrid := THexagonGrid.Create(GRID_SIZE);
  FHexGrid.MineCount := Grid_size * Grid_Size div 6;
  CalculateCellSize;
  SkPaintBox1.Redraw;
end;

procedure TForm31.RevealAllMines;
var
  I: Integer;
begin
  for I := 0 to High(FHexGrid.Hexagons) do
    if FHexGrid.Hexagons[I].HasMine then
      FHexGrid.Hexagons[I].IsRevealed := True;
end;

end.