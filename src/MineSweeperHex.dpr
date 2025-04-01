program MineSweeperHex;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MineSweeperBoard in 'MineSweeperBoard.pas' {Form31},
  HexagonGrid in 'HexagonGrid.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm31, Form31);
  Application.Run;
end.
