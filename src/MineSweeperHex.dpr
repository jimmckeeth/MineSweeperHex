program MineSweeperHex;

uses
  System.StartUpCopy,
  FMX.Forms,
  MineSweeperBoard in 'MineSweeperBoard.pas' {Form31},
  HexagonGrid in 'HexagonGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm31, Form31);
  Application.Run;
end.
