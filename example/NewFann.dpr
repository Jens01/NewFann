program NewFann;

uses
  Vcl.Forms,
  FannProjekt in 'FannProjekt.pas' {Form1},
  fann.DelphiApi in '..\source\fann.DelphiApi.pas',
  fann.DelphiTrainApi in '..\source\fann.DelphiTrainApi.pas',
  fann.Api in '..\source\fann.Api.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
