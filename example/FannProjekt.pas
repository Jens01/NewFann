unit FannProjekt;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  fann.DelphiApi, fann.DelphiTrainApi;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    mmoEvent: TMemo;
    btnTrain: TButton;
    btnExec: TButton;
    edtError: TEdit;
    procedure btnTrainClick(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Fann: TFannclass;
    procedure TrainEvent(epochs: Integer; MSE: Single; var IsTrainBreak: Boolean);
    procedure ClearMemo;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExecClick(Sender: TObject);
var
  _input, _output: TArray<Single>;
  i, j: integer;
begin
  if Assigned(Fann) then
  begin
    for i   := 0 to 1 do
      for j := 0 to 1 do
      begin
        _input  := [i, j];
        _output := Fann.Run(_input);
        mmo1.Lines.Add(Format('%f Xor %f = %f', [_input[0], _input[1], _output[0]]));
      end;
  end;
end;

procedure TForm1.btnTrainClick(Sender: TObject);
const
  fn = 'C:\si-delphi\kleine Programme\FANN\datasets\xor.data';
var
  Train: TTrainclass;
begin
  ClearMemo;
  Train := TTrainclass.Create(Fann);
  try
    Train.FannEvent     := TrainEvent;
    Train.desired_error := string(edtError.Text).ToSingle;
    Train.TrainFromFannFile(fn);
  finally
    Train.Free;
  end;
end;

procedure TForm1.ClearMemo;
begin
  mmo1.Clear;
  mmoEvent.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fann          := TFannclass.Create([2, 3, 1]);
  edtError.Text := '0,001';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Fann.Free;
end;

procedure TForm1.TrainEvent(epochs: Integer; MSE: Single; var IsTrainBreak: Boolean);
begin
  mmoEvent.Lines.Add(Format('No.:%5d MSE : %.6f', [epochs, MSE]));
end;

end.
