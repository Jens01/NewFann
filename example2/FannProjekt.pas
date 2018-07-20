unit FannProjekt;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, System.Math,
  fann.DelphiApi, fann.DelphiTrainApi, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    mmoEvent: TMemo;
    btnTrain: TButton;
    btnExec: TButton;
    edtError: TEdit;
    grpXor: TGroupBox;
    Cascade: TGroupBox;
    btnCascade: TButton;
    btnCTrain: TButton;
    btnCExec: TButton;
    edtCError: TEdit;
    btnBreak: TButton;
    btnTest: TButton;
    lvNeuron: TListView;
    procedure btnTrainClick(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCascadeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCExecClick(Sender: TObject);
    procedure btnCTrainClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    FannStream: TMemoryStream;
    procedure TrainEvent(epochs: Integer; MSE: Single);
    procedure TrainBreakEvent(epochs: Integer; MSE: Single; var IsTrainBreak: Boolean);
    procedure TrainCasEvent(epochs, total_epochs: Integer; MSE: Single);
    procedure FillNeuronsToList;
    procedure ClearMemo;
  public
  end;

var
  Form1: TForm1;

implementation

const
  FNXorTrain = '..\..\xor.data';
  FNParityTrain = '..\..\parity8.train';
  FNParityTest = '..\..\parity8.test';

{$R *.dfm}

function InRange(Value, _min, _max: Single): Boolean;
begin
  Result := (Value >= _min) and (Value <= _max)
end;

procedure TForm1.btnCascadeClick(Sender: TObject);
var
  Fann: TFannclass;
  CTrain: TCascadeclass;
  Steepness: TArray<Single>;
  AcFunctions: TArray<Integer>;
  i: Integer;
begin
  ClearMemo;
  Fann := TFannclass.Create([8, 1], FANN_NETTYPE_SHORTCUT);
  try
    CTrain := TCascadeclass.Create(Fann);
    try
      CTrain.epochs_between_reports := 50;
      Fann.DefNeutronLayer(1, 0.75, FANN_SIGMOID_SYMMETRIC_STEPWISE);

      CTrain.desired_error          := string(edtCError.Text).ToSingle;
      CTrain.FannCascadeEvent       := TrainCasEvent;
      CTrain.training_algorithm     := FANN_TRAIN_RPROP;
      CTrain.train_error_function   := FANN_ERRORFUNC_LINEAR;
      CTrain.bit_fail_limit         := 0.5;
      CTrain.train_stop_function    := FANN_STOPFUNC_MSE;
      AcFunctions                   := [FANN_SIGMOID_STEPWISE];
      Steepness                     := [0.5, 1];
      CTrain.activation_functions   := AcFunctions;
      CTrain.activation_steepnesses := Steepness;

      if (CTrain.training_algorithm <> FANN_TRAIN_QUICKPROP) then
      begin
        CTrain.learning_rate    := 0.7;
        CTrain.WeigthsMax       := 5;
        CTrain.WeigthsMin       := -5;
        CTrain.UseRandomWeigths := False;
      end;
      CTrain.TrainCascadeFromFannFile(FNParityTrain);
    finally
      CTrain.Free;
    end;

    for i := 0 to Fann.LayerCount - 1 do
      mmoEvent.Lines.Add(Format('LayerIndx %d : Neurons : %d (%d)', [i, Fann.NeuronCount[i], Fann.NeuronandBiasCount[i]]));

    FannStream.Clear;
    Fann.SaveToStream(FannStream);
    FillNeuronsToList;
  finally
    Fann.Free;
  end;
end;

procedure TForm1.btnCExecClick(Sender: TObject);
var
  Fann: TFannclass;
  data: TTraindataclass;
  i, ii, c: Integer;
  O: TArray<Single>;
  IsOk: Boolean;
  InputStr: TStringBuilder;
begin
  FannStream.Position := 0;
  Fann                := TFannclass.Create(FannStream);
  data                := TTraindataclass.Create;
  try
    data.LoadFromFannFile(FNParityTrain);
    c     := 0;
    for i := 0 to data.NumData - 1 do
    begin
      O    := Fann.Run(data.Inputset[i]);
      IsOk := InRange(O[0], -0.15, 0.15) and SameValue(data.outputset[i][0], 0) or InRange(O[0], 0.85, 1.15) and
        SameValue(data.outputset[i][0], 1);
      if not IsOk then
        Inc(c);
      InputStr := TStringBuilder.Create;
      for ii   := 0 to 7 do
        InputStr.Append(data.Inputset[i][ii]).Append(' ');
      mmo1.Lines.Add(Format('%s -> %.3f (%.3f) | %s', [InputStr.ToString, O[0], data.Inputset[i][0], IsOk.ToString]));
      InputStr.Free;
    end;
    mmo1.Lines.Add('Fehler: ' + c.ToString);
  finally
    data.Free;
    Fann.Free;
  end;
end;

procedure TForm1.btnCTrainClick(Sender: TObject);
var
  Fann: TFannclass;
  Train: TTrainclass;
begin
  ClearMemo;
  FannStream.Position := 0;
  Fann                := TFannclass.Create(FannStream);
  try
    Train := TTrainclass.Create(Fann);
    try
      Train.epochs_between_reports := 1;
      Train.FannEvent              := TrainEvent;
//       Train.FannBreakEvent := TrainBreakEvent;
      Train.desired_error := string(edtCError.Text).ToSingle;
      Train.TrainFromFannFile(FNParityTrain);
    finally
      Train.Free;
    end;
    FannStream.Clear;
    Fann.SaveToStream(FannStream);
  finally
    Fann.Free;
  end;
end;

procedure TForm1.btnExecClick(Sender: TObject);
var
  Fann: TFannclass;
  _input, _output: TArray<Single>;
  i, j: integer;
begin
  FannStream.Position := 0;
  Fann                := TFannclass.Create(FannStream);
  try
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
  finally
    Fann.Free;
  end;
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  Fann: TFannclass;
  Train: TTrainclass;
begin
  ClearMemo;
  FannStream.Position := 0;
  Fann                := TFannclass.Create(FannStream);
  try
    Train := TTrainclass.Create(Fann);
    try
      Train.epochs_between_reports := 1;
      Train.FannEvent              := TrainEvent;
      // Train.FannBreakEvent := TrainBreakEvent;
      Train.desired_error := string(edtCError.Text).ToSingle;
      Train.TestFromFannFile(FNParityTrain);
    finally
      Train.Free;
    end;
    FannStream.Clear;
    Fann.SaveToStream(FannStream);
  finally
    Fann.Free;
  end;
end;

procedure TForm1.btnTrainClick(Sender: TObject);
var
  Fann: TFannclass;
  Train: TTrainclass;
begin
  ClearMemo;
  Fann := TFannclass.Create([2, 3, 1]);
  try
    Train := TTrainclass.Create(Fann);
    try
      Train.epochs_between_reports := 1;
      Train.FannEvent              := TrainEvent;
      Train.desired_error          := string(edtError.Text).ToSingle;
      Train.TrainFromFannFile(FNXorTrain);
    finally
      Train.Free;
    end;
    FannStream.Clear;
    Fann.SaveToStream(FannStream);
  finally
    Fann.Free;
  end;
end;

procedure TForm1.ClearMemo;
begin
  mmo1.Clear;
  mmoEvent.Clear;
end;

procedure TForm1.FillNeuronsToList;
var
  Fann: TFannclass;
  iLayer, iNeuron, f: Integer;
  Neuron: TNeuron;
begin
  lvNeuron.Clear;
  FannStream.Position := 0;
  Fann                := TFannclass.Create(FannStream);
  try
    for iLayer    := 0 to Fann.LayerCount - 1 do
      for iNeuron := 0 to Fann.NeuronCount[iLayer] - 1 do
      begin
        Neuron := Fann.Neuron[iLayer, iNeuron];
        with lvNeuron.Items.Add do
        begin
          Caption := 'Neuron';
          SubItems.Add(iLayer.ToString);
          SubItems.Add(iNeuron.ToString);
          f := Neuron.activation_function;
          if f > -1 then
            SubItems.Add(FANN_ACTIVATIONFUNC_NAMES[f])
          else
            SubItems.Add('-');
          SubItems.Add(Neuron.activation_steepness.ToString);
          SubItems.Add(Neuron.IsBias.ToString);
        end;
      end;
  finally
    Fann.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edtError.Text  := '0,001';
  edtCError.Text := '0,001';
  FannStream     := TMemoryStream.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FannStream.Free;
end;

procedure TForm1.TrainBreakEvent(epochs: Integer; MSE: Single; var IsTrainBreak: Boolean);
begin
end;

procedure TForm1.TrainCasEvent(epochs, total_epochs: Integer; MSE: Single);
begin
  mmoEvent.Lines.Add(Format('No.:%5d MSE : %.6f', [epochs, MSE]));
end;

procedure TForm1.TrainEvent(epochs: Integer; MSE: Single);
begin
  mmoEvent.Lines.Add(Format('No.:%5d MSE : %.6f', [epochs, MSE]));
end;

end.
