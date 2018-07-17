// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

unit fann.DelphiTrainApi;

interface

uses
  System.SysUtils, System.Math, System.Classes, System.Generics.Collections, fann.Api, fann.DelphiApi;

type

  TFannEvent = procedure(epochs: Integer; MSE: Single; var IsTrainBreak: Boolean) of object;
  TFannCascadeEvent = procedure(epochs, total_epochs: Integer; MSE: Single; var IsTrainBreak: Boolean) of object;

  TTraindataclass = class
  strict private
    FIsChange: Boolean;
    FTrainData: pfann_train_data;
    FNum_InputNeurons: Integer;
    FNum_OutputNeurons: Integer;
    FInputs: TList<Single>;
    FOutputs: TList<Single>;
    function NumDataInput: Integer;
    function NumDataOutput: Integer;
    procedure ArrayToFannArray(FannArray: ppfann_type_array; Aarray: TArray<Single>);
    function GetInputs: TArray<Single>;
    function GetOutputs: TArray<Single>;
    function GetNumData: Integer;
    function GetTrainData: pfann_train_data;
    procedure SetNum_InputNeurons(const Value: Integer);
    procedure SetNum_OutputNeurons(const Value: Integer);
  public
    constructor Create(Num_InputNeurons, Num_OutputNeurons: Integer); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure ClearData;
    function IsDataValid: Boolean;
    procedure LoadFromFannFile(Filename: string);
    procedure AddInput(Input: TArray<Single>);
    procedure AddOutput(Output: TArray<Single>);
    property NumData: Integer read GetNumData;
    property NumInput: Integer read FNum_InputNeurons write SetNum_InputNeurons;
    property NumOutput: Integer read FNum_OutputNeurons write SetNum_OutputNeurons;
    property TrainData: pfann_train_data read GetTrainData;
    property Inputs: TArray<Single> read GetInputs;
    property Outputs: TArray<Single> read GetOutputs;
  end;

  TTrainclass = class
  strict private
    Fann: TFannclass;
    Fepochs_max: Integer;
    Fepochs_between_reports: Integer;
    Fdesired_error: Single;
    FFannEvent: TFannEvent;
    procedure train_on_data(traindata: TTraindataclass);
    procedure test_on_data(traindata: TTraindataclass);
  strict private // Cascade
    Fneurons_between_reports: Integer;
    Fmax_neurons: Integer;
    FFannCascadeEvent: TFannCascadeEvent;
    procedure train_cascade_on_data(traindata: TTraindataclass);
  public
    constructor Create(ann: TFannclass);
    procedure Train(traindata: TTraindataclass);
    procedure TrainFromFannFile(Filename: string);
    procedure Test(traindata: TTraindataclass);
    property desired_error: Single read Fdesired_error write Fdesired_error;
    property epochs_between_reports: Integer read Fepochs_between_reports write Fepochs_between_reports;
    property epochs_max: Integer read Fepochs_max write Fepochs_max;
    property FannEvent: TFannEvent read FFannEvent write FFannEvent;
  public // Cascade
    procedure TrainCascade(traindata: TTraindataclass);
    property neurons_between_reports: Integer read Fneurons_between_reports write Fneurons_between_reports;
    property max_neurons: Integer read Fmax_neurons write Fmax_neurons;
    property FannCascadeEvent: TFannCascadeEvent read FFannCascadeEvent write FFannCascadeEvent;
  end;

implementation

function StringToArray(s: TArray<string>): TArray<Single>;
var
  i: Integer;
begin
  SetLength(Result, Length(s));
  for i       := 0 to Length(s) - 1 do
    Result[i] := s[i].ToSingle;
end;

{ TTrainclass }

constructor TTraindataclass.Create(Num_InputNeurons, Num_OutputNeurons: Integer);
begin
  FTrainData         := nil;
  FInputs            := TList<Single>.Create;
  FOutputs           := TList<Single>.Create;
  FNum_InputNeurons  := Num_InputNeurons;
  FNum_OutputNeurons := Num_OutputNeurons;
  FIsChange          := False;
end;

constructor TTraindataclass.Create;
begin
  Create(0, 0);
end;

procedure TTraindataclass.ClearData;
begin
  FInputs.Clear;
  FOutputs.Clear;
  FIsChange := True;
end;

destructor TTraindataclass.Destroy;
begin
  FInputs.Free;
  FOutputs.Free;
  fann_destroy_train(FTrainData);
  inherited;
end;

function TTraindataclass.GetInputs: TArray<Single>;
begin
  Result := FInputs.ToArray;
end;

function TTraindataclass.GetNumData: Integer;
begin
  Result := IfThen(IsDataValid, NumDataInput, -1);
end;

function TTraindataclass.GetOutputs: TArray<Single>;
begin
  Result := FOutputs.ToArray;
end;

function TTraindataclass.GetTrainData: pfann_train_data;
begin
  if IsDataValid then
  begin
    if FIsChange then
    begin
      fann_destroy_train(FTrainData);
      FTrainData := fann_create_train(NumDataInput, FNum_InputNeurons, FNum_OutputNeurons);
      ArrayToFannArray(FTrainData.input, FInputs.ToArray);
      ArrayToFannArray(FTrainData.output, FOutputs.ToArray);
    end;
    Result := FTrainData;
  end
  else
    raise Exception.Create('Error: NumDataInput <> NumDataOutput');
end;

function TTraindataclass.IsDataValid: Boolean;
begin
  Result := (NumDataInput = NumDataOutput);
end;

procedure TTraindataclass.LoadFromFannFile(Filename: string);
var
  sl: TStringList;
  ss: TArray<string>;
  dd: TArray<Single>;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    if (sl.Count > 2) and Odd(sl.Count) then
    begin
      ss := sl[0].Split([' ']);
      if Length(ss) = 3 then
      begin
        FNum_InputNeurons  := ss[1].ToInteger;
        FNum_OutputNeurons := ss[2].ToInteger;
      end
      else
        Exit;

      for i := 1 to sl.Count - 1 do
      begin
        ss := sl[i].Split([' ']);
        dd := StringToArray(ss);
        if Odd(i) then
        begin
          if Length(dd) = FNum_InputNeurons then
            AddInput(dd)
          else
            raise Exception.Create('Wrong File');
        end
        else
        begin
          if Length(dd) = FNum_OutputNeurons then
            AddOutput(dd)
          else
            raise Exception.Create('Wrong File');
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

function TTraindataclass.NumDataInput: Integer;
begin
  Result := FInputs.Count div FNum_InputNeurons;
end;

function TTraindataclass.NumDataOutput: Integer;
begin
  Result := FOutputs.Count div FNum_OutputNeurons;
end;

procedure TTraindataclass.SetNum_InputNeurons(const Value: Integer);
begin
  if FNum_InputNeurons <> Value then
  begin
    FNum_InputNeurons := Value;
    FIsChange         := True;
  end;
end;

procedure TTraindataclass.SetNum_OutputNeurons(const Value: Integer);
begin
  if FNum_OutputNeurons <> Value then
  begin
    FNum_OutputNeurons := Value;
    FIsChange          := True;
  end;
end;

procedure TTraindataclass.AddInput(Input: TArray<Single>);
begin
  if Length(Input) = FNum_InputNeurons then
  begin
    FIsChange := True;
    FInputs.AddRange(Input);
  end
  else
    raise Exception.Create('Error: Wrong Input');
end;

procedure TTraindataclass.AddOutput(Output: TArray<Single>);
begin
  if Length(Output) = FNum_OutputNeurons then
  begin
    FIsChange := True;
    FOutputs.AddRange(Output);
  end
  else
    raise Exception.Create('Error: Wrong Output');
end;

procedure TTraindataclass.ArrayToFannArray(FannArray: ppfann_type_array; Aarray: TArray<Single>);
var
  i: Integer;
begin
  for i           := 0 to Length(Aarray) - 1 do
    FannArray^[i] := Aarray[i];
end;

{ TTrainclass }

constructor TTrainclass.Create(ann: TFannclass);
begin
  inherited Create;
  Fann                    := ann;
  Fepochs_between_reports := 1000;
  Fepochs_max             := 500000;
  Fdesired_error          := 0.0001;
end;

procedure TTrainclass.train_cascade_on_data(traindata: TTraindataclass);
// aus fann_cascade.c
// fann_cascadetrain_on_data
var
  total_epochs: Integer;
  i: Integer;
  error: Single;
  _is_desired_error_reached: Boolean;
  IsTrainingBreak: Boolean;
  IsBreak: Boolean;
begin
  total_epochs    := 0;
  IsTrainingBreak := False;
  i               := 0;
  repeat
    total_epochs              := total_epochs + fann_train_outputs(Fann.ann, traindata.TrainData, Fdesired_error);
    error                     := fann.MSE;
    _is_desired_error_reached := Fann.is_error_reached(Fdesired_error);

    if (Fneurons_between_reports > 0) and (((i + 1) mod Fneurons_between_reports = 0) or (i + 1 = Fmax_neurons) or (i = 0) or
      _is_desired_error_reached) then
    begin
      if Assigned(FFannEvent) then
        FFannCascadeEvent(i, total_epochs, error, IsTrainingBreak);
    end;

    IsBreak := _is_desired_error_reached or IsTrainingBreak;
    if not IsBreak then
    begin
      if fann_initialize_candidates(Fann.ann) = 0 then
      begin
        // train new candidates
        total_epochs := total_epochs + fann_train_candidates(Fann.ann, traindata.TrainData);

        // this installs the best candidate
        fann_install_candidate(Fann.ann);
      end
      else
        IsBreak := True;
    end;

    Inc(i);
  until (i >= Fepochs_max) or IsBreak;

  // Train outputs one last time but without any desired error
  total_epochs := total_epochs + fann_train_outputs(Fann.ann, traindata.TrainData, 0.0);

  // Set pointers in connected_neurons
  // This is ONLY done in the end of cascade training,
  // since there is no need for them during training.
  fann_set_shortcut_connections(Fann.ann);
end;

procedure TTrainclass.train_on_data(traindata: TTraindataclass);
// aus fann_train_data.c
var
  i: Integer;
  error: Single;
  _is_desired_error_reached: Boolean;
  IsTrainingBreak: Boolean;
begin
  IsTrainingBreak := False;
  i               := 0;
  repeat
    error                     := fann_train_epoch(Fann.ann, traindata.TrainData);
    _is_desired_error_reached := Fann.is_error_reached(Fdesired_error);
    if (Fepochs_between_reports > 0) and (((i + 1) mod Fepochs_between_reports = 0) or (i + 1 = Fepochs_max) or (i = 0) or
      _is_desired_error_reached) then
    begin
      if Assigned(FFannEvent) then
        FFannEvent(i, error, IsTrainingBreak);
    end;
    Inc(i);
  until _is_desired_error_reached or (i >= Fepochs_max) or IsTrainingBreak;
end;

procedure TTrainclass.Test(traindata: TTraindataclass);
begin
  if traindata.IsDataValid and Assigned(Fann) then
    test_on_data(traindata);
end;

procedure TTrainclass.Train(traindata: TTraindataclass);
begin
  if traindata.IsDataValid and Assigned(Fann) then
    train_on_data(traindata);
end;

procedure TTrainclass.TrainCascade(traindata: TTraindataclass);
begin
  train_cascade_on_data(traindata);
end;

procedure TTrainclass.TrainFromFannFile(Filename: string);
var
  td: TTraindataclass;
begin
  td := TTraindataclass.Create;
  try
    td.LoadFromFannFile(Filename);
    Train(td);
  finally
    td.free;
  end;
end;

procedure TTrainclass.test_on_data(traindata: TTraindataclass);
var
  i: Integer;
begin
  fann_reset_MSE(Fann.ann);
  for i := 0 to traindata.NumData - 1 do
    fann_test(Fann.ann, @traindata.Inputs[i], @traindata.Outputs[i]);
end;

end.
