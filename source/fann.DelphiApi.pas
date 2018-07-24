// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

unit fann.DelphiApi;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, Vcl.Dialogs, Fann.Api;

type

  TNeuron = record
  strict private
    Fann: pfann;
    FNum_Layer, FNum_Neuron: Integer;
    FIsBias: Boolean;
    function Getactivation_function: Integer;
    procedure Setactivation_function(const Value: Integer);
    function Getactivation_steepness: Single;
    procedure Setactivation_steepness(const Value: Single);
  public
    constructor Create(ann: pfann; Num_Layer, Num_Neuron: Integer; IsBias: Boolean = False);
    property LayerIndx: Integer read FNum_Layer;
    property NeuronIndx: Integer read FNum_Neuron;
    property activation_function: Integer read Getactivation_function write Setactivation_function;
    property activation_steepness: Single read Getactivation_steepness write Setactivation_steepness;
    property IsBias: Boolean read FIsBias;
  end;

  TConnection = record
  strict private
    FNum_Connection: Integer;
    FFromNeuron: TNeuron;
    FToNeuron: TNeuron;
    FWeight: Single;
  public
    constructor Create(Num_Connection: Integer; FromNeuron, ToNeuron: TNeuron; Weight: Single);
    property Indx: Integer read FNum_Connection;
    property FromNeuron: TNeuron read FFromNeuron;
    property ToNeuron: TNeuron read FToNeuron;
    property Weight: Single read FWeight;
  end;

Const
  FANN_NETTYPE_LAYER = 0;
  FANN_NETTYPE_SHORTCUT = 1;

  FANN_TRAIN_INCREMENTAL = 0;
  FANN_TRAIN_BATCH = 1;
  FANN_TRAIN_RPROP = 2;
  FANN_TRAIN_QUICKPROP = 3;
  FANN_TRAIN_SARPROP = 4;

  FANN_ERRORFUNC_LINEAR = 0;
  FANN_ERRORFUNC_TANH = 1;

  FANN_STOPFUNC_MSE = 0;
  FANN_STOPFUNC_BIT = 1;

  FANN_LINEAR = 0;
  FANN_THRESHOLD = 1;
  FANN_THRESHOLD_SYMMETRIC = 2;
  FANN_SIGMOID = 3;
  FANN_SIGMOID_STEPWISE = 4;
  FANN_SIGMOID_SYMMETRIC = 5;
  FANN_SIGMOID_SYMMETRIC_STEPWISE = 6;
  FANN_GAUSSIAN = 7;
  FANN_GAUSSIAN_SYMMETRIC = 8;
  FANN_GAUSSIAN_STEPWISE = 9;
  FANN_ELLIOT = 10;
  FANN_ELLIOT_SYMMETRIC = 11;
  FANN_LINEAR_PIECE = 12;
  FANN_LINEAR_PIECE_SYMMETRIC = 13;
  FANN_SIN_SYMMETRIC = 14;
  FANN_COS_SYMMETRIC = 15;
  FANN_SIN = 16;
  FANN_COS = 17;
  FANN_RELU = 18;
  FANN_LEAKY_RELU = 19;

  FANN_ACTIVATIONFUNC_NAMES: array of string = ['FANN_LINEAR', 'FANN_THRESHOLD', 'FANN_THRESHOLD_SYMMETRIC', 'FANN_SIGMOID',
    'FANN_SIGMOID_STEPWISE', 'FANN_SIGMOID_SYMMETRIC', 'FANN_SIGMOID_SYMMETRIC_STEPWISE', 'FANN_GAUSSIAN',
    'FANN_GAUSSIAN_SYMMETRIC', 'FANN_GAUSSIAN_STEPWISE', 'FANN_ELLIOT', 'FANN_ELLIOT_SYMMETRIC', 'FANN_LINEAR_PIECE',
    'FANN_LINEAR_PIECE_SYMMETRIC', 'FANN_SIN_SYMMETRIC', 'FANN_COS_SYMMETRIC', 'FANN_SIN', 'FANN_COS', 'FANN_RELU',
    'FANN_LEAKY_RELU'];

type

  TFannclass = class
  private
    Fann: pfann;
    function CreateStandard(NeuronsPerLayer: TArray<Integer>; ConnectionRate: Single = 1): pfann;
    function CreateShortCut(NeuronsPerLayer: TArray<Integer>): pfann;
    function NeuronOfIndx(indx: Integer): TNeuron;
    function Getbit_fail_limit: fann_type;
    function Getlearning_momentum: Single;
    function Getlearning_rate: Single;
    function GetMSE: Single;
    function Gettrain_error_function: Tfann_errorfunc_enum;
    function Gettrain_stop_function: Tfann_stopfunc_enum;
    function Gettraining_algorithm: Tfann_train_enum;
    procedure Setbit_fail_limit(const Value: fann_type);
    procedure Setlearning_momentum(const Value: Single);
    procedure Setlearning_rate(const Value: Single);
    procedure Settrain_error_function(const Value: Tfann_errorfunc_enum);
    procedure Settrain_stop_function(const Value: Tfann_stopfunc_enum);
    procedure Settraining_algorithm(const Value: Tfann_train_enum);
    function Getconnection_rate: Single;
    function Getnetwork_type: Integer;
    function GetLayerCount: Integer;
    function Getactivation_functions: TArray<TEnumType>;
    function Getactivation_steepnesses: TArray<Single>;
    function Getactivation_steepnesses_count: Integer;
    function Getnum_candidate_groups: Integer;
    procedure Setactivation_functions(const Value: TArray<TEnumType>);
    procedure Setactivation_steepnesses(const Value: TArray<Single>);
    procedure Setnum_candidate_groups(const Value: Integer);
    function Getoutput_change_fraction: Single;
    procedure Setoutput_change_fraction(const Value: Single);
    function Getquickprop_decay: Single;
    procedure Setquickprop_decay(const Value: Single);
    function Getquickprop_mu: Single;
    procedure Setquickprop_mu(const Value: Single);
    function Getrprop_increase_factor: Single;
    procedure Setrprop_increase_factor(const Value: Single);
    function Getrprop_decrease_factor: Single;
    procedure Setrprop_decrease_factor(const Value: Single);
    function Getrprop_delta_max: Single;
    function Getrprop_delta_min: Single;
    procedure Setrprop_delta_max(const Value: Single);
    procedure Setrprop_delta_min(const Value: Single);
    function Getrprop_delta_zero: Single;
    procedure Setrprop_delta_zero(const Value: Single);
    function Getoutput_stagnation_epochs: Integer;
    procedure Setoutput_stagnation_epochs(const Value: Integer);
    function Getcandidate_change_fraction: Single;
    procedure Setcandidate_change_fraction(const Value: Single);
    function Getcandidate_stagnation_epochs: Integer;
    procedure Setcandidate_stagnation_epochs(const Value: Integer);
    function Getmax_out_epochs: Integer;
    procedure Setmax_out_epochs(const Value: Integer);
    function Getmin_out_epochs: Integer;
    procedure Setmin_out_epochs(const Value: Integer);
    function Getmax_cand_epochs: Integer;
    function Getmin_cand_epochs: Integer;
    procedure Setmax_cand_epochs(const Value: Integer);
    procedure Setmin_cand_epochs(const Value: Integer);
    function Getcandidate_limit: Single;
    procedure Setcandidate_limit(const Value: Single);
    function Getweight_multiplier: Single;
    procedure Setweight_multiplier(const Value: Single);
    function Getactivation_functions_count: Integer;
    function GetNeuronsPerLayer: TArray<Integer>;
    function GetNeuron(LayerIndx, NeuronIndx: Integer): TNeuron;
    function GetConnections: TArray<Tfann_connection>;
    function GetConnectionCount: Integer;
    function GetConnection(Indx: Integer): TConnection;
    function GetNeuronsAndBiasPerLayer: TArray<Integer>;
    function GetBiasPerLayer: TArray<Integer>;
    function GetNeuronCount(LayerIndx: Integer): Integer;
    function GetNeuronandBiasCount(LayerIndx: Integer): Integer;
    function Getnum_bit_fail: Integer;
    function Getsarprop_step_error_shift: Single;
    function Getsarprop_temperature: Single;
    function Getsarprop_weight_decay_shift: Single;
    procedure Setsarprop_step_error_shift(const Value: Single);
    procedure Setsarprop_temperature(const Value: Single);
    procedure Setsarprop_weight_decay_shift(const Value: Single);
    function Getsarprop_step_error_threshold_factor: Single;
    procedure Setsarprop_step_error_threshold_factor(const Value: Single);
  public
    constructor Create(NeuronsPerLayer: TArray<Integer>; NetWorkType: Integer = FANN_NETTYPE_LAYER;
      ConnectionRate: Single = 1); overload;
    constructor Create(Filename: string); overload;
    constructor Create(S: TStream); overload;
    destructor Destroy; override;
    procedure FannSave(Filename: string);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    function is_error_reached(Desired_Error: Single): Boolean;
    procedure DefNeutronLayer(LayerIndx: Cardinal; Steppness: single; AcFuntion: Cardinal);
    function Run(Inputs: TArray<Single>): TArray<Single>;
    property ann: pfann read Fann;
    property training_algorithm: Tfann_train_enum read Gettraining_algorithm write Settraining_algorithm;
    property learning_momentum: Single read Getlearning_momentum write Setlearning_momentum;
    property train_error_function: Tfann_errorfunc_enum read Gettrain_error_function write Settrain_error_function;
    property train_stop_function: Tfann_stopfunc_enum read Gettrain_stop_function write Settrain_stop_function;
    property bit_fail_limit: fann_type read Getbit_fail_limit write Setbit_fail_limit;
    property learning_rate: Single read Getlearning_rate write Setlearning_rate;
    property MSE: Single read GetMSE;
    property num_bit_fail: Integer read Getnum_bit_fail;
    property connection_rate: Single read Getconnection_rate;
    property network_type: Integer read Getnetwork_type;
    // Layer, Neurons, Bias, Connections
    property LayerCount: Integer read GetLayerCount;
    property NeuronCount[LayerIndx: Integer]: Integer read GetNeuronCount;
    property NeuronandBiasCount[LayerIndx: Integer]: Integer read GetNeuronandBiasCount;
    property Neuron[LayerIndx, NeuronIndx: Integer]: TNeuron read GetNeuron;
    property NeuronsPerLayer: TArray<Integer> read GetNeuronsPerLayer;
    property BiasPerLayer: TArray<Integer> read GetBiasPerLayer;
    property NeuronsAndBiasPerLayer: TArray<Integer> read GetNeuronsAndBiasPerLayer;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections: TArray<Tfann_connection> read GetConnections;
    property Connection[Indx: Integer]: TConnection read GetConnection;

    property quickprop_decay: Single read Getquickprop_decay write Setquickprop_decay;
    property quickprop_mu: Single read Getquickprop_mu write Setquickprop_mu;
    property rprop_increase_factor: Single read Getrprop_increase_factor write Setrprop_increase_factor;
    property rprop_decrease_factor: Single read Getrprop_decrease_factor write Setrprop_decrease_factor;
    property rprop_delta_min: Single read Getrprop_delta_min write Setrprop_delta_min;
    property rprop_delta_max: Single read Getrprop_delta_max write Setrprop_delta_max;
    property rprop_delta_zero: Single read Getrprop_delta_zero write Setrprop_delta_zero;
    property sarprop_weight_decay_shift: Single read Getsarprop_weight_decay_shift write Setsarprop_weight_decay_shift;
    property sarprop_step_error_threshold_factor: Single read Getsarprop_step_error_threshold_factor
      write Setsarprop_step_error_threshold_factor;
    property sarprop_step_error_shift: Single read Getsarprop_step_error_shift write Setsarprop_step_error_shift;
    property sarprop_temperature: Single read Getsarprop_temperature write Setsarprop_temperature;
    // Cascade :
    property activation_steepnesses: TArray<Single> read Getactivation_steepnesses write Setactivation_steepnesses;
    property activation_functions: TArray<TEnumType> read Getactivation_functions write Setactivation_functions;
    property activation_steepnesses_count: Integer read Getactivation_steepnesses_count;
    property num_candidate_groups: Integer read Getnum_candidate_groups write Setnum_candidate_groups;
    property output_change_fraction: Single read Getoutput_change_fraction write Setoutput_change_fraction;
    property output_stagnation_epochs: Integer read Getoutput_stagnation_epochs write Setoutput_stagnation_epochs;
    property candidate_change_fraction: Single read Getcandidate_change_fraction write Setcandidate_change_fraction;
    property candidate_stagnation_epochs: Integer read Getcandidate_stagnation_epochs write Setcandidate_stagnation_epochs;
    property max_out_epochs: Integer read Getmax_out_epochs write Setmax_out_epochs;
    property min_out_epochs: Integer read Getmin_out_epochs write Setmin_out_epochs;
    property max_cand_epochs: Integer read Getmax_cand_epochs write Setmax_cand_epochs;
    property min_cand_epochs: Integer read Getmin_cand_epochs write Setmin_cand_epochs;
    property candidate_limit: Single read Getcandidate_limit write Setcandidate_limit;
    property weight_multiplier: Single read Getweight_multiplier write Setweight_multiplier;
    property activation_functions_count: Integer read Getactivation_functions_count;
  end;

  TFannGraphPositions = class
  strict private
    Fann: TFannclass;
    FLayerCount: Integer;
    FPixelWidth: Integer;
    FPixelHeight: Integer;
    function PosY(N: TNeuron): Single;
    function PosX(N: TNeuron): Single;
    procedure MaxNeutronCount(var NCount, LayerIndx: Integer);
    function NeutronXDistance: Single;
    function NeutronYDistance: Single;
  public
    constructor Create(ann: TFannclass; PixelWidth, PixelHeight: Integer);
    function NeuronPosition(N: TNeuron): TPointF;
  end;

implementation

{ TFannclass }

constructor TFannclass.Create(NeuronsPerLayer: TArray<Integer>; NetWorkType: Integer; ConnectionRate: Single);
begin
  inherited Create;

  case NetWorkType of
    FANN_NETTYPE_LAYER:
      Fann := CreateStandard(NeuronsPerLayer, ConnectionRate);
    FANN_NETTYPE_SHORTCUT:
      begin
        if Length(NeuronsPerLayer) = 2 then
          Fann := CreateShortCut(NeuronsPerLayer)
        else
          raise Exception.Create('Wrong NeuronsPerLayer -> 2');
      end
  else
    raise Exception.Create('WrongNetworktype');
  end;
end;

constructor TFannclass.Create(Filename: string);
begin
  inherited Create;
  Fann := fann_create_from_file(PAnsiChar(AnsiString(FileName)));
end;

constructor TFannclass.Create(S: TStream);
begin
  inherited Create;
  LoadFromStream(S);
end;

function TFannclass.CreateShortCut(NeuronsPerLayer: TArray<Integer>): pfann;
begin
  Result := fann_create_shortcut_array(Length(NeuronsPerLayer), PCardinal(NeuronsPerLayer));
end;

function TFannclass.CreateStandard(NeuronsPerLayer: TArray<Integer>; ConnectionRate: Single): pfann;
begin
  Result := fann_create_sparse_array(ConnectionRate, Length(NeuronsPerLayer), PCardinal(NeuronsPerLayer));
end;

procedure TFannclass.DefNeutronLayer(LayerIndx: Cardinal; Steppness: single; AcFuntion: Cardinal);
begin
  fann_set_activation_function_layer(Fann, AcFuntion, LayerIndx);
  fann_set_activation_steepness_layer(Fann, Steppness, LayerIndx);
end;

destructor TFannclass.Destroy;
begin
  fann_destroy(Fann);
  inherited;
end;

procedure TFannclass.FannSave(Filename: string);
begin
  fann_save(Fann, PAnsiChar(AnsiString(FileName)));
end;

function TFannClass.Gettraining_algorithm: Tfann_train_enum;
begin
  Result := fann_get_training_algorithm(Fann);
end;

function TFannClass.Gettrain_error_function: Tfann_errorfunc_enum;
begin
  Result := fann_get_train_error_function(Fann);
end;

function TFannClass.Getlearning_momentum: Single;
begin
  Result := fann_get_learning_momentum(Fann);
end;

procedure TFannClass.Setlearning_momentum(const Value: Single);
begin
  fann_set_learning_momentum(Fann, Value);
end;

procedure TFannclass.Setlearning_rate(const Value: Single);
begin
  fann_set_learning_rate(Fann, Value);
end;

procedure TFannclass.Setmax_cand_epochs(const Value: Integer);
begin
  fann_set_cascade_max_cand_epochs(fann, Value);
end;

function TFannclass.Getmax_cand_epochs: Integer;
begin
  Result := fann_get_cascade_max_cand_epochs(Fann);
end;

procedure TFannclass.Setmin_cand_epochs(const Value: Integer);
begin
  fann_set_cascade_min_cand_epochs(fann, Value);
end;

function TFannclass.Getmin_cand_epochs: Integer;
begin
  Result := fann_get_cascade_min_cand_epochs(Fann);
end;

procedure TFannclass.Setmax_out_epochs(const Value: Integer);
begin
  fann_set_cascade_max_out_epochs(fann, Value);
end;

function TFannclass.Getmax_out_epochs: Integer;
begin
  Result := fann_get_cascade_max_out_epochs(fann);
end;

procedure TFannclass.Setmin_out_epochs(const Value: Integer);
begin
  fann_set_cascade_min_out_epochs(fann, Value);
end;

function TFannclass.Getmin_out_epochs: Integer;
begin
  Result := fann_get_cascade_min_out_epochs(fann);
end;

procedure TFannclass.Settraining_algorithm(const Value: Tfann_train_enum);
begin
  fann_set_training_algorithm(Fann, Value);
end;

procedure TFannClass.Settrain_error_function(const Value: Tfann_errorfunc_enum);
begin
  fann_set_train_error_function(Fann, Value);
end;

function TFannClass.Gettrain_stop_function: Tfann_stopfunc_enum;
begin
  Result := fann_get_train_stop_function(Fann);
end;

procedure TFannClass.Settrain_stop_function(const Value: Tfann_stopfunc_enum);
begin
  fann_set_train_stop_function(Fann, Value);
end;

function TFannclass.Getweight_multiplier: Single;
begin
  Result := fann_get_cascade_weight_multiplier(Fann);
end;

function TFannclass.is_error_reached(Desired_Error: Single): Boolean;
begin
  case Gettrain_stop_function of
    FANN_STOPFUNC_MSE:
      Result := GetMSE <= desired_error;
    FANN_STOPFUNC_BIT:
      Result := Getnum_bit_fail <= desired_error;
  else
    raise Exception.Create('Error: is_error_reached');
  end;
end;

procedure TFannclass.Setweight_multiplier(const Value: Single);
begin
  fann_set_cascade_weight_multiplier(Fann, Value);
end;

procedure TFannClass.Setbit_fail_limit(const Value: fann_type);
begin
  fann_set_bit_fail_limit(Fann, Value)
end;

procedure TFannclass.Setcandidate_change_fraction(const Value: Single);
begin
  fann_set_cascade_candidate_change_fraction(Fann, Value);
end;

function TFannclass.Getcandidate_change_fraction: Single;
begin
  Result := fann_get_cascade_candidate_change_fraction(Fann);
end;

procedure TFannclass.Setcandidate_limit(const Value: Single);
begin
  fann_set_cascade_candidate_limit(Fann, Value);
end;

function TFannclass.Getcandidate_limit: Single;
begin
  Result := fann_get_cascade_candidate_limit(Fann);
end;

procedure TFannclass.Setcandidate_stagnation_epochs(const Value: Integer);
begin
  fann_set_cascade_candidate_stagnation_epochs(Fann, Value);
end;

function TFannclass.Getcandidate_stagnation_epochs: Integer;
begin
  Result := fann_get_cascade_candidate_stagnation_epochs(Fann);
end;

function TFannclass.GetConnections: TArray<Tfann_connection>;
begin
  SetLength(Result, GetConnectionCount);
  fann_get_connection_array(Fann, pfann_connection(Result));
end;

function TFannclass.GetConnection(Indx: Integer): TConnection;
var
  C: TArray<Tfann_connection>;
begin
  C      := GetConnections;
  Result := TConnection.Create(Indx, NeuronOfIndx(C[Indx].from_neuron), NeuronOfIndx(C[Indx].to_neuron), C[Indx].weight);
end;

function TFannclass.GetConnectionCount: Integer;
begin
  Result := fann_get_total_connections(Fann);
end;

procedure TFannclass.Setnum_candidate_groups(const Value: Integer);
begin
  fann_set_cascade_num_candidate_groups(Fann, Value);
end;

function TFannclass.Getnum_bit_fail: Integer;
begin
  Result := fann_get_bit_fail(Fann);
end;

function TFannclass.Getnum_candidate_groups: Integer;
begin
  Result := fann_get_cascade_num_candidate_groups(Fann);
end;

procedure TFannclass.Setoutput_change_fraction(const Value: Single);
begin
  fann_set_cascade_output_change_fraction(Fann, Value);
end;

function TFannclass.Getoutput_change_fraction: Single;
begin
  Result := fann_get_cascade_output_change_fraction(Fann);
end;

procedure TFannclass.Setoutput_stagnation_epochs(const Value: Integer);
begin
  fann_set_cascade_output_stagnation_epochs(Fann, Value)
end;

function TFannclass.Getoutput_stagnation_epochs: Integer;
begin
  Result := fann_get_cascade_output_stagnation_epochs(Fann);
end;

procedure TFannclass.Setquickprop_decay(const Value: Single);
begin
  fann_set_quickprop_decay(Fann, Value);
end;

function TFannclass.Getquickprop_decay: Single;
begin
  Result := fann_get_quickprop_decay(Fann);
end;

procedure TFannclass.Setquickprop_mu(const Value: Single);
begin
  fann_set_quickprop_mu(Fann, Value);
end;

function TFannclass.Getquickprop_mu: Single;
begin
  Result := fann_get_quickprop_mu(Fann);
end;

procedure TFannclass.Setrprop_decrease_factor(const Value: Single);
begin
  fann_set_rprop_decrease_factor(Fann, Value);
end;

procedure TFannclass.Setrprop_delta_max(const Value: Single);
begin
  fann_set_rprop_delta_max(Fann, Value);
end;

function TFannclass.Getrprop_delta_max: Single;
begin
  Result := fann_get_rprop_delta_max(Fann);
end;

procedure TFannclass.Setrprop_delta_min(const Value: Single);
begin
  fann_set_rprop_delta_min(Fann, Value);
end;

function TFannclass.Getrprop_delta_min: Single;
begin
  Result := fann_get_rprop_delta_min(Fann);
end;

procedure TFannclass.Setrprop_delta_zero(const Value: Single);
begin
  fann_set_rprop_delta_zero(Fann, Value);
end;

function TFannclass.Getrprop_delta_zero: Single;
begin
  Result := fann_get_rprop_delta_zero(Fann);
end;

function TFannclass.Getrprop_decrease_factor: Single;
begin
  Result := fann_get_rprop_decrease_factor(Fann);
end;

procedure TFannclass.Setrprop_increase_factor(const Value: Single);
begin
  fann_set_rprop_increase_factor(Fann, Value);
end;

procedure TFannclass.Setsarprop_step_error_shift(const Value: Single);
begin
  fann_set_sarprop_step_error_shift(fann, Value);
end;

function TFannclass.Getsarprop_step_error_shift: Single;
begin
  Result := fann_get_sarprop_step_error_shift(fann);
end;

procedure TFannclass.Setsarprop_temperature(const Value: Single);
begin
  fann_set_sarprop_temperature(Fann, Value);
end;

function TFannclass.Getsarprop_temperature: Single;
begin
  Result := fann_get_sarprop_temperature(Fann);
end;

procedure TFannclass.Setsarprop_weight_decay_shift(const Value: Single);
begin
  fann_set_sarprop_weight_decay_shift(Fann, Value);
end;

function TFannclass.Getsarprop_weight_decay_shift: Single;
begin
  Result := fann_get_sarprop_weight_decay_shift(Fann);
end;

function TFannclass.Getrprop_increase_factor: Single;
begin
  Result := fann_get_rprop_increase_factor(Fann);
end;

procedure TFannclass.Setsarprop_step_error_threshold_factor(const Value: Single);
begin
  fann_set_sarprop_step_error_threshold_factor(Fann, Value);
end;

function TFannclass.Getsarprop_step_error_threshold_factor: Single;
begin
  Result := fann_get_sarprop_step_error_threshold_factor(Fann);
end;

function TFannclass.NeuronOfIndx(indx: Integer): TNeuron;
var
  NC: TArray<Integer>;
  i: Integer;
  ii: Integer;
  z: Integer;
begin
  NC       := GetNeuronsAndBiasPerLayer;
  z        := 0;
  for i    := 0 to Length(NC) - 1 do
    for ii := 0 to NC[i] - 1 do
    begin
      if z = indx then
        Exit(TNeuron.Create(Fann, i, ii));
      Inc(z);
    end;
end;

function TFannclass.Run(Inputs: TArray<Single>): TArray<Single>;
var
  numin: Integer;
  numout: Integer;
  Erg: pfann_type_array;
  i: Integer;
begin
  numin := fann_get_num_input(Fann);
  if Length(Inputs) = numin then
  begin
    numout := fann_get_num_output(Fann);
    SetLength(Result, numout);
    Erg   := fann_run(Fann, @Inputs[0]);
    for i := 0 to numout - 1 do
    begin
      Result[i] := Erg^;
      Inc(Erg);
    end;
  end;
end;

procedure TFannclass.SaveToStream(Stream: TStream);
var
  w: TWriter;
  i, ii, c: Integer;
  iL, iE: Integer;
  LayerArray: TArray<Integer>;
  LayerBiasArray: TArray<Integer>;
  ConArray: TArray<Tfann_connection>;
  Neuron: TNeuron;
  ValuesSingle: TArray<Single>;

  steepness: Single;
  acFunc: Integer;
  CN: Integer;
  iLayer: pfann_layer;
  iNeuron: pfann_neuron;
begin
  W := TWriter.Create(Stream, $FF);
  try
    W.WriteListBegin;
    W.WriteString(FANN_FLO_VERSION);
    W.WriteInteger(Getnetwork_type);
    W.WriteSingle(Getconnection_rate);
    W.WriteInteger(GetLayerCount);
    // Layer ohne Bias
    LayerArray := GetNeuronsPerLayer;
    for i      := 0 to Length(LayerArray) - 1 do
      W.WriteInteger(LayerArray[i]);
    // Layer mit Bias
    LayerBiasArray := GetNeuronsAndBiasPerLayer;
    for i          := 0 to Length(LayerBiasArray) - 1 do
      W.WriteInteger(LayerBiasArray[i]);

    W.WriteSingle(Getlearning_rate);
    W.WriteSingle(Getlearning_momentum);
    W.WriteInteger(Gettraining_algorithm);
    W.WriteInteger(Gettrain_error_function);
    W.WriteInteger(Gettrain_stop_function);
    //
    W.WriteSingle(Getoutput_change_fraction);
    W.WriteSingle(Getquickprop_decay);
    W.WriteSingle(Getquickprop_mu);
    W.WriteSingle(Getrprop_increase_factor);
    W.WriteSingle(Getrprop_decrease_factor);
    W.WriteSingle(Getrprop_delta_min);
    W.WriteSingle(Getrprop_delta_max);
    W.WriteSingle(Getrprop_delta_zero);
    W.WriteInteger(Getoutput_stagnation_epochs);
    W.WriteSingle(Getcandidate_change_fraction);
    W.WriteInteger(Getcandidate_stagnation_epochs);
    W.WriteInteger(Getmax_out_epochs);
    W.WriteInteger(Getmin_out_epochs);
    W.WriteInteger(Getmax_cand_epochs);
    W.WriteInteger(Getmin_cand_epochs);
    W.WriteInteger(Getnum_candidate_groups);

    W.WriteSingle(Getbit_fail_limit);
    W.WriteSingle(Getcandidate_limit);
    W.WriteSingle(Getweight_multiplier);

    // Last Neuron
    iLayer := fann.first_layer;
    c      := fann.last_layer - Fann.first_layer;
    for i  := 0 to c - 1 do
    begin
      CN      := iLayer.last_neuron - iLayer.first_neuron;
      iNeuron := iLayer.first_neuron;
      for ii  := 0 to CN - 1 do
      begin
        steepness := iNeuron.activation_steepness;
        acFunc    := iNeuron.activation_function;
        Inc(iNeuron);
      end;
      Inc(iLayer);
    end;
    W.WriteSingle(steepness);
    W.WriteInteger(acFunc);
    //

    LayerArray := Getactivation_functions;
    W.WriteInteger(Getactivation_functions_count);
    for i := 0 to Getactivation_functions_count - 1 do
      W.WriteInteger(LayerArray[i]);

    ValuesSingle := Getactivation_steepnesses;
    W.WriteInteger(Getactivation_steepnesses_count);
    for i := 0 to Getactivation_steepnesses_count - 1 do
      W.WriteSingle(ValuesSingle[i]);

    for iL   := 0 to GetLayerCount - 1 do
      for iE := 0 to LayerBiasArray[iL] - 1 do
      begin
        Neuron := GetNeuron(iL, iE);
        W.WriteInteger(Neuron.activation_function);
        W.WriteSingle(Neuron.activation_steepness);
      end;

    C := GetConnectionCount;
    W.WriteInteger(C);
    ConArray := GetConnections;
    for i    := 0 to C - 1 do
    begin
      W.WriteInteger(ConArray[i].from_neuron);
      W.WriteInteger(ConArray[i].to_neuron);
      W.WriteSingle(ConArray[i].weight);
    end;

    W.WriteListEnd;
  finally
    W.Free;
  end;
end;

procedure TFannclass.LoadFromStream(Stream: TStream);
var
  R: TReader;
  NP: Integer;
  LC: Integer;
  CR: Single;
  i, ii, c: Integer;
  iL, iE: Integer;
  S: string;
  Fnc: Integer;
  Stepness: Single;
  FromNeuron, ToNeuron: Integer;
  Weight: Single;
  LayerArray: TArray<Integer>;
  LayerBiasArray: TArray<Integer>;
  ConArray: TArray<Tfann_connection>;

  ValuesInt: TArray<Integer>;
  ValuesSingle: TArray<Single>;

  CN: Integer;
  iLayer: pfann_layer;
  iNeuron, LastNeuron: pfann_neuron;
begin
  R := TReader.Create(Stream, $FF);
  try
    R.ReadListBegin;
    S := R.ReadString;
    if not SameText(s, FANN_FLO_VERSION) then
      raise Exception.Create('Error : Wrong Version');
    NP := R.ReadInteger; // NetworkType
    CR := R.ReadSingle; // connection_rate

    LC := R.ReadInteger; // LayerCount
    // Layer ohne Bias
    SetLength(LayerArray, LC);
    for i           := 0 to LC - 1 do
      LayerArray[i] := R.ReadInteger; // NeuronsPerLayer
    // Layer mit Bias
    SetLength(LayerBiasArray, LC);
    for i               := 0 to LC - 1 do
      LayerBiasArray[i] := R.ReadInteger; // NeuronsAndBiasPerLayer

    fann_destroy(Fann);
    case NP of
      FANN_NETTYPE_LAYER:
        Fann := CreateStandard(LayerArray, CR);
      FANN_NETTYPE_SHORTCUT:
        Fann := CreateShortCut(LayerArray);
    else
      raise Exception.Create('Error: Wrong NetworkType');
    end;

    Setlearning_rate(R.ReadSingle);
    Setlearning_momentum(R.ReadSingle);
    Settraining_algorithm(R.ReadInteger);
    Settrain_error_function(R.ReadInteger);
    Settrain_stop_function(R.ReadInteger);
    //
    Setoutput_change_fraction(R.ReadSingle);
    Setquickprop_decay(R.ReadSingle);
    Setquickprop_mu(R.ReadSingle);
    Setrprop_increase_factor(R.ReadSingle);
    Setrprop_decrease_factor(R.ReadSingle);
    Setrprop_delta_min(R.ReadSingle);
    Setrprop_delta_max(R.ReadSingle);
    Setrprop_delta_zero(R.ReadSingle);
    Setoutput_stagnation_epochs(R.ReadInteger);
    Setcandidate_change_fraction(R.ReadSingle);
    Setcandidate_stagnation_epochs(R.ReadInteger);
    Setmax_out_epochs(R.ReadInteger);
    Setmin_out_epochs(R.ReadInteger);
    Setmax_cand_epochs(R.ReadInteger);
    Setmin_cand_epochs(R.ReadInteger);
    Setnum_candidate_groups(R.ReadInteger);

    Setbit_fail_limit(R.ReadSingle);
    Setcandidate_limit(R.ReadSingle);
    Setweight_multiplier(R.ReadSingle);

    // Last Neutron
    iLayer := fann.first_layer;
    c      := fann.last_layer - Fann.first_layer;
    for i  := 0 to c - 1 do
    begin
      CN      := iLayer.last_neuron - iLayer.first_neuron;
      iNeuron := iLayer.first_neuron;
      for ii  := 0 to CN - 1 do
      begin
        LastNeuron := iNeuron;
        Inc(iNeuron);
      end;
      Inc(iLayer);
    end;
    LastNeuron.activation_steepness := R.ReadSingle;
    LastNeuron.activation_function  := R.ReadInteger;
    //

    c := R.ReadInteger;
    SetLength(ValuesInt, c);
    for i          := 0 to c - 1 do
      ValuesInt[i] := R.ReadInteger;
    Setactivation_functions(ValuesInt);

    c := R.ReadInteger;
    SetLength(ValuesSingle, c);
    for i             := 0 to c - 1 do
      ValuesSingle[i] := R.ReadSingle;
    Setactivation_steepnesses(ValuesSingle);

    for iL   := 0 to LC - 1 do
      for iE := 0 to LayerBiasArray[iL] - 1 do
      begin
        Fnc      := R.ReadInteger;
        Stepness := R.ReadSingle;
        fann_set_activation_function(Fann, Fnc, iL, iE);
        fann_set_activation_steepness(Fann, Stepness, iL, iE);
      end;

    C := R.ReadInteger;
    SetLength(ConArray, C);
    for i := 0 to C - 1 do
    begin
      FromNeuron              := R.ReadInteger;
      ToNeuron                := R.ReadInteger;
      Weight                  := R.ReadSingle;
      ConArray[i].from_neuron := FromNeuron;
      ConArray[i].to_neuron   := ToNeuron;
      ConArray[i].weight      := Weight;
    end;
    fann_set_weight_array(fann, pfann_connection(ConArray), C);
  finally
    R.Free;
  end;
end;

procedure TFannclass.Setactivation_functions(const Value: TArray<TEnumType>);
begin
  fann_set_cascade_activation_functions(Fann, pfann_activationfunc_enum(Value), Length(Value));
end;

function TFannclass.Getactivation_functions: TArray<TEnumType>;
var
  erg: pfann_activationfunc_enum;
  i: Integer;
begin
  SetLength(Result, Getactivation_functions_count);
  erg   := fann_get_cascade_activation_functions(Fann);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := erg[i];
    // Result[i] := erg^;
    // Inc(erg);
  end;
end;

function TFannclass.Getactivation_functions_count: Integer;
begin
  Result := fann_get_cascade_activation_functions_count(Fann);
end;

procedure TFannclass.Setactivation_steepnesses(const Value: TArray<Single>);
begin
  fann_set_cascade_activation_steepnesses(Fann, pfann_type(Value), Length(Value));
end;

function TFannclass.Getactivation_steepnesses: TArray<single>;
var
  erg: pfann_type;
  i: Integer;
begin
  SetLength(Result, Getactivation_steepnesses_count);
  erg   := fann_get_cascade_activation_steepnesses(Fann);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := erg[i];
    // Result[i] := erg^;
    // Inc(erg);
  end;
end;

function TFannclass.Getactivation_steepnesses_count: Integer;
begin
  Result := fann_get_cascade_activation_steepnesses_count(Fann);
end;

function TFannclass.GetBiasPerLayer: TArray<Integer>;
begin
  SetLength(Result, GetLayerCount);
  fann_get_bias_array(fann, PCardinal(Result));
end;

function TFannClass.Getbit_fail_limit: fann_type;
begin
  Result := fann_get_bit_fail_limit(Fann);
end;

function TFannClass.Getconnection_rate: Single;
begin
  Result := fann_get_connection_rate(Fann);
end;

function TFannclass.GetLayerCount: Integer;
begin
  Result := fann_get_num_layers(Fann);
end;

function TFannClass.Getlearning_rate: Single;
begin
  Result := fann_get_learning_rate(Fann);
end;

function TFannClass.GetMSE: Single;
begin
  Result := fann_get_MSE(Fann);
end;

function TFannclass.Getnetwork_type: Integer;
begin
  Result := fann_get_network_type(Fann);
end;

function TFannclass.GetNeuron(LayerIndx, NeuronIndx: Integer): TNeuron;
var
  IsBias: Boolean;
begin
  if (LayerIndx < GetlayerCount) and (NeuronIndx < GetNeuronandBiasCount(LayerIndx)) then
  begin
    IsBias := (NeuronIndx = GetNeuronandBiasCount(LayerIndx) - 1) and not(LayerIndx = GetlayerCount - 1);
    Result := TNeuron.Create(Fann, LayerIndx, NeuronIndx, IsBias);
  end
  else
    raise Exception.Create('Error : No Neuron found');
end;

function TFannclass.GetNeuronandBiasCount(LayerIndx: Integer): Integer;
var
  LC: Integer;
begin
  LC := GetlayerCount;
  if (LayerIndx < LC) then
    Result := GetNeuronsAndBiasPerLayer[LayerIndx]
  else
    Result := -1;
end;

function TFannclass.GetNeuronCount(LayerIndx: Integer): Integer;
var
  LC: Integer;
begin
  LC := GetlayerCount;
  if (LayerIndx < LC) then
    Result := GetNeuronsPerLayer[LayerIndx]
  else
    Result := -1;
end;

function TFannclass.GetNeuronsAndBiasPerLayer: TArray<Integer>;
var
  N, B: TArray<Integer>;
  i: Integer;
begin
  SetLength(Result, GetLayerCount);
  N           := GetNeuronsPerLayer;
  B           := GetBiasPerLayer;
  for i       := 0 to GetLayerCount - 1 do
    Result[i] := N[i] + B[i];
end;

function TFannclass.GetNeuronsPerLayer: TArray<Integer>;
begin
  SetLength(Result, GetLayerCount);
  fann_get_layer_array(Fann, PCardinal(Result));
end;

{ TNeuron }

constructor TNeuron.Create(ann: pfann; Num_Layer, Num_Neuron: Integer; IsBias: Boolean);
begin
  Fann        := ann;
  FNum_Layer  := Num_Layer;
  FNum_Neuron := Num_Neuron;
  FIsBias     := IsBias;
end;

function TNeuron.Getactivation_function: Integer;
begin
  Result := fann_get_activation_function(Fann, FNum_Layer, FNum_Neuron);
end;

procedure TNeuron.Setactivation_function(const Value: Integer);
begin
  fann_set_activation_function(Fann, Value, FNum_Layer, FNum_Neuron);
end;

function TNeuron.Getactivation_steepness: Single;
begin
  Result := fann_get_activation_steepness(Fann, FNum_Layer, FNum_Neuron);
end;

procedure TNeuron.Setactivation_steepness(const Value: Single);
begin
  fann_set_activation_steepness(Fann, Value, FNum_Layer, FNum_Neuron);
end;

{ TConnection }

constructor TConnection.Create(Num_Connection: Integer; FromNeuron, ToNeuron: TNeuron; Weight: Single);
begin
  FNum_Connection := Num_Connection;
  FFromNeuron     := FromNeuron;
  FToNeuron       := ToNeuron;
  FWeight         := Weight;
end;

{ TFannGraphPositions }

constructor TFannGraphPositions.Create(ann: TFannclass; PixelWidth, PixelHeight: Integer);
begin
  inherited Create;
  Fann         := ann;
  FPixelWidth  := PixelWidth;
  FPixelHeight := PixelHeight;
  FLayerCount  := Fann.LayerCount;
end;

procedure TFannGraphPositions.MaxNeutronCount(var NCount, LayerIndx: Integer);
var
  i: Integer;
begin
  NCount    := -1;
  LayerIndx := -1;
  for i     := 0 to FLayerCount - 1 do
    if fann.NeuronandBiasCount[i] > NCount then
    begin
      NCount    := fann.NeuronandBiasCount[i];
      LayerIndx := i;
    end;
end;

function TFannGraphPositions.NeuronPosition(N: TNeuron): TPointF;
begin
  Result := TPointF.Create(PosX(N), PosY(N));
end;

function TFannGraphPositions.NeutronXDistance: Single;
var
  NCount, LayerIndx: Integer;
begin
  MaxNeutronCount(NCount, LayerIndx);
  Result := FPixelWidth / NCount;
end;

function TFannGraphPositions.NeutronYDistance: Single;
begin
  Result := FPixelHeight / (FLayerCount - 1)
end;

function TFannGraphPositions.PosX(N: TNeuron): Single;
var
  C: Integer;
  P: Single;
begin
  C := fann.NeuronandBiasCount[N.LayerIndx];
  if Odd(C) then
    P := N.NeuronIndx - (C - 1) / 2
  else
    P := N.NeuronIndx - C / 2 + 0.5;

  Result := FPixelWidth / 2 + P * NeutronXDistance;
end;

function TFannGraphPositions.PosY(N: TNeuron): Single;
begin
  Result := NeutronYDistance * N.LayerIndx;
end;

end.
