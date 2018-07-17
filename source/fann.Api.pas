// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

unit fann.Api;

{$POINTERMATH ON}
{$MINENUMSIZE 4}
{$WRITEABLECONST ON}

// Default - single(float) FANN
// or
// {$DEFINE FIXEDFANN} // Uncomment for fixed fann
// or
// {$DEFINE DOUBLEFANN} // Uncomment for double fann
interface

const
  VERSION = '2.2.0';

Type
  FANNChar = AnsiChar;
  PFANNChar = PAnsiChar;
  Float = Single;
  pFloat = ^Float;
  TEnumType = Integer;

  fann_type =
{$IF Defined(FIXEDFANN)}
    integer
{$ELSEIF Defined(DOUBLEFANN)}
    double
{$ELSE}
    Single
{$ENDIF}
    ;

  pfann_type = ^fann_type;
  ppfann_type = ^pfann_type;

  fann_type_array = array [word] of fann_type;
  pfann_type_array = pfann_type;
  ppfann_type_array = ^pfann_type_array;

  _iobuf = record
    _ptr: PFANNChar;
    _cnt: integer;
    _base: PFANNChar;
    _flag: integer;
    _file: integer;
    _charbuf: integer;
    _bufsiz: integer;
    _tmpfname: PFANNChar;
  end;

  PFile = ^TFile;
  TFile = _iobuf;

const

{$IF Defined(FIXEDFANN)}
  FANNFILENAME = 'fannfixed';
{$ELSEIF Defined(DOUBLEFANN)}
  FANNFILENAME = 'fanndouble';
{$ELSE}
  FANNFILENAME = 'fannfloat';
{$ENDIF}
{$IFDEF WIN32}
  FANNPLATFORM = '32';
{$ELSEIF Defined(WIN64)}
  FANNPLATFORM = '64';
{$ELSE}
raise Exception.Create('Error: Platform');
{$ENDIF}
FANN_DLL_FILE = FANNFILENAME + FANNPLATFORM + '.dll';

const
  RAND_MAX = $7FFF;

  FANN_FIX_VERSION = 'FANN_FIX_2.0';
  FANN_FLO_VERSION = 'FANN_FLO_2.1';

{$IFDEF FIXEDFANN}
  FANN_CONF_VERSION = FANN_FIX_VERSION;
{$ELSE}
  FANN_CONF_VERSION = FANN_FLO_VERSION;
{$ENDIF}
procedure fann_scale_data_to_range(data: ppfann_type; num_data: Cardinal; num_elem: Cardinal;
  old_min, old_max, new_min, new_max: fann_type); stdcall; external FANN_DLL_FILE name
{$IF Defined(DOUBLEFANN)}
  '_fann_scale_data_to_range@44';
{$ELSE}
  '_fann_scale_data_to_range@28';
{$ENDIF}

Type

  Tfann_train_enum = TEnumType;

const
  FANN_TRAIN_INCREMENTAL = 0;
  FANN_TRAIN_BATCH = 1;
  FANN_TRAIN_RPROP = 2;
  FANN_TRAIN_QUICKPROP = 3;
  FANN_TRAIN_SARPROP = 4;

  FANN_TRAIN_NAMES: array of PFANNChar = ['FANN_TRAIN_INCREMENTAL', 'FANN_TRAIN_BATCH', 'FANN_TRAIN_RPROP',
    'FANN_TRAIN_QUICKPROP', 'FANN_TRAIN_SARPROP'];

Type
  Tfann_activationfunc_enum = TEnumType;
  pfann_activationfunc_enum = ^Tfann_activationfunc_enum;

Const
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

  FANN_ACTIVATIONFUNC_NAMES: array of PFANNChar = ['FANN_LINEAR', 'FANN_THRESHOLD', 'FANN_THRESHOLD_SYMMETRIC', 'FANN_SIGMOID',
    'FANN_SIGMOID_STEPWISE', 'FANN_SIGMOID_SYMMETRIC', 'FANN_SIGMOID_SYMMETRIC_STEPWISE', 'FANN_GAUSSIAN',
    'FANN_GAUSSIAN_SYMMETRIC', 'FANN_GAUSSIAN_STEPWISE', 'FANN_ELLIOT', 'FANN_ELLIOT_SYMMETRIC', 'FANN_LINEAR_PIECE',
    'FANN_LINEAR_PIECE_SYMMETRIC', 'FANN_SIN_SYMMETRIC', 'FANN_COS_SYMMETRIC', 'FANN_SIN', 'FANN_COS', 'FANN_RELU',
    'FANN_LEAKY_RELU'];

Type
  Tfann_errorfunc_enum = TEnumType;

Const
  FANN_ERRORFUNC_LINEAR = 0;
  FANN_ERRORFUNC_TANH = 1;

  FANN_ERRORFUNC_NAMES: array of PFANNChar = ['FANN_ERRORFUNC_LINEAR', 'FANN_ERRORFUNC_TANH'];

Type
  Tfann_stopfunc_enum = TEnumType;

Const
  FANN_STOPFUNC_MSE = 0;
  FANN_STOPFUNC_BIT = 1;

  FANN_STOPFUNC_NAMES: array of PFANNChar = ['FANN_STOPFUNC_MSE', 'FANN_STOPFUNC_BIT'];

Type
  Tfann_nettype_enum = TEnumType;
  Tfann_errno_enum = TEnumType;

Const
  FANN_NETTYPE_LAYER = 0;
  FANN_NETTYPE_SHORTCUT = 1;

  FANN_NETTYPE_NAMES: array of PFANNChar = ['FANN_NETTYPE_LAYER', 'FANN_NETTYPE_SHORTCUT'];

Type
  pfann = ^Tfann;

  pfann_train_data = ^Tfann_train_data;

  Tfann_train_data = record
    errno_f: Tfann_errno_enum;
    error_log: PFile;
    errstr: PFANNChar;
    num_data: Cardinal;
    num_input: Cardinal;
    num_output: Cardinal;
    input: ppfann_type_array;
    output: ppfann_type_array;
  end;

  Tfann_callback_type = function(ann: pfann; train: pfann_train_data; max_epochs: Cardinal; epochs_between_reports: Cardinal;
    desired_error: Float; epochs: Cardinal): integer; stdcall;

  pfann_neuron = ^Tfann_neuron;
  ppfann_neuron = ^pfann_neuron;

  Tfann_neuron = record
    first_con: Cardinal;
    last_con: Cardinal;
    sum: fann_type;
    value: fann_type;
    activation_steepness: fann_type;
    activation_function: Tfann_activationfunc_enum;
  end;

  pfann_layer = ^Tfann_layer;

  Tfann_layer = record
    first_neuron: pfann_neuron;
    last_neuron: pfann_neuron;
  end;

  pfann_error = ^Tfann_error;

  Tfann_error = record
    errno_f: Tfann_errno_enum;
    error_log: PFile;
    errstr: PFANNChar;
  end;

  Tfann = record
    errno_f: Tfann_errno_enum;
    error_log: PFile;
    errstr: PFANNChar;
    learning_rate: Float;
    learning_momentum: Float;
    connection_rate: Float;
    network_type: Tfann_nettype_enum;
    first_layer: pfann_layer;
    last_layer: pfann_layer;
    total_neurons: Cardinal;
    num_input: Cardinal;
    num_output: Cardinal;
    weights: fann_type;
    connections: ppfann_neuron;
    train_errors: pfann_type;
    training_algorithm: Tfann_train_enum;
{$IFDEF FIXEDFANN}
    decimal_point: Cardinal;
    multiplier: Cardinal;
    sigmoid_results: array [0 .. 5] of fann_type;
    sigmoid_values: array [0 .. 5] of fann_type;
    symmetric_results: array [0 .. 5] of fann_type;
    symmetric_values: array [0 .. 5] of fann_type;
{$ENDIF}
    total_connections: Cardinal;
    output: pfann_type;
    num_MSE: Cardinal;
    MSE_value: Float;
    num_bit_fail: Cardinal;
    bit_fail_limit: fann_type;
    train_error_function: Tfann_errorfunc_enum;
    train_stop_function: Tfann_stopfunc_enum;
    callback: Tfann_callback_type;
    user_data: Pointer;
    cascade_output_change_fraction: Float;
    cascade_output_stagnation_epochs: Cardinal;
    cascade_candidate_change_fraction: Float;
    cascade_candidate_stagnation_epochs: Cardinal;
    cascade_best_candidate: Cardinal;
    cascade_candidate_limit: fann_type;
    cascade_weight_multiplier: fann_type;
    cascade_max_out_epochs: Cardinal;
    cascade_max_cand_epochs: Cardinal;
    cascade_min_out_epochs: Cardinal;
    cascade_min_cand_epochs: Cardinal;
    cascade_activation_functions: Tfann_activationfunc_enum;
    cascade_activation_functions_count: Cardinal;
    cascade_activation_steepnesses: pfann_type;
    cascade_activation_steepnesses_count: Cardinal;
    cascade_num_candidate_groups: Cardinal;
    cascade_candidate_scores: pfann_type;
    total_neurons_allocated: Cardinal;
    total_connections_allocated: Cardinal;
    quickprop_decay: Float;
    quickprop_mu: Float;
    rprop_increase_factor: Float;
    rprop_decrease_factor: Float;
    rprop_delta_min: Float;
    rprop_delta_max: Float;
    rprop_delta_zero: Float;
    sarprop_weight_decay_shift: Float;
    sarprop_step_error_threshold_factor: Float;
    sarprop_step_error_shift: Float;
    sarprop_temperature: Float;
    sarprop_epoch: Cardinal;
    train_slopes: pfann_type;
    prev_steps: pfann_type;
    prev_train_slopes: pfann_type;
    prev_weights_deltas: pfann_type;
{$IFNDEF FIXEDFANN}
    scale_mean_in: pFloat;
    scale_deviation_in: pFloat;
    scale_new_min_in: pFloat;
    scale_factor_in: pFloat;
    scale_mean_out: pFloat;
    scale_deviation_out: pFloat;
    scale_new_min_out: pFloat;
    scale_factor_out: pFloat;
{$ENDIF}
  end;

  pfann_connection = ^Tfann_connection;

  Tfann_connection = record
    from_neuron: Cardinal;
    to_neuron: Cardinal;
    weight: fann_type;
  end;

const
  FANN_ERRSTR_MAX = 128;

const
  FANN_E_NO_ERROR = 0;
  FANN_E_CANT_OPEN_CONFIG_R = 1;
  FANN_E_CANT_OPEN_CONFIG_W = 2;
  FANN_E_WRONG_CONFIG_VERSION = 3;
  FANN_E_CANT_READ_CONFIG = 4;
  FANN_E_CANT_READ_NEURON = 5;
  FANN_E_CANT_READ_CONNECTIONS = 6;
  FANN_E_WRONG_NUM_CONNECTIONS = 7;
  FANN_E_CANT_OPEN_TD_W = 8;
  FANN_E_CANT_OPEN_TD_R = 9;
  FANN_E_CANT_READ_TD = 10;
  FANN_E_CANT_ALLOCATE_MEM = 11;
  FANN_E_CANT_TRAIN_ACTIVATION = 12;
  FANN_E_CANT_USE_ACTIVATION = 13;
  FANN_E_TRAIN_DATA_MISMATCH = 14;
  FANN_E_CANT_USE_TRAIN_ALG = 15;
  FANN_E_TRAIN_DATA_SUBSET = 16;
  FANN_E_INDEX_OUT_OF_BOUND = 17;
  FANN_E_SCALE_NOT_PRESENT = 18;
  FANN_E_INPUT_NO_MATCH = 19;
  FANN_E_OUTPUT_NO_MATCH = 20;
  FANN_E_WRONG_PARAMETERS_FOR_CREATE = 21;

procedure fann_set_error_log(errdat: pfann_error; Log_File: PFile); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_error_log@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_error_log'
{$ENDIF}
  ;
function fann_get_errno(errdat: pfann_error): Tfann_errno_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_errno@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_errno'
{$ENDIF}
  ;
procedure fann_reset_errno(errdat: pfann_error); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_reset_errno@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_reset_errno'
{$ENDIF}
  ;
procedure fann_reset_errstr(errdat: pfann_error); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_reset_errstr@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_reset_errstr'
{$ENDIF}
  ;
function fann_get_errstr(errdat: pfann_error): PFANNChar; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_errstr@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_errstr'
{$ENDIF}
  ;

procedure fann_print_error(errdat: pfann_error); stdcall; external FANN_DLL_FILE name '_fann_print_error@4';

Var
  fann_default_error_log: PFile = PFile(-1);

function fann_create_from_file(const configuration_file: PFANNChar): pfann; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_from_file@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_from_file'
{$ENDIF}
  ;

procedure fann_save(ann: pfann; const configuration_file: PFANNChar); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_save@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_save'
{$ENDIF}
  ;

function fann_save_to_fixed(ann: pfann; const configuration_file: PFANNChar): integer; stdcall;
  external FANN_DLL_FILE name '_fann_save_to_fixed@8';

{$IFNDEF FIXEDFANN}
procedure fann_train(ann: pfann; input: pfann_type; Desired_Output: pfann_type); stdcall;
  external FANN_DLL_FILE name '_fann_train@12';

{$ENDIF}
function fann_test(ann: pfann; input: pfann_type; Desired_Output: pfann_type): pfann_type_array; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_test@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_test'
{$ENDIF}
  ;

function fann_get_MSE(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_MSE@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_MSE'
{$ENDIF}
  ;
function fann_get_bit_fail(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_bit_fail@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_bit_fail'
{$ENDIF}
  ;

procedure fann_reset_MSE(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_reset_MSE@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_reset_MSE'
{$ENDIF}
  ;

{$IFNDEF FIXEDFANN}
procedure fann_train_on_data(ann: pfann; data: pfann_train_data; max_epochs: Cardinal; epochs_between_reports: Cardinal;
  desired_error: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_on_data@20'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_on_data'
{$ENDIF}
  ;

procedure fann_train_on_file(ann: pfann; Filename: PFANNChar; max_epochs: Cardinal; epochs_between_reports: Cardinal;
  desired_error: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_on_file@20'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_on_file'
{$ENDIF}
  ;

function fann_train_epoch(ann: pfann; data: pfann_train_data): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_epoch@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_epoch'
{$ENDIF}
  ;
{$ENDIF}
function fann_test_data(ann: pfann; data: pfann_train_data): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_test_data@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_test_data'
{$ENDIF}
  ;

function fann_read_train_from_file(const Filename: PFANNChar): pfann_train_data; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_read_train_from_file@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_read_train_from_file'
{$ENDIF}
  ;

function fann_create_train(num_data: Cardinal; num_input: Cardinal; num_output: Cardinal): pfann_train_data; stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_train@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_train'
{$ENDIF}
  ;

Type
  TUser_Function = procedure(num: Cardinal; num_input: Cardinal; num_output: Cardinal; var input, output: pfann_type); stdcall;

function fann_create_train_from_callback(num_data: Cardinal; num_input: Cardinal; num_output: Cardinal;
  user_function: TUser_Function): pfann_train_data; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_train_from_callback@16'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_train_from_callback'
{$ENDIF}
  ;

function fann_create_train_array(num_data: Cardinal; num_input: Cardinal; input: pfann_type_array; num_output: Cardinal;
  output: pfann_type_array): pfann_train_data; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_train_array@20'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_train_pointer_array'
{$ENDIF}
  ;

function fann_create_train_pointer_array(num_data: Cardinal; num_input: Cardinal; input: ppfann_type_array; num_output: Cardinal;
  output: ppfann_type_array): pfann_train_data; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_train_pointer_array@20'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_train_pointer_array'
{$ENDIF}
  ;

procedure fann_destroy_train(train_data: pfann_train_data); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_destroy_train@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_destroy_train'
{$ENDIF}
  ;

procedure fann_shuffle_train_data(train_data: pfann_train_data); stdcall;
  external FANN_DLL_FILE name '_fann_shuffle_train_data@4';

{$IFNDEF FIXEDFANN}
procedure fann_scale_train(ann: pfann; data: pfann_train_data); stdcall; external FANN_DLL_FILE name '_fann_scale_train@8';

procedure fann_descale_train(ann: pfann; data: pfann_train_data); stdcall; external FANN_DLL_FILE name '_fann_descale_train@8';

function fann_set_input_scaling_params(ann: pfann; const data: pfann_train_data; new_input_min: Float; new_input_max: Float)
  : integer; stdcall; external FANN_DLL_FILE name '_fann_set_input_scaling_params@16';

function fann_set_output_scaling_params(ann: pfann; const data: pfann_train_data; new_output_min: Float; new_output_max: Float)
  : integer; stdcall; external FANN_DLL_FILE name '_fann_set_output_scaling_params@16';

function fann_set_scaling_params(ann: pfann; const data: pfann_train_data; new_input_min: Float; new_input_max: Float;
  new_output_min: Float; new_output_max: Float): integer; stdcall; external FANN_DLL_FILE name '_fann_set_scaling_params@24';

function fann_clear_scaling_params(ann: pfann): integer; stdcall; external FANN_DLL_FILE name '_fann_clear_scaling_params@4';

procedure fann_scale_input(ann: pfann; input_vector: pfann_type); stdcall; external FANN_DLL_FILE name
{$IF Defined(FIXEDFANN)}
  '_fann_scale_input_train_data@12';
{$ELSE}
  '_fann_scale_input@8';
{$ENDIF}
procedure fann_scale_output(ann: pfann; output_vector: pfann_type); stdcall; external FANN_DLL_FILE name
{$IF Defined(FIXEDFANN)}
  '_fann_scale_output@12';
{$ELSE}
  '_fann_scale_output@8';
{$ENDIF}
procedure fann_descale_input(ann: pfann; input_vector: pfann_type); stdcall; external FANN_DLL_FILE name '_fann_descale_input@8';

procedure fann_descale_output(ann: pfann; output_vector: pfann_type); stdcall;
  external FANN_DLL_FILE name '_fann_descale_input@8';

{$ENDIF}
procedure fann_scale_input_train_data(train_data: pfann_train_data; new_min: fann_type; new_max: fann_type); stdcall;
  external FANN_DLL_FILE name '_fann_descale_output@8';

procedure fann_scale_output_train_data(train_data: pfann_train_data; new_min: fann_type; new_max: fann_type); stdcall;
  external FANN_DLL_FILE name
{$IFDEF DOUBLEFANN}
  '_fann_scale_input_train_data@20';
{$ELSE}
  '_fann_scale_input_train_data@12';
{$ENDIF}
procedure fann_scale_train_data(train_data: pfann_train_data; new_min: fann_type; new_max: fann_type); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_scale_train_data@20'
{$ELSE}
  name '_fann_scale_train_data@12'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_scale_train_data'
{$ENDIF}
  ;

function fann_merge_train_data(Data1: pfann_train_data; Data2: pfann_train_data): pfann_train_data; stdcall;
  external FANN_DLL_FILE name '_fann_merge_train_data@8';

function fann_duplicate_train_data(data: pfann_train_data): pfann_train_data; stdcall;
  external FANN_DLL_FILE name '_fann_duplicate_train_data@4';

function fann_subset_train_data(data: pfann_train_data; pos: Cardinal; length: Cardinal): pfann_train_data; stdcall;
  external FANN_DLL_FILE name '_fann_subset_train_data@12';

function fann_length_train_data(data: pfann_train_data): Cardinal; stdcall;
  external FANN_DLL_FILE name '_fann_length_train_data@4';

function fann_num_input_train_data(data: pfann_train_data): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_num_input_train_data@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_num_input_train_data'
{$ENDIF}
  ;

function fann_num_output_train_data(data: pfann_train_data): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_num_output_train_data@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_num_output_train_data'
{$ENDIF}
  ;

function fann_save_train(data: pfann_train_data; const Filename: PFANNChar): integer; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_save_train@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_save_train'
{$ENDIF}
  ;

function fann_save_train_to_fixed(data: pfann_train_data; const Filename: PFANNChar; decimal_point: Cardinal): integer; stdcall;
  external FANN_DLL_FILE name '_fann_save_train_to_fixed@12';

function fann_get_training_algorithm(ann: pfann): Tfann_train_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_training_algorithm@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_training_algorithm'
{$ENDIF}
  ;

procedure fann_set_training_algorithm(ann: pfann; training_algorithm: Tfann_train_enum); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_training_algorithm@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_training_algorithm'
{$ENDIF}
  ;

function fann_get_learning_rate(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_learning_rate@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_learning_rate'
{$ENDIF}
  ;

procedure fann_set_learning_rate(ann: pfann; learning_rate: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_learning_rate@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_learning_rate'
{$ENDIF}
  ;

function fann_get_learning_momentum(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_learning_momentum@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_learning_momentum'
{$ENDIF}
  ;

procedure fann_set_learning_momentum(ann: pfann; learning_momentum: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_learning_momentum@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_learning_momentum'
{$ENDIF}
  ;

function fann_get_activation_function(ann: pfann; layer: integer; neuron: integer): Tfann_activationfunc_enum; stdcall;
  external FANN_DLL_FILE
{$IFDEF  WIN32}
  name '_fann_get_activation_function@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_activation_function'
{$ENDIF}
  ;

procedure fann_set_activation_function(ann: pfann; activation_function: Tfann_activationfunc_enum; layer: integer;
  neuron: integer); stdcall; external FANN_DLL_FILE
{$IFDEF  WIN32}
  name '_fann_set_activation_function@16'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_function'
{$ENDIF}
  ;

procedure fann_set_activation_function_layer(ann: pfann; activation_function: Tfann_activationfunc_enum; layer: integer); stdcall;
  external FANN_DLL_FILE
{$IFDEF  WIN32}
  name '_fann_set_activation_function_layer@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_function_layer'
{$ENDIF}
  ;

procedure fann_set_activation_function_hidden(ann: pfann; activation_function: Tfann_activationfunc_enum); stdcall;
  external FANN_DLL_FILE
{$IFDEF  WIN32}
  name '_fann_set_activation_function_hidden@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_function_hidden'
{$ENDIF}
  ;

procedure fann_set_activation_function_output(ann: pfann; activation_function: Tfann_activationfunc_enum); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_activation_function_output@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_function_output'
{$ENDIF}
  ;
function fann_get_activation_steepness(ann: pfann; layer: integer; neuron: integer): fann_type; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_activation_steepness@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_activation_steepness'
{$ENDIF}
  ;

procedure fann_set_activation_steepness(ann: pfann; steepness: fann_type; layer: integer; neuron: integer); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_activation_steepness@20'
{$ELSE}
  name '_fann_set_activation_steepness@16'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_steepness'
{$ENDIF}
  ;

procedure fann_set_activation_steepness_layer(ann: pfann; steepness: fann_type; layer: integer); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_activation_steepness_layer@16'
{$ELSE}
  name '_fann_set_activation_steepness_layer@12'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_steepness_layer'
{$ENDIF}
  ;

procedure fann_set_activation_steepness_hidden(ann: pfann; steepness: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_activation_steepness_hidden@12'
{$ELSE}
  name '_fann_set_activation_steepness_hidden@8'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_steepness_hidden'
{$ENDIF}
  ;

procedure fann_set_activation_steepness_output(ann: pfann; steepness: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_activation_steepness_output@12'
{$ELSE}
  name '_fann_set_activation_steepness_output@8'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_activation_steepness_output'
{$ENDIF}
  ;

function fann_get_train_error_function(ann: pfann): Tfann_errorfunc_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_train_error_function@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_train_error_function'
{$ENDIF}
  ;

procedure fann_set_train_error_function(ann: pfann; train_error_function: Tfann_errorfunc_enum); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_train_error_function@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_train_error_function'
{$ENDIF}
  ;

function fann_get_train_stop_function(ann: pfann): Tfann_stopfunc_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_train_stop_function@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_train_stop_function'
{$ENDIF}
  ;
procedure fann_set_train_stop_function(ann: pfann; train_stop_function: Tfann_stopfunc_enum); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_train_stop_function@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_train_stop_function'
{$ENDIF}
  ;

function fann_get_bit_fail_limit(ann: pfann): fann_type; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_bit_fail_limit@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_bit_fail_limit'
{$ENDIF}
  ;

procedure fann_set_bit_fail_limit(ann: pfann; bit_fail_limit: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_bit_fail_limit@12';
{$ELSE}
  name '_fann_set_bit_fail_limit@8';
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
name 'fann_set_bit_fail_limit';
{$ENDIF}
procedure fann_set_callback(ann: pfann; callback: Tfann_callback_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_callback@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_callback'
{$ENDIF}
  ;
//
function fann_get_quickprop_decay(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_quickprop_decay@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_quickprop_decay'
{$ENDIF}
  ;

procedure fann_set_quickprop_decay(ann: pfann; quickprop_decay: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_quickprop_decay@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_quickprop_decay'
{$ENDIF}
  ;

function fann_get_quickprop_mu(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_quickprop_mu@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_quickprop_mu'
{$ENDIF}
  ;

procedure fann_set_quickprop_mu(ann: pfann; Mu: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_quickprop_mu@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_quickprop_mu'
{$ENDIF}
  ;

function fann_get_rprop_increase_factor(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_rprop_increase_factor@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_rprop_increase_factor'
{$ENDIF}
  ;

procedure fann_set_rprop_increase_factor(ann: pfann; rprop_increase_factor: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_rprop_increase_factor@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_rprop_increase_factor'
{$ENDIF}
  ;

function fann_get_rprop_decrease_factor(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_rprop_decrease_factor@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_rprop_decrease_factor'
{$ENDIF}
  ;

procedure fann_set_rprop_decrease_factor(ann: pfann; rprop_decrease_factor: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_rprop_decrease_factor@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_rprop_decrease_factor'
{$ENDIF}
  ;

function fann_get_rprop_delta_min(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_rprop_delta_min@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_rprop_delta_min'
{$ENDIF}
  ;

procedure fann_set_rprop_delta_min(ann: pfann; rprop_delta_min: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_rprop_delta_min@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_rprop_delta_min'
{$ENDIF}
  ;

function fann_get_rprop_delta_max(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_rprop_delta_max@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_rprop_delta_max'
{$ENDIF}
  ;

procedure fann_set_rprop_delta_max(ann: pfann; rprop_delta_max: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_rprop_delta_max@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_rprop_delta_max'
{$ENDIF}
  ;

function fann_get_rprop_delta_zero(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_rprop_delta_zero@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_rprop_delta_zero'
{$ENDIF}
  ;

procedure fann_set_rprop_delta_zero(ann: pfann; rprop_delta_zero: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_rprop_delta_zero@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_rprop_delta_zero'
{$ENDIF}
  ;

function fann_get_sarprop_weight_decay_shift(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_sarprop_weight_decay_shift@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_sarprop_weight_decay_shift'
{$ENDIF}
  ;

procedure fann_set_sarprop_weight_decay_shift(ann: pfann; sarprop_weight_decay_shift: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_sarprop_weight_decay_shift@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_sarprop_weight_decay_shift'
{$ENDIF}
  ;

function fann_get_sarprop_step_error_threshold_factor(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_sarprop_step_error_threshold_factor@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_sarprop_step_error_threshold_factor'
{$ENDIF}
  ;

procedure fann_set_sarprop_step_error_threshold_factor(ann: pfann; sarprop_step_error_threshold_factor: Float); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_sarprop_step_error_threshold_factor@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_sarprop_step_error_threshold_factor'
{$ENDIF}
  ;

function fann_get_sarprop_step_error_shift(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_sarprop_step_error_shift@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_sarprop_step_error_shift'
{$ENDIF}
  ;

procedure fann_set_sarprop_step_error_shift(ann: pfann; sarprop_step_error_shift: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_sarprop_step_error_shift@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_sarprop_step_error_shift'
{$ENDIF}
  ;

function fann_get_sarprop_temperature(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_sarprop_temperature@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_sarprop_temperature'
{$ENDIF}
  ;

procedure fann_set_sarprop_temperature(ann: pfann; sarprop_temperature: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_sarprop_temperature@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_sarprop_temperature'
{$ENDIF}
  ;

{$IFNDEF FIXEDFANN}
procedure fann_cascadetrain_on_data(ann: pfann; data: pfann_train_data; max_neurons: Cardinal; neurons_between_reports: Cardinal;
  desired_error: Float); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_cascadetrain_on_data@20'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_cascadetrain_on_data'
{$ENDIF}
  ;

function fann_train_outputs(ann: pfann; data: pfann_train_data; desired_error: Float): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_outputs@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_outputs'
{$ENDIF}
  ;
function fann_initialize_candidates(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_initialize_candidates@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_initialize_candidates'
{$ENDIF}
  ;
function fann_train_candidates(ann: pfann; data: pfann_train_data): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_candidates@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_candidates'
{$ENDIF}
  ;
procedure fann_install_candidate(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_install_candidate@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_install_candidate'
{$ENDIF}
  ;
procedure fann_set_shortcut_connections(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_shortcut_connections@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_shortcut_connections'
{$ENDIF}
  ;

procedure fann_cascadetrain_on_file(ann: pfann; const Filename: PFANNChar; max_neurons: Cardinal;
  neurons_between_reports: Cardinal; desired_error: Float); stdcall; external FANN_DLL_FILE name '_fann_cascadetrain_on_file@20';
{$ENDIF}
function fann_get_cascade_output_change_fraction(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_output_change_fraction@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_output_change_fraction'
{$ENDIF}
  ;

procedure fann_set_cascade_output_change_fraction(ann: pfann; cascade_output_change_fraction: Float); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_output_change_fraction@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_output_change_fraction'
{$ENDIF}
  ;

function fann_get_cascade_output_stagnation_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_output_stagnation_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_output_stagnation_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_output_stagnation_epochs(ann: pfann; cascade_output_stagnation_epochs: Cardinal); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_output_stagnation_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_output_stagnation_epochs'
{$ENDIF}
  ;

function fann_get_cascade_candidate_change_fraction(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_candidate_change_fraction@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_candidate_change_fraction'
{$ENDIF}
  ;

procedure fann_set_cascade_candidate_change_fraction(ann: pfann; cascade_candidate_change_fraction: Float); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_candidate_change_fraction@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_candidate_change_fraction'
{$ENDIF}
  ;

function fann_get_cascade_candidate_stagnation_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_candidate_stagnation_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_candidate_stagnation_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_candidate_stagnation_epochs(ann: pfann; cascade_candidate_stagnation_epochs: Cardinal); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_candidate_stagnation_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_candidate_stagnation_epochs'
{$ENDIF}
  ;

function fann_get_cascade_weight_multiplier(ann: pfann): fann_type; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_weight_multiplier@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_weight_multiplier'
{$ENDIF}
  ;

procedure fann_set_cascade_weight_multiplier(ann: pfann; cascade_weight_multiplier: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_cascade_weight_multiplier@12'
{$ELSE}
  name '_fann_set_cascade_weight_multiplier@8'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_weight_multiplier'
{$ENDIF}
  ;

function fann_get_cascade_candidate_limit(ann: pfann): fann_type; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_candidate_limit@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_candidate_limit'
{$ENDIF}
  ;

procedure fann_set_cascade_candidate_limit(ann: pfann; cascade_candidate_limit: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
{$IFDEF DOUBLEFANN}
  name '_fann_set_cascade_candidate_limit@12'
{$ELSE}
  name '_fann_set_cascade_candidate_limit@8'
{$ENDIF}
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_candidate_limit'
{$ENDIF}
  ;

function fann_get_cascade_max_out_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_max_out_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_max_out_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_max_out_epochs(ann: pfann; cascade_max_out_epochs: Cardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_max_out_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_max_out_epochs'
{$ENDIF}
  ;

function fann_get_cascade_min_out_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_min_out_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_min_out_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_min_out_epochs(ann: pfann; cascade_min_out_epochs: Cardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_min_out_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_min_out_epochs'
{$ENDIF}
  ;

function fann_get_cascade_max_cand_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_max_cand_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_max_cand_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_max_cand_epochs(ann: pfann; cascade_max_cand_epochs: Cardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_max_cand_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_max_cand_epochs'
{$ENDIF}
  ;

function fann_get_cascade_min_cand_epochs(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_min_cand_epochs@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_min_cand_epochs'
{$ENDIF}
  ;

procedure fann_set_cascade_min_cand_epochs(ann: pfann; cascade_min_cand_epochs: Cardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_min_cand_epochs@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_min_cand_epochs'
{$ENDIF}
  ;

function fann_get_cascade_num_candidates(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_num_candidates@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_num_candidates'
{$ENDIF}
  ;

function fann_get_cascade_activation_functions_count(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_activation_functions_count@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_activation_functions_count'
{$ENDIF}
  ;

function fann_get_cascade_activation_functions(ann: pfann): pfann_activationfunc_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_activation_functions@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_activation_functions'
{$ENDIF}
  ;

procedure fann_set_cascade_activation_functions(ann: pfann; cascade_activation_functions: pfann_activationfunc_enum;
  cascade_activation_functions_count: Cardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_activation_functions@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_activation_functions'
{$ENDIF}
  ;

function fann_get_cascade_activation_steepnesses_count(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_activation_steepnesses_count@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_activation_steepnesses_count'
{$ENDIF}
  ;

function fann_get_cascade_activation_steepnesses(ann: pfann): pfann_type; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_activation_steepnesses@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_activation_steepnesses'
{$ENDIF}
  ;

procedure fann_set_cascade_activation_steepnesses(ann: pfann; cascade_activation_steepnesses: pfann_type;
  cascade_activation_steepnesses_count: Cardinal);
stdcall external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_activation_steepnesses@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_activation_steepnesses'
{$ENDIF}
  ;

function fann_get_cascade_num_candidate_groups(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_cascade_num_candidate_groups@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_cascade_num_candidate_groups'
{$ENDIF}
  ;

procedure fann_set_cascade_num_candidate_groups(ann: pfann; cascade_num_candidate_groups: Cardinal); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_cascade_num_candidate_groups@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_cascade_num_candidate_groups'
{$ENDIF}
  ;

function fann_create_standard(num_layers: Cardinal): pfann; cdecl; varargs; external FANN_DLL_FILE name 'fann_create_standard';

function fann_create_standard_array(num_layers: Cardinal; const layers: PCardinal): pfann; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_standard_array@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_standard_array'
{$ENDIF}
  ;

function fann_create_sparse(connection_rate: Float; num_layers: Cardinal): pfann; cdecl; varargs;
  external FANN_DLL_FILE name 'fann_create_sparse';

function fann_create_sparse_array(connection_rate: Float; num_layers: Cardinal; const layers: PCardinal): pfann; stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_sparse_array@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_sparse_array'
{$ENDIF}
  ;

function fann_create_shortcut(num_layers: Cardinal): pfann; cdecl; varargs; external FANN_DLL_FILE name 'fann_create_shortcut';

function fann_create_shortcut_array(num_layers: Cardinal; const layers: PCardinal): pfann; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_create_shortcut_array@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_create_shortcut_array'
{$ENDIF}
  ;

procedure fann_destroy(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32} name '_fann_destroy@4' {$ENDIF} {$IFDEF WIN64} name 'fann_destroy' {$ENDIF};

function fann_copy(ann: pfann): pfann; stdcall; external FANN_DLL_FILE name '_fann_copy@4';

function fann_run(ann: pfann; input: pfann_type): pfann_type_array; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32} name '_fann_run@8' {$ENDIF} {$IFDEF WIN64} name 'fann_run' {$ENDIF};

procedure fann_randomize_weights(ann: pfann; min_weight: fann_type; max_weight: fann_type); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_randomize_weights@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_randomize_weights'
{$ENDIF}
  ;

procedure fann_init_weights(ann: pfann; train_data: pfann_train_data); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32} name '_fann_init_weights@8' {$ENDIF} {$IFDEF WIN64} name 'fann_init_weights' {$ENDIF};

procedure fann_print_connections(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_print_connections@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_print_connections'
{$ENDIF}
  ;

procedure fann_print_parameters(ann: pfann); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_print_parameters@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_print_parameters'
{$ENDIF}
  ;

function fann_get_num_input(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_num_input@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_num_input'
{$ENDIF}
  ;

function fann_get_num_output(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_num_output@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_num_output'
{$ENDIF}
  ;

function fann_get_total_neurons(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_total_neurons@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_total_neurons'
{$ENDIF}
  ;

function fann_get_total_connections(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_total_connections@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_total_connections'
{$ENDIF}
  ;

function fann_get_network_type(ann: pfann): Tfann_nettype_enum; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_network_type@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_network_type'
{$ENDIF}
  ;

function fann_get_connection_rate(ann: pfann): Float; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_connection_rate@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_connection_rate'
{$ENDIF}
  ;

function fann_get_num_layers(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_num_layers@4'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_num_layers'
{$ENDIF}
  ;

procedure fann_get_layer_array(ann: pfann; layers: PCardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_layer_array@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_layer_array'
{$ENDIF}
  ;

procedure fann_get_bias_array(ann: pfann; bias: PCardinal); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_bias_array@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_bias_array'
{$ENDIF}
  ;

procedure fann_get_connection_array(ann: pfann; connections: pfann_connection); stdcall; external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_get_connection_array@8'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_get_connection_array'
{$ENDIF}
  ;

procedure fann_set_weight_array(ann: pfann; connections: pfann_connection; num_connection: Cardinal); stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_set_weight_array@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_set_weight_array'
{$ENDIF}
  ;

procedure fann_set_weight(ann: pfann; from_neuron: Cardinal; to_neuron: Cardinal; weight: fann_type); stdcall;
  external FANN_DLL_FILE name
{$IFDEF DOUBLEFANN}
  '_fann_set_weight@20';
{$ELSE}
  '_fann_set_weight@16';
{$ENDIF}
procedure fann_set_user_data(ann: pfann; user_data: Pointer); stdcall; external FANN_DLL_FILE name '_fann_set_user_data@8';

function fann_get_user_data(ann: pfann): Pointer; stdcall; external FANN_DLL_FILE name '_fann_get_user_data@4';

{$IFDEF FIXEDFANN}
function fann_get_decimal_point(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE name '_fann_get_decimal_point@4';
function fann_get_multiplier(ann: pfann): Cardinal; stdcall; external FANN_DLL_FILE name '_fann_get_multiplier@4';
{$ENDIF}
{$IFNDEF FIXEDFANN}
function fann_train_epoch_batch_parallel(ann: pfann; data: pfann_train_data; const threadnumb: Cardinal): Float; stdcall;
  external FANN_DLL_FILE name '_fann_train_epoch_batch_parallel@12';

function fann_train_epoch_irpropm_parallel(ann: pfann; data: pfann_train_data; const threadnumb: Cardinal): Float; stdcall;
  external FANN_DLL_FILE
{$IFDEF WIN32}
  name '_fann_train_epoch_irpropm_parallel@12'
{$ENDIF} {$IFDEF WIN64}
  name 'fann_train_epoch_irpropm_parallel'
{$ENDIF}
  ;

function fann_train_epoch_quickprop_parallel(ann: pfann; data: pfann_train_data; const threadnumb: Cardinal): Float; stdcall;
  external FANN_DLL_FILE name '_fann_train_epoch_quickprop_parallel@12';

function fann_train_epoch_sarprop_parallel(ann: pfann; data: pfann_train_data; const threadnumb: Cardinal): Float; stdcall;
  external FANN_DLL_FILE name '_fann_train_epoch_sarprop_parallel@12';

function fann_train_epoch_incremental_mod(ann: pfann; data: pfann_train_data): Float; stdcall;
  external FANN_DLL_FILE name '_fann_train_epoch_incremental_mod@8';

{$ENDIF}

implementation

end.
