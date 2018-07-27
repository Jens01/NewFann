// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

unit fann.Graph;

interface

{.$DEFINE GR32! }

uses
  System.Types, System.SysUtils, Vcl.Graphics, System.Generics.Collections,
{$IFDEF GR32!}
  GR32, Grafik.LinesAndText,
{$ENDIF}
  fann.DelphiApi;

type

  TFannGraphPositions = class
  strict private
    FPixelWidth: Integer;
    FPixelHeight: Integer;
    FDrawNeurons: TNeuronList;
    FLayerCount: TArray<Integer>;
    function PosY(N: TNeuron): Single;
    function PosX(N: TNeuron): Single;
    function NeuronXDistance: Single;
    function NeuronYDistance: Single;
  public
    constructor Create(PixelWidth, PixelHeight: Integer; DrawNeurons: TArray<TNeuron>);
    destructor Destroy; override;
    function NeuronPosition(N: TNeuron): TPointF;
    function NeuronOfPoint(P: TPointF; CircleWidth: Integer; var Neuron: TNeuron): Boolean;
  end;

  TDrawNeuronGraph = class
  private const
    CircleWidth = 20;
  strict private
    FCanvas: TCanvas;
{$IFDEF GR32!}
    FBitmap: TBitmap32;
{$ENDIF}
    FWidth, FHeight: Integer;
    FBorderVert: Integer;
    FBorderHori: Integer;
    FIsDrawWeigths: Boolean;
    FIsDrawNeurons: Boolean;
    FIsDrawCons: Boolean;
    FNeurons: TNeuronList;
    FCons: TList<TConnection>;
    procedure ClearCanvas;
    procedure DrawLine(P1: TPointF; P2: TPointF; C: TColor; LW: Single);
    function Gray(Intensity: Byte): TColor;
    procedure DrawWeights(GP: TFannGraphPositions);
    procedure DrawCon(GP: TFannGraphPositions; LineWidth: Single);
    procedure DrawNeurons(GP: TFannGraphPositions);
    procedure DrawText(GP: TFannGraphPositions; Pos: TPointF; Text: string);
    procedure DrawNeuron(Pos: TPointF; IsBias: Boolean);
    procedure DrawWeight(Pos: TPointF; Weight: Single);
  public
    constructor Create(Canvas: TCanvas; Width, Height: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure SetNeurons(Neurons: TArray<TNeuron>);
    procedure SetConnections(Cons: TArray<TConnection>);
    procedure Draw(LineWidth: Single);
    function NeuronOfPoint(P: TPoint; var Neuron: TNeuron): Boolean;
    function ConOfPoint(P: TPoint; Radius: Single; var Con: TConnection): Boolean;
    property BorderVert: Integer read FBorderVert write FBorderVert;
    property BorderHori: Integer read FBorderHori write FBorderHori;
    property IsDrawWeigths: Boolean read FIsDrawWeigths write FIsDrawWeigths;
    property IsDrawCons: Boolean read FIsDrawCons write FIsDrawCons;
    property IsDrawNeurons: Boolean read FIsDrawNeurons write FIsDrawNeurons;
  end;

implementation

function IntersecCircleLine(LinePoint1, LinePoint2, Circle: TPointF; Radius: Single): Boolean; inline;
var
  D, l, r: Single;
  d1, d2: TPointF;
begin
  d1     := LinePoint1 - Circle;
  d2     := LinePoint2 - Circle;
  l      := (d2 - d1).Length;
  D      := d1.CrossProduct(d2);
  r      := Radius * Radius * l - D * D;
  Result := r >= 0;
end;

{ TDrawNeuronGraph }

constructor TDrawNeuronGraph.Create(Canvas: TCanvas; Width, Height: Integer);
begin
  inherited Create;
{$IFDEF GR32!}
  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear(clWhite32);
{$ENDIF}
  FNeurons      := TNeuronList.Create;
  FCons          := TList<TConnection>.Create;
  FCanvas        := Canvas;
  FWidth         := Width;
  FHeight        := Height;
  FBorderVert    := 20;
  FBorderHori    := 10;
  FIsDrawWeigths := False;
  FIsDrawCons    := True;
  FIsDrawNeurons := True;
end;

destructor TDrawNeuronGraph.Destroy;
begin
  FCons.Free;
  FNeurons.Free;
{$IFDEF GR32!}
  FBitmap.Free;
{$ENDIF}
  inherited;
end;

function TDrawNeuronGraph.Gray(Intensity: Byte): TColor;
begin
  Result := TColor(Intensity) shl 16 + TColor(Intensity) shl 8 + TColor(Intensity);
end;

function TDrawNeuronGraph.NeuronOfPoint(P: TPoint; var Neuron: TNeuron): Boolean;
var
  N: TNeuron;
  GP: TFannGraphPositions;
begin
  P  := P - TPoint.Create(0, FBorderVert);
  GP := TFannGraphPositions.Create(FWidth - FBorderHori * 2, FHeight - FBorderVert * 2, FNeurons.ToArray);
  try
    Result := GP.NeuronOfPoint(P, CircleWidth, N);
    if Result then
      Neuron := N;
  finally
    GP.Free;
  end;
end;

procedure TDrawNeuronGraph.DrawNeurons(GP: TFannGraphPositions);
var
  iNeuron: TNeuron;
  P: TPointF;
begin
  for iNeuron in FNeurons do
  begin
    P := GP.NeuronPosition(iNeuron) + TPointF.Create(0, FBorderVert);
    DrawNeuron(P, iNeuron.IsBias);
    DrawText(GP, P, iNeuron.NeuronIndx.ToString);
  end;
end;

procedure TDrawNeuronGraph.SetConnections(Cons: TArray<TConnection>);
begin
  FCons.Clear;
  FCons.AddRange(Cons);
end;

procedure TDrawNeuronGraph.SetNeurons(Neurons: TArray<TNeuron>);
begin
  FNeurons.Clear;
  FNeurons.AddRange(Neurons);
end;

procedure TDrawNeuronGraph.Draw(LineWidth: Single);
var
  GP: TFannGraphPositions;
begin
  Clear;
  GP := TFannGraphPositions.Create(FWidth - FBorderHori * 2, FHeight - FBorderVert * 2, FNeurons.ToArray);
  try
    if FIsDrawCons then
      DrawCon(GP, LineWidth);
    if FIsDrawWeigths then
      DrawWeights(GP);
    if FIsDrawNeurons then
      DrawNeurons(GP);
  finally
    GP.Free;
  end;
{$IFDEF GR32!}
  FBitmap.DrawTo(FCanvas.Handle, 0, 0);
{$ENDIF}
end;

procedure TDrawNeuronGraph.DrawWeights(GP: TFannGraphPositions);
var
  Pmov, P1, P2: TPointF;
  P: TPointF;
  N1, N2: TNeuron;
  iCon: TConnection;
begin
  Pmov := TPointF.Create(0, FBorderVert);
  for iCon in FCons do
  begin
    N1 := iCon.FromNeuron;
    N2 := iCon.ToNeuron;
    if FNeurons.Contains(N1) and FNeurons.Contains(N2) then
    begin
      P1 := GP.NeuronPosition(N1) + Pmov;
      P2 := GP.NeuronPosition(N2) + Pmov;

      if Odd(iCon.FromNeuron.NeuronIndx) then
        P := (P2 - P1) * 0.45
      else
        P := (P2 - P1) * 0.55;
      P   := P + P1;
      DrawWeight(P, iCon.Weight);
    end;
  end;
end;

procedure TDrawNeuronGraph.DrawCon(GP: TFannGraphPositions; LineWidth: Single);
var
  Pmov, P1, P2: TPointF;
  N1, N2: TNeuron;
  iCon: TConnection;
  C: TColor;
begin
  Pmov := TPointF.Create(0, FBorderVert);
  for iCon in FCons do
  begin
    N1 := iCon.FromNeuron;
    N2 := iCon.ToNeuron;
    if FNeurons.Contains(N1) and FNeurons.Contains(N2) then
    begin
      P1 := GP.NeuronPosition(N1) + Pmov;
      P2 := GP.NeuronPosition(N2) + Pmov;
      if Abs(iCon.Weight) > 40 then
        C := clBlack
      else
        C := Gray(125 + 3 * Round(iCon.Weight));
      DrawLine(P1, P2, C, LineWidth);
    end;
  end;
end;

procedure TDrawNeuronGraph.ClearCanvas;
var
  R: TRect;
begin
  R                   := TRect.Create(TPoint.Create(0, 0), FWidth, FHeight);
  Fcanvas.brush.color := clWhite;
  Fcanvas.fillrect(R);
end;

function TDrawNeuronGraph.ConOfPoint(P: TPoint; Radius: Single; var Con: TConnection): Boolean;
var
  iCon: TConnection;
  N1, N2: TNeuron;
  P1, P2: TPointF;
  GP: TFannGraphPositions;
begin
  P  := P - TPoint.Create(0, FBorderVert);
  GP := TFannGraphPositions.Create(FWidth - FBorderHori * 2, FHeight - FBorderVert * 2, FNeurons.ToArray);
  try
    for iCon in FCons do
    begin
      N1 := iCon.FromNeuron;
      N2 := iCon.ToNeuron;
      if FNeurons.Contains(N1) and FNeurons.Contains(N2) then
      begin
        P1 := GP.NeuronPosition(N1);
        P2 := GP.NeuronPosition(N2);
        if IntersecCircleLine(P1, P2, P, Radius) then
        begin
          Con := iCon;
          Exit(True);
        end;
      end;
    end;
  finally
    GP.Free;
  end;
  Result := False;
end;

procedure TDrawNeuronGraph.Clear;
begin
{$IFDEF GR32!}
  FBitmap.Clear(clWhite32);
{$ENDIF}
  ClearCanvas;
end;

{$IFDEF GR32!}

procedure TDrawNeuronGraph.DrawLine(P1: TPointF; P2: TPointF; C: TColor; LW: Single);
var
  L: TLine;
begin
  L := TLine.Create(FBitmap);
  try
    L.AddPoints(P1);
    L.AddPoints(P2);
    L.DrawLine(LW, Color32(C));
  finally
    L.Free;
  end;
end;

procedure TDrawNeuronGraph.DrawText(GP: TFannGraphPositions; Pos: TPointF; Text: string);
var
  T: TText;
  H, W: Single;
begin
  T := TText.Create(FBitmap);
  try
    T.Text := Text;
    H      := T.TextHeight;
    W      := T.TextWidth;
    T.DrawText(Pos.X - W / 2, Pos.Y + H / 4);
  finally
    T.Free;
  end;
end;

procedure TDrawNeuronGraph.DrawNeuron(Pos: TPointF; IsBias: Boolean);
var
  L: TLine;
  R: TRectF;
  P: TPointF;
begin
  L := TLine.Create(FBitmap);
  try
    if IsBias then
    begin
      P := Pos + TPointF.Create(-CircleWidth / 2, -CircleWidth / 2);
      R := TRectF.Create(P, CircleWidth, CircleWidth);
      L.AddRect(R.Round);
    end
    else
      L.AddCircle(Pos.X, Pos.Y, CircleWidth / 2);

    L.DrawArea(1, clBlack32, clWhite32);
  finally
    L.Free;
  end;
end;

procedure TDrawNeuronGraph.DrawWeight(Pos: TPointF; Weight: Single);
var
  T: TText;
  H, W: Single;
begin
  T := TText.Create(FBitmap);
  try
    T.Text := Format('%.1f', [Weight]);
    H      := T.TextHeight;
    W      := T.TextWidth;
    T.DrawText(Pos.X - W / 2, Pos.Y + H / 4);
  finally
    T.Free;
  end;
end;

{$ELSE}

procedure TDrawNeuronGraph.DrawLine(P1: TPointF; P2: TPointF; C: TColor; LW: Single);
var
  _LW: Integer;
begin
  _LW := FCanvas.Pen.Width;
  try
    FCanvas.Pen.Width := Round(LW);
    FCanvas.Pen.Color := C;
    FCanvas.Polyline([P1.Round, P2.Round]);
  finally
    FCanvas.Pen.Width := _LW;
  end;
end;

procedure TDrawNeuronGraph.DrawNeuron(Pos: TPointF; IsBias: Boolean);
var
  R: TRect;
  P1: TPoint;
begin
  P1 := Pos.Round + TPoint.Create(-CircleWidth div 2, -CircleWidth div 2);
  R  := TRect.Create(P1, CircleWidth, CircleWidth);

  FCanvas.Brush.Style := bsSolid;
  // FCanvas.Brush.Style := bsClear;
  if IsBias then
    FCanvas.Rectangle(R)
  else
    FCanvas.Ellipse(R);
end;

procedure TDrawNeuronGraph.DrawText(GP: TFannGraphPositions; Pos: TPointF; Text: string);
var
  H, W: Integer;
begin
  H := FCanvas.TextHeight('0');
  W := FCanvas.TextWidth('0');
  FCanvas.TextOut(Pos.Round.X - W div 2, Pos.Round.Y - H div 2, Text);
end;

procedure TDrawNeuronGraph.DrawWeight(Pos: TPointF; Weight: Single);
var
  H, W: Integer;
  Text: string;
begin
  Text := Format('%.1f', [Weight]);
  H    := FCanvas.TextHeight(Text);
  W    := FCanvas.TextWidth(Text);
  // FCanvas.Brush.Style := bsClear;
  FCanvas.TextOut(Pos.Round.X - W div 2, Pos.Round.Y - H div 2, Text);
end;

{$ENDIF}
{ TFannGraphPositions }

constructor TFannGraphPositions.Create(PixelWidth, PixelHeight: Integer; DrawNeurons: TArray<TNeuron>);
begin
  inherited Create;
  FDrawNeurons := TNeuronList.Create;
  FDrawNeurons.AddRange(DrawNeurons);
  FDrawNeurons.Sort;
  FLayerCount  := FDrawNeurons.LayerCounts;
  FPixelWidth  := PixelWidth;
  FPixelHeight := PixelHeight;
end;

destructor TFannGraphPositions.Destroy;
begin
  FDrawNeurons.Free;
  inherited;
end;

function TFannGraphPositions.NeuronOfPoint(P: TPointF; CircleWidth: Integer; var Neuron: TNeuron): Boolean;
var
  iNeuron: TNeuron;
  P1: TPointF;
begin
  for iNeuron in FDrawNeurons do
  begin
    P1 := NeuronPosition(iNeuron);
    if TPointF.PointInCircle(P, P1, CircleWidth div 2) then
    begin
      Neuron := iNeuron;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TFannGraphPositions.NeuronPosition(N: TNeuron): TPointF;
begin
  Result := TPointF.Create(PosX(N), PosY(N));
end;

function TFannGraphPositions.NeuronXDistance: Single;
begin
  Result := FPixelWidth / FDrawNeurons.NeuronCountMax;
end;

function TFannGraphPositions.NeuronYDistance: Single;
begin
  Result := FPixelHeight / (Length(FDrawNeurons.LayerCounts) - 1)
end;

function TFannGraphPositions.PosX(N: TNeuron): Single;
var
  C: Integer;
  P: Single;
begin
  C := FDrawNeurons.NeuronCount[N.LayerIndx];
  if Odd(C) then
    P := N.NeuronIndx - (C - 1) / 2
  else
    P := N.NeuronIndx - C / 2 + 0.5;

  Result := FPixelWidth / 2 + P * NeuronXDistance;
end;

function TFannGraphPositions.PosY(N: TNeuron): Single;
begin
  Result := NeuronYDistance * FDrawNeurons.CleanedLayerIndex[N.LayerIndx];
end;

end.
