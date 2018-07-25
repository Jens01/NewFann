unit fann.Graph;

interface

{ .$DEFINE GR32! }

uses
  System.Types, System.SysUtils, Vcl.Graphics,
{$IFDEF GR32!}
  GR32, Grafik.LinesAndText,
{$ELSE}
{$ENDIF}
  fann.DelphiApi;

type
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
    procedure ClearCanvas;
    procedure DrawLine(P1: TPointF; P2: TPointF; C: TColor; LW: Single);
    function Gray(Intensity: Byte): TColor;
    procedure DrawCon(GP: TFannGraphPositions; ann: TFannclass; LineWidth: Single);
    procedure DrawText(GP: TFannGraphPositions; Pos: TPointF; Text: string);
    procedure DrawNeuron(Pos: TPointF; IsBias: Boolean);
  public
    constructor Create(Canvas: TCanvas; Width, Height: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure Draw(ann: TFannclass; LineWidth: Single);
    property BorderVert: Integer read FBorderVert write FBorderVert;
    property BorderHori: Integer read FBorderHori write FBorderHori;
  end;

implementation

{ TDrawNeuronGraph }

constructor TDrawNeuronGraph.Create(Canvas: TCanvas; Width, Height: Integer);
begin
  inherited Create;
{$IFDEF GR32!}
  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear(clWhite32);
{$ENDIF}
  FCanvas     := Canvas;
  FWidth      := Width;
  FHeight     := Height;
  FBorderVert := 20;
  FBorderHori := 10;
end;

destructor TDrawNeuronGraph.Destroy;
begin
{$IFDEF GR32!}
  FBitmap.Free;
{$ENDIF}
  inherited;
end;

function TDrawNeuronGraph.Gray(Intensity: Byte): TColor;
begin
  Result := TColor(Intensity) shl 16 + TColor(Intensity) shl 8 + TColor(Intensity);
end;

procedure TDrawNeuronGraph.Draw(ann: TFannclass; LineWidth: Single);
var
  N: TNeuron;
  GP: TFannGraphPositions;
  P: TPointF;
  i, ii: Integer;
begin
  Clear;
  GP := TFannGraphPositions.Create(ann, FWidth - FBorderHori * 2, FHeight - FBorderVert * 2);
  try
    DrawCon(GP, ann, LineWidth);

    for i    := 0 to ann.LayerCount - 1 do
      for ii := 0 to ann.NeuronandBiasCount[i] - 1 do
      begin
        N := ann.Neuron[i, ii];
        P := GP.NeuronPosition(N) + TPointF.Create(0, FBorderVert);

        DrawNeuron(P, N.IsBias);
        DrawText(GP, P, N.NeuronIndx.ToString);
      end;

{$IFDEF GR32!}
    FBitmap.DrawTo(FCanvas.Handle, 0, 0);
{$ENDIF}
  finally
    GP.Free;
  end;
end;

procedure TDrawNeuronGraph.DrawCon(GP: TFannGraphPositions; ann: TFannclass; LineWidth: Single);
var
  Pmov, P1, P2: TPointF;
  N1, N2: TNeuron;
  Con: TConnection;
  i: Integer;
  C: TColor;
begin
  Pmov  := TPointF.Create(0, FBorderVert);
  for i := 0 to ann.ConnectionCount - 1 do
  begin
    Con := ann.Connection[i];
    N1  := Con.FromNeuron;
    N2  := Con.ToNeuron;
    P1  := GP.NeuronPosition(N1) + Pmov;
    P2  := GP.NeuronPosition(N2) + Pmov;
    if Abs(Con.Weight) > 40 then
      C := clBlack
    else
      C := Gray(125 + 3 * Round(Con.Weight));
    DrawLine(P1, P2, C, LineWidth);
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
begin
  L := TLine.Create(FBitmap);
  try
    L.AddCircle(Pos.X, Pos.Y, CircleWidth / 2);
    L.DrawArea(1, clBlack32, clWhite32);
  finally
    L.Free;
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

{$ENDIF}

end.
