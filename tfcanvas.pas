unit TFCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, Math,
  TFTypes in './tftypes.pas';

type

  { TTextCanvas }

  TTextCanvas = class
  private
    FWidth, FHeight: integer;
    FGraphic: TPrintMap;
    FColor: TPrintColor;
    function GetLineChanged(Y: integer): boolean;
    function GetObj(x, y: integer): TPrintObject;
    procedure SetHeight(AValue: integer);
    procedure SetObj(x, y: integer; AValue: TPrintObject);
    procedure SetWidth(AValue: integer);
    procedure MergePO(X, Y: integer; Obj: TPrintObject);
    procedure PrintLine(y:Integer);
  public
    procedure Draw(X, Y: integer; Graphic: TPrintMap);
    procedure TextOut(X, Y: integer; Str: string);
    procedure Rectangle(X, Y, Width, Heigth: integer);
    procedure Ellipsis(X, Y, Width, Height: integer);
    procedure Line(X, Y, X2, Y2: integer);
    constructor Create;
    procedure Resize(Width, Height: integer);
    procedure SetColor(Foreground, Background: TColor);
    procedure Clear;
    procedure Draw(FullRepaint: Boolean = false);

    property Graphic: TPrintMap read FGraphic;
    property Color: TPrintColor read FColor write FColor;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property LineChanged[Y: integer]: boolean read GetLineChanged;
    property Obj[x, y: integer]: TPrintObject read GetObj write SetObj; default;
  end;

const
  {$IfDef Col24}
  Transparency: TColor = (Color: $64000000);
  ResetColor: TColor = (Color: $FF000000);
  NoChange: TColor = (Color: $FE000000);
  {$Else}
  Transparency: TColor = 255;
  ResetColor: TColor = LightGray;
  NoChange: TColor = 254;
  {$EndIf}

function RGB(R, G, B: Byte): TColor;
function PrintColor(FG, BG: TColor): TPrintColor;
implementation

function MergeColor(A, B: TColor): TColor;
begin
  {$IfDef Col24}
  case B.Opc of
    0: Result:=A;
    1..99:
  begin
    Result.Opc:=100;
    Result.R:=trunc((A.R/100)*(100-B.Opc)+(B.R/100)*B.Opc);
    Result.G:=trunc((A.G/100)*(100-B.Opc)+(B.G/100)*B.Opc);
    Result.B:=trunc((A.B/100)*(100-B.Opc)+(B.B/100)*B.Opc);
  end;
  else
    Result:=B;
  end;
  {$Else}
  if B < 16 then
    Result := B
  else
    Result := A;
  {$EndIf}
end;

function RGB(R, G, B: Byte): TColor;
begin
  Result.Opc:=100;
  Result.R:=R;
  Result.G:=G;
  Result.B:=B;
end;

function PrintColor(FG, BG: TColor): TPrintColor;
begin
  Result.Foreground:=FG;
  Result.Background:=BG;
end;

{ TTextCanvas }

procedure TTextCanvas.MergePO(X, Y: integer; Obj: TPrintObject);
begin
  FGraphic[Y].Line[X].Color.Foreground :=
    MergeColor(FGraphic[Y].Line[X].Color.Foreground, Obj.Color.Foreground);
  FGraphic[Y].Line[X].Color.Background :=
    MergeColor(FGraphic[Y].Line[X].Color.Background, Obj.Color.Background);

  if Obj.Value <> ' ' then
    FGraphic[Y].Line[X].Value := Obj.Value;
end;


{$IfDef COL24}

procedure TTextCanvas.PrintLine(y: Integer);
// Buffer Array type
type TCharBuffer = array [0..255] of Char;

// Flush buffer to STDOut
procedure FlushBuffer(Buff: TCharBuffer; Len: Integer);
var outstr:String;
begin
  if Len = 0 then Exit;
  SetLength(outstr, Len);
  Move(Buff[Low(Buff)], outstr[1], Len);
  Write(outstr);
end;

// Write string into buffer, flush if full
procedure WriteBuffer(var Buff: TCharBuffer; const Value: String; var pos: Integer);
var i: Integer;
begin
  for i:=1 to Length(Value) do
  begin
    // Write char by char
    Buff[pos]:=Value[i];
    inc(pos);
    // on full buffer flush to stdOut
    if pos>High(Buff) then
    begin
      pos:=Low(Buff);
      FlushBuffer(Buff, Length(Buff));
    end;
  end;
end;

// Get the string to change the color
function GetColorString(Color: TPrintColor): String;
var FG, BG: String;
begin
  // Background
  with Color.Background do
    if Opc=ResetColor.Opc then // check if reset
      BG := #27'[49m'
    else if opc=NoChange.Opc then // no change required
      BG:=''
    else
      BG := Format('%c[48;2;%d;%d;%d;m',[#27, R, G, B]);

  // Foreground
  with Color.Foreground do
    if Opc=ResetColor.Opc then
      FG := #27'[39m'
    else if opc=NoChange.Opc then
      FG:=''
    else
      FG := Format('%c[38;2;%d;%d;%d;m',[#27, R, G, B]);

  Result:=FG+BG;
end;

var Buff: TCharBuffer;
  c, x: Integer;
begin
  c:=Low(TCharBuffer);
  FillChar(Buff, Length(Buff), ' ');
  GotoXY(1, y + 1);
  for x:=0 to FWidth-1 do
  begin
    WriteBuffer(Buff, GetColorString(Obj[x,y].Color), c);
    WriteBuffer(Buff, Obj[x,y].Value, c);
  end;
  // FLush Buffer
  FlushBuffer(Buff,c-Low(Buff));
  WriteLn('');
end;

{$Else}

procedure TTextCanvas.PrintLine(y: integer);
var
  x: integer;
begin
  GotoXY(1, y + 1);
  for x := 0 to FCanvas.Width - 1 do
  begin
    if FCanvas.FCanvas.Obj[x, y].Color.Foregrond < NoChange then
      TextColor(FCanvas.Obj[x, y].Color.Foregrond);
    if FCanvas.FCanvas.Obj[x, y].Color.Background < NoChange then
      TextBackground(FCanvas.Obj[x, y].Color.Background);
    Write(FCanvas.Obj[x, y].Value);
  end;
  WriteLn('');
end;

{$EndIf}

procedure TTextCanvas.SetHeight(AValue: integer);
var
  w: integer;
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  SetLength(FGraphic, AValue);
  w := FWidth;
  FWidth := 0;
  SetWidth(w);
end;

function TTextCanvas.GetObj(x, y: integer): TPrintObject;
begin
  Result := FGraphic[y].Line[x];
end;

function TTextCanvas.GetLineChanged(Y: integer): boolean;
begin
  Result := FGraphic[Y].Changed;
end;

procedure TTextCanvas.SetObj(x, y: integer; AValue: TPrintObject);
begin
  FGraphic[y].Line[x] := AValue;
end;

procedure TTextCanvas.SetWidth(AValue: integer);
var
  w, i, x: integer;
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  // Change each line
  for i := 0 to Length(FGraphic) - 1 do
  begin
    w := Length(FGraphic[i].Line);
    SetLength(FGraphic[i].Line, AValue);
    // New PO's clear
    for x := w to AValue - 1 do
    begin
      FGraphic[i].Line[x].Value := ' ';
      FGraphic[i].Line[x].Color.Background := ResetColor;
      FGraphic[i].Line[x].Color.Foreground := ResetColor;
    end;
    FGraphic[i].Changed := True;
  end;
end;

procedure TTextCanvas.Draw(X, Y: integer; Graphic: TPrintMap);
var
  j, k: integer;
begin
  for j := 0 to Length(Graphic) - 1 do
  begin
    if j + y >= FHeight then
      break;
    FGraphic[j + y].Changed := True;
    for k := 0 to Length(Graphic[j].Line) - 1 do
    begin
      if k + x >= FWidth then
        break;
      MergePO(k + x, j + y, Graphic[j].Line[k]);
    end;
  end;
end;

procedure TTextCanvas.TextOut(X, Y: integer; Str: string);
var
  i: integer;
  newVal: TPrintObject;
begin
  if y >= FHeight then
    exit;
  for i := 1 to Length(Str) do
  begin
    if x + i > FWidth then
      break;
    if i=1 then
    newVal.Color := FColor
    else
      newVal.Color:= PrintColor(NoChange, NoChange);
    newVal.Value := Str[i];
    MergePO(x + i - 1, Y, newVal);
  end;
end;

procedure TTextCanvas.Rectangle(X, Y, Width, Heigth: integer);

  function IfThenPO(Cond: boolean; A, B: TPrintObject): TPrintObject;
  begin
    if Cond then
      Result := A
    else
      Result := B;
  end;

var
  j, k, right, bottom: integer;
  Pen, Brush: TPrintObject;
begin
  Brush.Value := ' ';
  Brush.Color.Foreground := Transparency;
  Brush.Color.Background := FColor.Background;
  Pen := Brush;
    Pen.Color.Background :=
    MergeColor(Pen.Color.Background, FColor.Foreground);

  right := Min(x + Width, FWidth - 1);
  bottom := Min(y + Height, FHeight - 1);
  for j := y to bottom do
    for k := x to right do
      MergePO(k, j, IfThenPO((j = y) or (k = x) or (j = y + Height) or (k = x + Width), Pen, Brush));
end;

procedure TTextCanvas.Ellipsis(X, Y, Width, Height: integer);
begin
  // TODO
end;

procedure TTextCanvas.Line(X, Y, X2, Y2: integer);
begin
  //TODO
end;

constructor TTextCanvas.Create;
begin
  FColor.Foreground := NoChange;
  FColor.Background := NoChange;
end;

procedure TTextCanvas.Resize(Width, Height: integer);
begin
  SetHeight(Height);
  SetWidth(Width);
end;

procedure TTextCanvas.SetColor(Foreground, Background: TColor);
begin
  FColor.Foreground := Foreground;
  FColor.Background := Background;
end;

procedure TTextCanvas.Clear;
var
  x, y: integer;
  c: TPrintObject;
begin
  c.Color.Background := ResetColor;
  c.Color.Foreground := ResetColor;
  c.Value := ' ';
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
      Obj[x, y] := c;
end;

procedure TTextCanvas.Draw(FullRepaint: Boolean);
var
  y: integer;
begin
  for y := 0 to FHeight - 1 do
  begin
    if not (FullRepaint or LineChanged[Y]) then
      Continue;
    PrintLine(y);
  end;
  GotoXY(FWidth, FHeight);
  WriteLn('');
end;

end.
