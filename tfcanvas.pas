unit TFCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, Crt, math;

type

  { TTextCanvas }

  TTextCanvas = class
  private
    FWidth, FHeight: integer;
    FGraphic: TPrintMap;
    FColor: TPrintColor;
    function GetLineChanged(Y: Integer): Boolean;
    function GetObj(x, y: Integer): TPrintObject;
    procedure SetHeight(AValue: integer);
    procedure SetObj(x, y: Integer; AValue: TPrintObject);
    procedure SetWidth(AValue: integer);
    procedure MergePO(X, Y: Integer; Obj: TPrintObject);
  public
    procedure Draw(X, Y: integer; Graphic: TPrintMap);
    procedure TextOut(X, Y: integer; Str: string);
    procedure Rectangle(X, Y, Width, Heigth: integer);
    procedure Ellipsis(X, Y, Width, Height: integer);
    procedure Line(X, Y, X2, Y2: integer);
    constructor Create;
    procedure Resize(Width, Height: integer);
    procedure SetColor(Foreground, Background: byte);
    procedure Clear;

    property Graphic: TPrintMap read FGraphic;
    property Color: TPrintColor read FColor write FColor;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property LineChanged[Y:Integer]: Boolean read GetLineChanged;
    property Obj[x, y: Integer]: TPrintObject read GetObj write SetObj; default;
  end;

const
  Transparency = 255;

implementation

{ TTextCanvas }

procedure TTextCanvas.MergePO(X, Y: Integer; Obj: TPrintObject);
begin
  if Obj.Color.Foregrond < Transparency then
    FGraphic[Y].Line[X].Color.Foregrond := Obj.Color.Foregrond;

  if Obj.Color.Background < Transparency then
    FGraphic[Y].Line[X].Color.Background := Obj.Color.Background;

  if Obj.Value <> ' ' then
    FGraphic[Y].Line[X].Value := Obj.Value;
end;

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

function TTextCanvas.GetObj(x, y: Integer): TPrintObject;
begin
  Result:=FGraphic[y].Line[x];
end;

function TTextCanvas.GetLineChanged(Y: Integer): Boolean;
begin
  Result:=FGraphic[Y].Changed;
end;

procedure TTextCanvas.SetObj(x, y: Integer; AValue: TPrintObject);
begin
  FGraphic[y].Line[x]:=AValue;
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
      FGraphic[i].Line[x].Color.Background := LightGray;
      FGraphic[i].Line[x].Color.Foregrond := LightGray;
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
    newVal.Color := FColor;
    newVal.Value := Str[i];
    MergePO(x + i - 1, Y, newVal);
  end;
end;

procedure TTextCanvas.Rectangle(X, Y, Width, Heigth: integer);

function IfThenPO(Cond: Boolean; A, B: TPrintObject): TPrintObject;
begin
  if Cond then
    Result := A
  else
    Cond := B;
end;

var
  j, k, right, bottom: Integer;
  Pen, Brush: TPrintObject;
begin
  Brush.Value:=' ';
  Brush.Color.Foregrond:=Transparency;
  Brush.Color.Background:=FColor.Background;
  Pen:=Brush;
  if FColor.Foregrond<Transparency then
    Pen.Color.Background:=FColor.Foregrond;

  right:=Min(x+Width, FWidth - 1);
  bottom:=Min(y+Height, FHeight - 1);
  for j:=y to bottom do
    for k:=x to right do
      MergePO(k,j, IfThenPO((j=y) Or (k=x) Or (j=y+Height) or (k=x+Width), Pen, Brush));
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
  FColor.Foregrond:=Green;
  FColor.Foregrond:=LightGray;
  FColor.Background:=LightGray;
end;

procedure TTextCanvas.Resize(Width, Height: integer);
begin
  SetHeight(Height);
  SetWidth(Width);
end;

procedure TTextCanvas.SetColor(Foreground, Background: byte);
begin
  FColor.Foregrond:=Foregrond;
  FColor.Background:=Background;
end;

procedure TTextCanvas.Clear;
var x, y: Integer;
  c: TPrintObject;
begin
  c.Color.Background:=LightGray;
  c.Color.Foregrond:=LightGray;
  c.Value:=' ';
  for y:=0 to FHeight - 1 do
    for x:=0 to FWidth - 1 do
      Obj[x, y] := c;
end;

end.
