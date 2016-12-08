unit TFCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  TFTypes in './tftypes.pas'
  {$IfNDef Col24}
  , crt
  {$EndIf}
  {$IfDef UNIX}
  , BaseUnix, termio
  {$Else}
  , Windows
  {$EndIf}  ;

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
    procedure PrintLine(y: integer);
  public
    procedure Draw(X, Y: integer; Graphic: TPrintMap);
    procedure TextOut(X, Y: integer; Str: string);
    procedure Rectangle(X, Y, Width, Height: integer);
    procedure Ellipsis(X, Y, Width, Height: integer);
    procedure Line(X, Y, X2, Y2: integer);
    constructor Create;
    procedure Resize(Width, Height: integer);
    procedure SetColor(Foreground, Background: TColor);
    procedure Clear;
    procedure ClearLine(Ln: integer);
    procedure Print(FullRepaint: boolean = False);

    property Graphic: TPrintMap read FGraphic;
    property Color: TPrintColor read FColor write FColor;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property LineChanged[Y: integer]: boolean read GetLineChanged;
    property Obj[x, y: integer]: TPrintObject read GetObj write SetObj; default;
  end;

const
  {$IfDef Col24}
  Transparency: TColor = (Color: $00000000);
  ResetFGColor: TColor = (Color: Integer($FF000000));
  ResetBGColor: TColor = (Color: Integer($FF000000));
  {$Else}
  Transparency: TColor = 255;
  ResetFGColor: TColor = LightGray;
  ResetBGColor: TColor = Black;
  {$EndIf}
	MaxBuffSize = 256;

  {$IfDef Col24}
function RGB(R, G, B: byte): TColor;
  {$EndIf}
function PrintColor(FG, BG: TColor): TPrintColor;
procedure MoveCursor(X, Y: integer); inline;
procedure ClearScreen(); inline;
procedure SetCursorVisibility(Visible: boolean);
function GetWindowSize: TWindowSize;
// Reads a char without the need of enter
function ReadChar(Blocking: Boolean = True): Char;

implementation

{$IfDef UNIX}

// found at https://ubuntuforums.org/showthread.php?t=554845
  function ReadChar(Blocking: Boolean = True): Char;
  var
    org_opts, new_opts: Termios;
    {$IfDef NonBlockingStdIn}
    flags,
    {$EndIf}
    res: Integer;
  begin
    if not Blocking then
    begin
    {$ifDef NonBlockingStdIn}
      flags:=FpFcntl(StdInputHandle, F_GetFl, 0);
      FpFcntl(StdInputHandle, F_SetFl, flags or O_NONBLOCK);
    {$Else}
    FpIOCtl(StdInputHandle, FIONREAD, @res);
    if res = 0 then exit;
    {$EndIf}
    end;
    //-----  store old settings -----------
    TCGetAttr(StdInputHandle, org_opts);
    //---- set new terminal parms --------
    Move(org_opts, new_opts, sizeof(new_opts));
    new_opts.c_lflag := new_opts.c_lflag or not (ICANON or ECHO or
      ECHOE or ECHOK or ECHONL or ECHOPRT or ECHOKE or ICRNL);
    //new_opts.c_lflag := new_opts.c_lflag And not ICANON;
    TCSetAttr(StdInputHandle, TCSANOW, new_opts);
    res := FpRead(StdInputHandle, Result, 1);
    if res = 0 then Result:=#0;

    //------  restore old settings ---------
    TCSetAttr(StdInputHandle, TCSANOW, org_opts);
    {$ifDef NonBlockingStdIn}
    if not Blocking then
    FpFcntl(StdInputHandle, F_SetFl, flags);
    {$EndIf}
  end;
{$Else}

   // found at http://www.cplusplus.com/forum/articles/19975/
  function ReadChar(Blocking: Boolean = True): Char;
  var
    hstdin: HANDLE;
    irInputRecord: INPUT_RECORD;
    dwEventsRead: DWORD;
    r: DWORD;
  begin
    hStdin := GetStdHandle(STD_INPUT_HANDLE);
    GetNumberOfConsoleInputEvents(hstdin, r);
    if Blocking or (r>0) then
    while ReadConsoleInputA(hStdin, irInputRecord, 1, dwEventsRead) do
      if (irInputRecord.EventType = KEY_EVENT) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_MENU) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) then
      begin
        Result := irInputRecord.Event.KeyEvent.AsciiChar;
        ReadConsoleInputA(hStdin, irInputRecord, 1, dwEventsRead);
        Exit;
      end
      else if not Blocking then break;
    Result := #0;
  end;

{$EndIf}

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

  {$IfDef Col24}
function RGB(R, G, B: byte): TColor;
begin
  Result.Opc := 100;
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;
  {$EndIf}

function PrintColor(FG, BG: TColor): TPrintColor;
begin
  Result.Foreground := FG;
  Result.Background := BG;
end;

procedure MoveCursor(X, Y: integer);
begin
  {$IfDef Col24}
  Write(Format(#27'[%d;%dH', [Y, X]));
  {$Else}
  GotoXY(X, Y);
  {$EndIf}
end;

procedure ClearScreen;
begin
  {$IfDef Col24}
  MoveCursor(1, 1);
  Write(#27'[39;49m'#27'[2J');
  {$Else}
  ClrScr;
  {$EndIf}
end;

procedure SetCursorVisibility(Visible: boolean);
begin
  if Visible then
  {$IfDef Col24}
  Write(#27'[?25h')
  {$Else}
    cursoron
  {$EndIf}
  else
  {$IfDef Col24}
  Write(#27'[?25l');
  {$Else}
    cursoroff;
  {$EndIf}
end;

  {$IfDef UNIX}
function GetWindowSize: TWindowSize;
var sz: TWinSize;
begin
  FpIOCtl(StdOutputHandle, TIOCGWINSZ, @sz);
  Result.Width:=sz.ws_col;
  Result.Height:=sz.ws_row;
end;
  {$Else}

function GetWindowSize: TWindowSize;
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  FillChar(csbi, SizeOf(csbi), 0);
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), csbi);
  Result.Width := csbi.srWindow.Right - csbi.srWindow.Left + 1;
  Result.Height := csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
end;

  {$EndIf}

function InEllipses(const px, py, cx, cy, rx, ry: integer): boolean; inline;
begin
  Result := power((px - cx) / rx, 2) + power((py - cy) / ry, 2) <= 1;
end;

function IfThenPO(Cond: boolean; A, B: TPrintObject): TPrintObject;
begin
  if Cond then
    Result := A
  else
    Result := B;
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


{$IfDef Col24}

procedure TTextCanvas.PrintLine(y: Integer);
// Buffer Array type
type TCharBuffer = array [0..MaxBuffSize - 1] of Char;

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
    if Opc=ResetBGColor.Opc then // check if reset
      BG := #27'[49m'
    else
      BG := Format(#27'[48;2;%d;%d;%dm',[R, G, B]);

  // Foreground
  with Color.Foreground do
    if Opc=ResetFGColor.Opc then
      FG := #27'[39m'
    else
      FG := Format(#27'[38;2;%d;%d;%dm',[R, G, B]);

  Result:=FG+BG;
end;

var Buff: TCharBuffer;
  c, x: Integer;
begin
  c:=Low(TCharBuffer);
  FillChar(Buff, Length(Buff), ' ');
  MoveCursor(1, y + 1);
  for x:=0 to FWidth-1 do
  begin
    WriteBuffer(Buff, GetColorString(Obj[x,y].Color), c);
    WriteBuffer(Buff, Obj[x,y].Value, c);
  end;
  // Flush Buffer
  FlushBuffer(Buff,c-Low(Buff));
  WriteLn('');
  FGraphic[y].Changed:=False;
end;

{$Else}

procedure TTextCanvas.PrintLine(y: integer);
var
  x: integer;
begin
  GotoXY(1, y + 1);
  for x := 0 to Width - 1 do
  begin
    if Obj[x, y].Color.Foreground < Transparency then
      TextColor(Obj[x, y].Color.Foreground);
    if Obj[x, y].Color.Background < Transparency then
      TextBackground(Obj[x, y].Color.Background);
    Write(Obj[x, y].Value);
  end;
  WriteLn('');
  FGraphic[y].Changed := False;
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
  FGraphic[y].Changed := True;
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
      FGraphic[i].Line[x].Color.Background := ResetBGColor;
      FGraphic[i].Line[x].Color.Foreground := ResetFGColor;
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
      break
    else if j + y < 0 then
      Continue;
    FGraphic[j + y].Changed := True;
    for k := 0 to Length(Graphic[j].Line) - 1 do
    begin
      if k + x >= FWidth then
        break
      else if k + x < 0 then
        Continue;
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
  newVal.Color := FColor;
  for i := 1 to Length(Str) do
  begin
    if x + i > FWidth then
      break;
    newVal.Value := Str[i];
    MergePO(x + i - 1, Y, newVal);
  end;
  FGraphic[Y].Changed := True;
end;

procedure TTextCanvas.Rectangle(X, Y, Width, Height: integer);
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

  right := Min(x + Width - 1, FWidth - 1);
  bottom := Min(y + Height - 1, FHeight - 1);
  for j := Max(y, 0) to bottom do
  begin
    for k := Max(x, 0) to right do
      MergePO(k, j, IfThenPO((j = y) or (k <= x + 1) or (j = bottom) or
        (k >= right - 1), Pen, Brush));
    FGraphic[j].Changed := True;
  end;
end;

procedure TTextCanvas.Ellipsis(X, Y, Width, Height: integer);
var
  cx, cy, rx, ry, right, bottom, k, j: integer;
  Pen, Brush: TPrintObject;
begin
  rx := (Width - 1) div 2;
  ry := (Height) div 2;
  cx := X + rx;
  cy := y + ry;

  Brush.Value := ' ';
  Brush.Color.Foreground := Transparency;
  Brush.Color.Background := FColor.Background;
  Pen := Brush;
  Pen.Color.Background :=
    MergeColor(Pen.Color.Background, FColor.Foreground);

  right := Min(x + Width - 1, FWidth - 1);
  bottom := Min(y + Height - 1, FHeight - 1);

  for j := Max(y, 0) to bottom do
  begin
    for k := Max(x, 0) to right do
      if InEllipses(k, j, cx, cy, rx, ry) then
        MergePO(k, j, IfThenPO(InEllipses(k + 1, j, cx, cy, rx, ry) and
          InEllipses(k - 1, j, cx, cy, rx, ry) and InEllipses(k + 2,
          j, cx, cy, rx, ry) and InEllipses(k - 2, j, cx, cy, rx, ry) and
          InEllipses(k, j + 1, cx, cy, rx, ry) and InEllipses(k, j - 1, cx, cy, rx, ry),
          Brush, Pen));
    FGraphic[j].Changed := True;
  end;
end;

procedure TTextCanvas.Line(X, Y, X2, Y2: integer);
var
  m: double;
  j, k, lx, rx, ly, ry, right, n: integer;
  Pen: TPrintObject;
begin
  lx := ifthen(X < X2, X, X2);
  rx := ifthen(X < X2, X2, X);
  lY := ifthen(Y < Y2, Y, Y2);
  rY := ifthen(Y < Y2, Y2, Y);

  m := (ry - ly) / (rx - lx);
  n := -trunc(X * m - Y);

  Pen.Value := ' ';
  Pen.Color.Foreground := Transparency;
  Pen.Color.Background := FColor.Background;

  right := Min(rx, FWidth - 1);
  for k := Max(lx, 0) to right do
  begin
    j := trunc(k * m + n);
    if (j > Min(ry, FHeight - 1)) or (j < Max(ly, 0)) then
      Continue;
    MergePO(k, j, Pen);
    FGraphic[j].Changed := True;
  end;

end;

constructor TTextCanvas.Create;
begin
  FColor.Foreground := ResetFGColor;
  FColor.Background := ResetBGColor;
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
  c.Color.Background := ResetBGColor;
  c.Color.Foreground := ResetFGColor;
  c.Value := ' ';
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
      FGraphic[y].Line[x] := c;
    FGraphic[Y].Changed := True;
  end;
end;

procedure TTextCanvas.ClearLine(Ln: integer);
var
  x: integer;
begin
  FGraphic[Ln].Changed := True;
  for x := 0 to FWidth - 1 do
  begin
    FGraphic[Ln].Line[x].Value := ' ';
    FGraphic[Ln].Line[x].Color := PrintColor(ResetFGColor, ResetBGColor);
  end;
end;

procedure TTextCanvas.Print(FullRepaint: boolean);
var
  y: integer;
begin
  for y := 0 to FHeight - 1 do
  begin
    if not (FullRepaint or LineChanged[Y]) then
      Continue;
    PrintLine(y);
  end;
  MoveCursor(FWidth, FHeight);
  WriteLn('');
end;

end.
