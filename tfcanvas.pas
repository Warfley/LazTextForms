{ TODO : Utf-8 Support }
unit TFCanvas;

{$Include defines.inc}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  TFTypes
  {$IfDef Col4}
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


type
  TArrowKey = (akNone, akLeft, akUp, akDown, akRight);

const
  {$IfDef Col24}
  Transparency: TColor = (Color: $00000000);
  ResetFGColor: TColor = (Color: integer($FF000000));
  ResetBGColor: TColor = (Color: integer($FF000000));
  {$Else}
    {$IfDef Col8}
  Transparency: TColor = (Color: $0000);
  ResetFGColor: TColor = (Color: integer($FF00));
  ResetBGColor: TColor = (Color: integer($FF00));
    {$Else}
  Transparency: TColor = 255;
  ResetFGColor: TColor = LightGray;
  ResetBGColor: TColor = Black;
    {$EndIf}
  {$EndIf}
  MaxBuffSize = 1024;

function RGB(R, G, B: byte): TColor;
function PrintColor(FG, BG: TColor): TPrintColor;
procedure MoveCursor(X, Y: integer); inline;
procedure ClearScreen(); inline;
procedure SetCursorVisibility(Visible: boolean);
function GetWindowSize: TWindowSize;
// Reads a char without the need of enter
function ReadChar(Blocking: boolean = True): char;
function LCLColToCanvasColor(Col: cardinal): TColor;
function GetArrow(seq: string): TArrowKey;
function ReadSequence(Blocking: boolean = True): string;

{$If defined(COL8) or defined(Col4)}
function FindTableIndex(C: cardinal; StartIndex: integer = 0): integer;

const
  ColTable: array[0..255] of cardinal = ({$Include table.inc});
{$EndIf}

implementation

function GetArrow(seq: string): TArrowKey;
begin
  Result := akNone;
  if seq.Length = 0 then exit;
  case seq[1] of
    #27:
      if seq.length = 3 then
        case seq[3] of
          'A': Result := akUp;
          'B': Result := akDown;
          'C': Result := akRight;
          'D': Result := akLeft;
        end;
  end;
end;

function ReadSequence(Blocking: boolean = True): string;
var
  c: char;
  l: integer;
begin
  SetLength(Result, 1);
  Result[1] := ReadChar(Blocking);
  if Result[1] = #0 then
  begin
    SetLength(Result, 0);
    Exit;
  end
  else if Result[1] <> #27 then
    exit;
  l := 1;
  repeat
    c := ReadChar(False);
    if c > #0 then
    begin
      Inc(l);
      if l > Result.Length then
        SetLength(Result, Result.Length * 2);
      Result[l] := c;
    end;
  until c = #0;
  SetLength(Result, l);
end;

{$IfDef UNIX}

function ReadChar(Blocking: boolean = True): char;
var
  oTIO, nTIO: Termios;
    {$IfDef NonBlockingStdIn}
  flags,
    {$Else}
  fdsin: tfdSet;
    {$EndIf}
  res: integer;
begin
  res := 1;
  Result := #0;
  TCGetAttr(1, oTIO);
  nTIO := oTIO;
  CFMakeRaw(nTIO);
  TCSetAttr(1, TCSANOW, nTIO);
  if not Blocking then
  begin
    {$ifDef NonBlockingStdIn}
    flags := FpFcntl(StdInputHandle, F_GetFl, 0);
    FpFcntl(StdInputHandle, F_SetFl, flags or O_NONBLOCK);
    {$Else}
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle, fdsin);
    res := fpSelect(StdInputHandle + 1, @fdsin, nil, nil, 0);
    {$EndIf}
  end;
  if res > 0 then
    res := FpRead(StdInputHandle, Result, 1);

  {$ifDef NonBlockingStdIn}
  if res = 0 then
    Result := #0;
  {$EndIf}

  //restore settings
  TCSetAttr(1, TCSANOW, oTIO);
  {$ifDef NonBlockingStdIn}
  if not Blocking then
    FpFcntl(StdInputHandle, F_SetFl, flags);
  {$EndIf}
end;

{$Else}

var
  NextChars: string;

// found at http://www.cplusplus.com/forum/articles/19975/
function ReadChar(Blocking: boolean = True): char;
var
  STDInHandle: HANDLE;
  ConsoleInput: INPUT_RECORD;
  RecordCount: cardinal;
begin
  if NextChars.Length > 0 then
  begin
    Result := NextChars.Chars[0];
    NextChars := NextChars.Substring(1);
    Exit;
  end;
  STDInHandle := GetStdHandle(STD_INPUT_HANDLE);
  GetNumberOfConsoleInputEvents(STDInHandle, RecordCount);
  if Blocking or (RecordCount > 0) then
    while ReadConsoleInputA(STDInHandle, ConsoleInput, 1, RecordCount) do
      if (ConsoleInput.EventType = KEY_EVENT) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_MENU) and
        (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) and
        (ConsoleInput.Event.KeyEvent.bKeyDown) then
      begin
        if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= VK_PRIOR) and
          (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= VK_HOME) then
        begin
          case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of
            VK_HOME: NextChars := '[H';
            VK_END: NextChars := '[F';
            VK_PRIOR: NextChars := '[5~';
            VK_NEXT: NextChars := '[6~';
          end;
          Result := #27;
        end
        else if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= VK_F1) and
          (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= VK_F12) then
        begin
          case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of   // F-Keys
            VK_F1: NextChars := #79'P';
            VK_F2: NextChars := #79'Q';
            VK_F3: NextChars := #79'R';
            VK_F4: NextChars := #79'S';
            VK_F5: NextChars := #91#49#53'~';
            VK_F6: NextChars := #91#49#55'~';
            VK_F7: NextChars := #91#49#56'~';
            VK_F8: NextChars := #91#49#57'~';
            VK_F9: NextChars := #91#50#48'~';
            VK_F10: NextChars := #91#50#49'~';
            VK_F11: NextChars := #91#50#50'~';
            VK_F12: NextChars := #91#50#52'~';
          end;
          Result := #27;
        end
        else if ConsoleInput.Event.KeyEvent.dwControlKeyState and ($8 or $4) <> 0 then
        begin
          if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= Ord('A')) and
            (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= Ord('Z')) then
            Result := chr(ConsoleInput.Event.KeyEvent.wVirtualKeyCode - Ord('A') + 1)
          else if (ConsoleInput.Event.KeyEvent.wVirtualKeyCode >= Ord('3')) and
            (ConsoleInput.Event.KeyEvent.wVirtualKeyCode <= Ord('7')) then
            Result := chr(ConsoleInput.Event.KeyEvent.wVirtualKeyCode - Ord('3') + 27);
        end
        else
          case ConsoleInput.Event.KeyEvent.AsciiChar of
            #8, #27, #13, #32..#254:
              if ConsoleInput.Event.KeyEvent.dwControlKeyState and $2 = $2 then
              begin
                NextChars := ConsoleInput.Event.KeyEvent.AsciiChar;
                Result := #27;
              end
              else
                Result := ConsoleInput.Event.KeyEvent.AsciiChar;
            #9:
            begin
              if ConsoleInput.Event.KeyEvent.dwControlKeyState and $0010 = $0010 then
              begin
                NextChars := '[Z';
                Result := #27;
              end
              else
                Result := #9;
            end;
            #0:
            begin
              // Arrows
              SetLength(NextChars, 2);
              NextChars[1] := '[';
              case ConsoleInput.Event.KeyEvent.wVirtualKeyCode of
                $25: NextChars[2] := 'D';
                $26: NextChars[2] := 'A';
                $27: NextChars[2] := 'C';
                $28: NextChars[2] := 'B';
              end;
              Result := #27;
            end;
          end;
        Exit;
      end
      else
      if not Blocking then
        break;
  Result := #0;
end;

{$EndIf}

function MergeColor(A, B: TColor): TColor;
  {$IfDef Col8}
var
  ra, ga, ba, rb, gb, bb: integer;
  {$EndIf}
begin
  {$IfDef Col24}
  case B.Opc of
    0: Result := A;
    1..99:
    begin
      Result.Opc := 100;
      Result.R := trunc((A.R / 100) * (100 - B.Opc) + (B.R / 100) * B.Opc);
      Result.G := trunc((A.G / 100) * (100 - B.Opc) + (B.G / 100) * B.Opc);
      Result.B := trunc((A.B / 100) * (100 - B.Opc) + (B.B / 100) * B.Opc);
    end;
    else
      Result := B;
  end;
  {$Else}
  {$IfDef Col8}
  case B.Opc of
    0: Result := A;
    1..99:
    begin
      Result.Opc := 100;
      case b.Col of
        16..231:
        begin
          ra := (a.Col - 16) div 36;
          ga := (a.Col - 16) mod 36 div 6;
          ba := (a.Col - 16) mod 6;

          rb := (b.Col - 16) div 36;
          gb := (b.Col - 16) mod 36 div 6;
          bb := (b.Col - 16) mod 6;

          Ra := trunc((Ra / 100) * (100 - B.Opc) + (Rb / 100) * B.Opc);
          Ga := trunc((Ga / 100) * (100 - B.Opc) + (gb / 100) * B.Opc);
          Ba := trunc((Ba / 100) * (100 - B.Opc) + (Bb / 100) * B.Opc);
          Result.Col := 16 * 36 * ra + 6 * ga + ba;
        end;
        232..255: Result.Col :=
            trunc(((A.Col - 232) / 100) * (100 - B.Opc) + ((B.Col - 232) / 100) *
            B.Opc) + 232;
        else
        begin
          Result.Col := ifthen(b.Opc >= 50, b.Col, a.Col);
        end;
      end;
    end
    else
      Result := B;
  end;
  {$Else}
  if B < 16 then
    Result := B
  else
    Result := A;
  {$EndIf}
  {$EndIf}
end;


function LCLColToCanvasColor(Col: cardinal): TColor;
type
  TLazColor = record
    case boolean of
      True: (Color: integer);
    {$IfDef ENDIAN_LITTLE}
      False: (Opc, R, G, B: byte);
    {$Else}
      False: (B, G, R, Opc: byte);
    {$EndIf}
  end;
var
  lc: TLazColor;
begin
  lc.Color := Col;
  Result := RGB(lc.R, lc.G, lc.B);
end;

{$If defined(Col4) or defined(COL8)}
function CompareColors(A, B: cardinal): integer;
var
  C1, C2: TColor24;
begin
  C1.Color := A;
  C2.Color := B;
  Result := (C1.R - C2.R) ** 2 + (C1.G - C2.G) ** 2 + (C1.B - C2.B) ** 2;
end;

function FindTableIndex(C: cardinal; StartIndex: integer = 0): integer;
const
  {$IfDef Col4}
  TableMax = 15;
  {$else}
  TableMax = 255;
  {$EndIf}
var
  i, cmp, MinDiff: integer;
begin
  Result := StartIndex;
  MinDiff := High(integer);
  for i := StartIndex to TableMax do
  begin
    cmp := CompareColors(C, ColTable[i]);
    if cmp < MinDiff then
    begin
      Result := i;
      MinDiff := cmp;
    end;
  end;
end;

{$EndIf}

function RGB(R, G, B: byte): TColor;
{$If defined(Col4) or defined(COL8)}
var
  Ref: TColor24;
{$EndIf}
begin
  {$IfDef Col24}
  Result.Opc := 100;
  Result.R := R;
  Result.G := G;
  Result.B := B;
  {$Else}
  Ref.R := B;
  Ref.G := G;
  Ref.B := R;
  Ref.Opc := 0;
  {$IfDef COL8}
  Result.Opc := 100;
  if (R = B) and (B = G) then // grayscale
    Result.Col := FindTableIndex(Ref.Color, 232)
  else
    Result.Col := 16 + round(R / 255 * 5) * 36 + round(G / 255 * 5) *
      6 + round(B / 255 * 5);
  {$Else}// Col4
  Result := FindTableIndex(Ref.Color);
  {$EndIf}
  {$EndIf}
end;

function PrintColor(FG, BG: TColor): TPrintColor;
begin
  Result.Foreground := FG;
  Result.Background := BG;
end;

procedure MoveCursor(X, Y: integer);
begin
  {$IfDef AnsiEscape}
  Write(Format(#27'[%d;%dH', [Y, X]));
  {$Else}
  GotoXY(X, Y);
  {$EndIf}
end;

procedure ClearScreen;
begin
  {$IfDef AnsiEscape}
  MoveCursor(1, 1);
  Write(#27'[39;49m'#27'[2J');
  {$Else}
  ClrScr;
  {$EndIf}
end;

procedure SetCursorVisibility(Visible: boolean);
begin
  if Visible then
  begin
  {$IfDef AnsiEscape}
    Write(#27'[?25h');
  {$Else}
    cursoron;
  {$EndIf}
  end
  else
  {$IfDef AnsiEscape}
    Write(#27'[?25l');
  {$Else}
  cursoroff;
  {$EndIf}
end;

  {$IfDef UNIX}
function GetWindowSize: TWindowSize;
var
  sz: TWinSize;
begin
  FpIOCtl(StdOutputHandle, TIOCGWINSZ, @sz);
  Result.Width := sz.ws_col;
  Result.Height := sz.ws_row;
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

  FGraphic[Y].Line[X].Value := Obj.Value;
end;


{$IfDef AnsiEscape}

procedure TTextCanvas.PrintLine(y: integer);
// Buffer Array type
type
  TCharBuffer = array [0..MaxBuffSize - 1] of char;

  // Flush buffer to STDOut
  procedure FlushBuffer(Buff: TCharBuffer; Len: integer);
  var
    outstr: string;
  begin
    if Len = 0 then
      Exit;
    SetLength(outstr, Len);
    Move(Buff[Low(Buff)], outstr[1], Len);
    Write(outstr);
  end;

  // Write string into buffer, flush if full
  procedure WriteBuffer(var Buff: TCharBuffer; const Value: string; var pos: integer);
  var
    i: integer;
  begin
    for i := 1 to Length(Value) do
    begin
      // Write char by char
      Buff[pos] := Value[i];
      Inc(pos);
      // on full buffer flush to stdOut
      if pos > High(Buff) then
      begin
        pos := Low(Buff);
        FlushBuffer(Buff, Length(Buff));
      end;
    end;
  end;

  // Get the string to change the color
  function GetColorString(Color: TPrintColor): string;
  var
    FG, BG: string;
  begin
    // Background
    with Color.Background do
      if Opc = ResetBGColor.Opc then // check if reset
        BG := #27'[49m'
      else
        BG :=
      {$IfDef Col24}
          Format(#27'[48;2;%d;%d;%dm', [R, G, B]);
      {$Else}
    Format(#27'[48;5;%dm', [Col]);
      {$EndIf}


    // Foreground
    with Color.Foreground do
      if Opc = ResetFGColor.Opc then
        FG := #27'[39m'
      else
        FG :=
      {$IfDef Col24}
          Format(#27'[38;2;%d;%d;%dm', [R, G, B]);
      {$Else}
    Format(#27'[38;5;%dm', [Col]);
      {$EndIf}

    Result := FG + BG;
  end;

var
  Buff: TCharBuffer;
  c, x: integer;
begin
  c := Low(TCharBuffer);
  FillChar(Buff, Length(Buff), ' ');
  MoveCursor(1, y + 1);
  for x := 0 to FWidth - 1 do
  begin
    WriteBuffer(Buff, GetColorString(Obj[x, y].Color), c);
    WriteBuffer(Buff, Obj[x, y].Value, c);
  end;
  // Flush Buffer
  FlushBuffer(Buff, c - Low(Buff));
  WriteLn('');
  FGraphic[y].Changed := False;
end;

{$Else}

procedure TTextCanvas.PrintLine(y: integer);
var
  x, len: integer;
  str: string;
  fg, bg: TColor;
begin
  GotoXY(1, y + 1);
  x := 0;
  while x < Width do
  begin
    SetLength(str, Width);
    len := 0;
    fg := Obj[x, y].Color.Foreground;
    bg := Obj[x, y].Color.Background;
    while (Obj[x + len, y].Color.Foreground = fg) and
      (Obj[x + len, y].Color.Background = bg) do
    begin
      str[len + 1] := Obj[x + len, y].Value;
      Inc(len);
    end;
    SetLength(str, len);
    if fg < Transparency then
      TextColor(fg);
    if bg < Transparency then
      TextBackground(bg);
    Write(str);
    Inc(x, len);
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

initialization
  {$IfDef WINDOWS}
  NextChars := '';
  {$EndIf}

end.
