unit TFBaseControls;

{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFControls, TFCanvas, Math;

type

  { TTFLabel }

  TTFLabel = class(TTextControl)
  private
    FText: TStringList;
    FAutoSize: boolean;
    FAlign: TAlign;
    procedure SetAlign(AValue: TAlign);
    procedure TextChanged(Sender: TObject);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property Text: TStringList read FText;
    property AutoSize: boolean read FAutoSize write FAutoSize;
    property Align: TAlign read FAlign write SetAlign;
  end;


  { TTFCheckBox }

  TTFCheckBox = class(TUserControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FText: TStringList;
    FAlign: TAlign;
    FAutoSize: boolean;
    procedure SetAlign(AValue: TAlign);
    procedure setAutoSize(AValue: boolean);
    procedure setChecked(AValue: boolean);
    procedure TextChanged(Sender: TObject);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessInput(inp: string): boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Text: TStringList read FText;
    property Align: TAlign read FAlign write SetAlign;
    property AutoSize: boolean read FAutoSize write setAutoSize;
    property Checked: boolean read FChecked write setChecked;
  end;

  { TTFEdit }

  TTFEdit = class(TUserControl)
  private
    FNumbersOnly: boolean;
    FOnChange: TNotifyEvent;
    FText: string;
    FAlign: TAlign;
    FCursorPos: integer;
    FCursorColor: TColor;
    FOnEnterKey: TNotifyEvent;
    procedure SetAlign(AValue: TAlign);
    procedure SetCursorColor(AValue: TColor);
    procedure SetPosition(AValue: integer);
    procedure SetText(AValue: string);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessInput(inp: string): boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property NumbersOnly: boolean read FNumbersOnly write FNumbersOnly;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Text: string read FText write SetText;
    property CursorColor: TColor read FCursorColor write SetCursorColor;
    property Position: integer read FCursorPos write SetPosition;
    property Align: TAlign read FAlign write SetAlign;
    property OnEnterKey: TNotifyEvent read FOnEnterKey write FOnEnterKey;
  end;

  { TTFButton }

  TTFButton = class(TUserControl)
  private
    FOnClick: TNotifyEvent;
    FCaption: string;
    FAlign: TAlign;
    FHighlightColor: TColor;
    FShortCut: char;
    procedure SetAlign(AValue: TAlign);
    procedure SetCaption(AValue: string);
    procedure SetHighlightColor(AValue: TColor);
    procedure SetShortCut(AValue: char);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessInput(inp: string): boolean; override;
    function ProcessInputGlobal(inp: string): boolean; override;
    constructor Create(AParent: TTextForm); override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read FCaption write SetCaption;
    property Align: TAlign read FAlign write SetAlign;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor;
    property ShortCut: char read FShortCut write SetShortCut;
  end;


implementation

{ TTFCheckBox }

procedure TTFCheckBox.SetAlign(AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  Invalidate;
end;

procedure TTFCheckBox.setAutoSize(AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit;
  FAutoSize := AValue;
  Invalidate;
end;

procedure TTFCheckBox.setChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  Invalidate;
end;

procedure TTFCheckBox.TextChanged(Sender: TObject);
var
  s: string;
begin
  Invalidate;
  if AutoSize then
  begin
    Height := Max(1, Text.Count);
    for s in Text do
      if s.Length + 3 > Width then
        Width := s.Length + 3;
  end;
end;

procedure TTFCheckBox.Draw(ACanvas: TTextCanvas);
var
  i: integer;
begin
  inherited Draw(ACanvas);
  for i := 0 to Text.Count - 1 do
    case Align of
      alLeft:
        ACanvas.TextOut(Left + 3, Top + i, Text[i].Substring(0, Width - 3));
      alRight:
        ACanvas.TextOut(Left + Width - Text[i].Substring(0, Width - 3).Length,
          Top + i, Text[i].Substring(0, Width - 3));
      alCenter:
        ACanvas.TextOut(Left + 3 +
          (((Width - 3) - Text[i].Substring(0, Width - 3).Length) div 2),
          Top + i, Text[i].Substring(0, Width - 3));
    end;
  if Checked then
    Canvas.TextOut(Left, Top + (Height div 2), '[X]')
  else
    Canvas.TextOut(Left, Top + (Height div 2), '[ ]');
end;

function TTFCheckBox.ProcessInput(inp: string): boolean;
begin
  Result := inp = ' ';
  if Result then
  begin
    Checked := not Checked;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  if not Result then
    Result := inherited ProcessInput(inp);
end;

constructor TTFCheckBox.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  AutoSize := True;
  Foreground := RGB(255, 255, 255);
  Background := Transparency;
  FocusedBackground := RGB(0, 128, 255);
  FocusedForeground := RGB(255, 255, 255);
  FText := TStringList.Create;
  FText.OnChange := @TextChanged;
  FText.Text := 'Checkbox';
end;

destructor TTFCheckBox.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

{ TTFButton }

procedure TTFButton.SetAlign(AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  Invalidate;
end;

procedure TTFButton.SetCaption(AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TTFButton.SetHighlightColor(AValue: TColor);
begin
  if FHighlightColor = AValue then
    Exit;
  FHighlightColor := AValue;
  Invalidate;
end;

procedure TTFButton.SetShortCut(AValue: char);
begin
  if FShortCut = AValue then
    Exit;
  FShortCut := AValue;
  Invalidate;
end;

procedure TTFButton.Draw(ACanvas: TTextCanvas);
var
  str, s, e: string;
  l: integer;
  p: IntPtr;
begin
  inherited Draw(ACanvas);
  str := FCaption.Substring(0, Width);
  p := Pos(lowerCase(FShortCut), str.ToLower);
  if p < 1 then
    case Align of
      alLeft:
        ACanvas.TextOut(Left, Top + (Height div 2), str);
      alRight:
        ACanvas.TextOut(Left + Width - str.Length,
          Top + (Height div 2), str);
      alCenter:
        ACanvas.TextOut(Left + ((Width - str.Length) div 2),
          Top + (Height div 2), str);
    end
  else
  begin
    s := str.Substring(0, p - 1);
    e := str.Substring(p);
    case Align of
      alLeft:
        l := Left + Width - str.Length;
      alRight:
        l := Left + Width - str.Length;
      alCenter:
        l := Left + ((Width - str.Length) div 2);
    end;
    ACanvas.TextOut(l, Top + (Height div 2), s);
    ACanvas.SetColor(FHighlightColor, Transparency);
    ACanvas.TextOut(l + s.Length, Top + (Height div 2), str[p]);
    ACanvas.SetColor(Foreground, Transparency);
    ACanvas.TextOut(l + s.Length + 1, Top + (Height div 2), e);
  end;
end;

function TTFButton.ProcessInput(inp: string): boolean;
begin
  Result := inp = #13;
  if Result and Assigned(FOnClick) then
    FOnClick(Self);
  if not Result then
    Result := inherited ProcessInput(inp);
end;

function TTFButton.ProcessInputGlobal(inp: string): boolean;
begin
  Result := inp = #27 + FShortCut;
  if Result and Assigned(FOnClick) then
    FOnClick(Self)
  else if not Result then
    Result := inherited ProcessInputGlobal(inp);
end;

constructor TTFButton.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Width := 20;
  Height := 3;
  Foreground := RGB(0, 0, 0);
  Background := RGB(200, 200, 200);
  FocusedBackground := RGB(255, 0, 0);
  FocusedForeground := RGB(255, 255, 255);
  Align := alCenter;
  FShortCut := #0;
  FHighlightColor := RGB(255, 255, 0);
  Caption := 'Click me';
end;

{ TTFEdit }

procedure TTFEdit.SetAlign(AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  Invalidate;
end;

procedure TTFEdit.SetCursorColor(AValue: TColor);
begin
  if FCursorColor = AValue then
    Exit;
  FCursorColor := AValue;
  Invalidate;
end;

procedure TTFEdit.SetPosition(AValue: integer);
begin
  if FCursorPos = AValue then
    Exit;
  FCursorPos := AValue;
  Invalidate;
end;

procedure TTFEdit.SetText(AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  Invalidate;
  if FText.Length < FCursorPos then
    FCursorPos := FText.Length;
end;

procedure TTFEdit.Draw(ACanvas: TTextCanvas);
var
  fg, bg: TColor;
  t: string;
  l: integer;
begin
  if Focused then
  begin
    fg := FocusedForeground;
    bg := FocusedBackground;
  end
  else
  begin
    fg := Foreground;
    bg := Background;
  end;
  inherited Draw(ACanvas);
  t := Text.Substring(0, Width);
  case Align of
    alLeft:
      l := Left;
    alRight:
    begin
      l := Left + Width - t.Length;
    end;
    alCenter:
      l := Left + ((Width - t.Length) div 2);
  end;
  ACanvas.SetColor(fg, bg);
  ACanvas.TextOut(l, Top, t.Substring(0, FCursorPos));
  ACanvas.SetColor(fg, CursorColor);
  if Text.Length = FCursorPos then
    ACanvas.TextOut(l + FCursorPos, Top, ' ')
  else
  begin
    ACanvas.TextOut(l + FCursorPos, Top, t.Substring(FCursorPos, 1));
    ACanvas.SetColor(fg, bg);
    ACanvas.TextOut(l + FCursorPos + 1, Top, t.Substring(FCursorPos + 1));
  end;
end;

function TTFEdit.ProcessInput(inp: string): boolean;
begin
  Result := True;
  case GetArrow(inp) of
    akLeft: Position := Max(0, Position - 1);
    akRight: Position := Min(Text.Length, Position + 1);
    akUp, akDown: Result := False;
    akNone:
      case inp[1] of
        #8, #127:
        begin
          // FText because of FCursorPos
          FText := FText.Substring(0, FCursorPos - 1) + FText.Substring(FCursorPos);
          FCursorPos := Max(0, FCursorPos - 1);
          Invalidate;
          if Assigned(FOnChange) then
            FOnChange(Self);
        end;
        #13: if Assigned(FOnEnterKey) then
            FOnEnterKey(Self)
          else
            Result := False;
        #33..#126, #128..#254:
          if (NumbersOnly and (inp[1] in ['0'..'9'])) or not NumbersOnly then
          begin
            Text := FText.Substring(0, FCursorPos) + inp[1] +
              FText.Substring(FCursorPos);
            Inc(FCursorPos);
            if Assigned(FOnChange) then
              FOnChange(Self);
          end;
        else
          Result := False;
      end;
  end;
  if not Result then
    Result := inherited ProcessInput(inp);
end;

constructor TTFEdit.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Width := 20;
  Height := 1;
  Align := alLeft;
  Background := RGB(255, 255, 255);
  Foreground := RGB(0, 0, 0);
  FCursorColor := RGB(196, 196, 196);
  FocusedBackground := RGB(0, 128, 255);
  FocusedForeground := Background;
  FText := 'Edit';
  FCursorPos := FText.Length;
end;

destructor TTFEdit.Destroy;
begin
  inherited Destroy;
end;


{ TTFLabel }

procedure TTFLabel.SetAlign(AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  Invalidate;
end;

procedure TTFLabel.TextChanged(Sender: TObject);
var
  s: string;
begin
  Invalidate;
  if AutoSize then
  begin
    Height := Text.Count;
    for s in Text do
      if s.Length > Width then
        Width := s.Length;
  end;
end;

procedure TTFLabel.Draw(ACanvas: TTextCanvas);
var
  i: integer;
begin
  inherited Draw(ACanvas);
  for i := 0 to Text.Count - 1 do
    case Align of
      alLeft:
        ACanvas.TextOut(Left, Top + i, Text[i]);
      alRight:
        ACanvas.TextOut(Left + Width - Text[i].Length, Top + i, Text[i]);
      alCenter:
        ACanvas.TextOut(Left + ((Width - Text[i].Length) div 2), Top + i, Text[i]);
    end;
end;

constructor TTFLabel.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Foreground := RGB(255, 255, 255);
  Background := Transparency;
  AutoSize := True;
  FText := TStringList.Create;
  FText.OnChange := @TextChanged;
  FText.Text := 'Hallo Welt!';
end;

destructor TTFLabel.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

end.
