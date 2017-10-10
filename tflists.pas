unit TFLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFCanvas, TFTypes, TFControls, math;

type

  { TTFListControl }

  TTFListControl = class(TDeltaUpdateControl)
  private
    FOnSelect: TNotifyEvent;
    FUpdateRows: TList;
    FItemIndex: IntPtr;
    FSelectionBG, FSelectionFG: TColor;
    FTopRow: IntPtr;
    procedure SetItemIndex(AValue: IntPtr);
    procedure SetSelectionBackground(AValue: TColor);
    procedure SetSelectionForeground(AValue: TColor);
    procedure SetTopRow(AValue: IntPtr);
  protected
    function GetItemCount: IntPtr; virtual; abstract;
    function GetItem(Index: IntPtr): string; virtual; abstract;
    procedure FocusChanged; override;
    procedure DeltaDraw(ACanvas: TTextCanvas); override;
    procedure Draw(ACanvas: TTextCanvas); override;
    procedure UpdateRow(Row: IntPtr); virtual;
    procedure DrawDone; override;
  public
    function ProcessInput(inp: String): boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property SelectionBackground: TColor read FSelectionBG write SetSelectionBackground;
    property SelectionForeground: TColor read FSelectionFG write SetSelectionForeground;
    property ItemIndex: IntPtr read FItemIndex write SetItemIndex;
    property TopRow: IntPtr read FTopRow write SetTopRow;
  end;

  { TTFListBox }

  TTFListBox = class(TTFListControl)
  private
    FItems: TStringList;
    procedure ItemsChanged(Sender: TObject);
  protected
    function GetItem(Index: IntPtr): string; override;
    function GetItemCount: IntPtr; override;
  public
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property Items: TStringList read FItems;
  end;

  { TTFCheckListBox }

  TTFCheckListBox = class(TTFListControl)
  private
    FItems: TStringList;
    FCheckCount: IntPtr;
    FOnChange: TNotifyEvent;
    FFilter: String;
    FFilterList: TList;
    function getChecked(Index: IntPtr): Boolean;
    procedure ItemsChanged(Sender: TObject);
    procedure SetChecked(Index: IntPtr; AValue: Boolean);
    procedure SetFilter(AValue: String);
  protected
    function GetItem(Index: IntPtr): string; override;
    function GetItemCount: IntPtr; override;
  public
    function ProcessInput(inp: String): boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property Checked[Index: IntPtr]: Boolean read getChecked write SetChecked;
    property CheckCount: IntPtr read FCheckCount;
    property Items: TStringList read FItems;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Filter: String read FFilter write SetFilter;
  end;

implementation

{ TTFCheckListBox }

procedure TTFCheckListBox.ItemsChanged(Sender: TObject);
var
  i: Integer;
begin
  FCheckCount:=0;
  FFilterList.Clear;
  for i:=0 to Items.Count-1 do
  begin
    if IntPtr(Items.Objects[i])<>0 then inc(FCheckCount);
    if (FFilter.Length>0) and Items[i].ToLower.Contains(FFilter.ToLower) then
      FFilterList.Add(Pointer(i));
  end;

  FullReadraw;
end;

function TTFCheckListBox.getChecked(Index: IntPtr): Boolean;
begin
  Result:=IntPtr(Items.Objects[Index]) <> 0;
end;

procedure TTFCheckListBox.SetChecked(Index: IntPtr; AValue: Boolean);
var i: IntPtr;
begin
  Items.OnChange:=nil;
  try
  if AValue and not Checked[Index] then
  begin
    Items.Objects[Index]:=TObject(-1);
    inc(FCheckCount);
  end
  else if not AValue and Checked[Index] then
  begin
    Items.Objects[Index]:=TObject(0);
    Dec(FCheckCount);
  end;
  finally
    items.OnChange:=@ItemsChanged;
  end;
  if FFilter.Length>0 then
  begin
    for i:=0 to FFilterList.Count-1 do
      if FFilterList[i] = Pointer(Index) then
      begin
        UpdateRow(i);
        break;
      end;
  end
  else
    UpdateRow(Index);
  if Assigned(FOnChange) then
    FOnChange(Self)
end;

procedure TTFCheckListBox.SetFilter(AValue: String);
var
  i: Integer;
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
  FullReadraw;
  FFilterList.Clear;
  for i:=0 to Items.Count-1 do
    if Items[i].ToLower.Contains(FFilter.ToLower) then
      FFilterList.Add(Pointer(i));
end;

function TTFCheckListBox.GetItem(Index: IntPtr): string;
begin
  if FFilter.Length>0 then
    Index:=IntPtr(FFilterList[Index]);
  SetLength(Result, Items[Index].Length+2);
  Move(Items[Index][1], Result[3], Items[Index].Length);
  Result[2]:='|';
  if Checked[Index] then Result[1]:='X'
  else Result[1]:='_';
end;

function TTFCheckListBox.GetItemCount: IntPtr;
begin
  if FFilter.Length>0 then
    Result:=FFilterList.Count
  else
    Result:=Items.Count;
end;

function TTFCheckListBox.ProcessInput(inp: String): boolean;
begin
  Result:=inherited ProcessInput(inp);
  if not Result then
    if inp = ' ' then
    begin
      if FFilter.Length>0 then 
         Checked[IntPtr(FFilterList[ItemIndex])]:=not Checked[IntPtr(FFilterList[ItemIndex])]
      else
        Checked[ItemIndex]:=not Checked[ItemIndex];
      Result:=True;
    end;
end;

constructor TTFCheckListBox.Create(AParent: TTextForm);
begin                       
  FItems:=TStringList.Create;
  FItems.OnChange:=@ItemsChanged;
  FFilterList:=TList.Create;
  inherited Create(AParent);
end;

destructor TTFCheckListBox.Destroy;
begin
  FItems.Free;
  FFilterList.Free;
  inherited Destroy;
end;

{ TTFListBox }

procedure TTFListBox.ItemsChanged(Sender: TObject);
begin
  FullReadraw;
end;

function TTFListBox.GetItem(Index: IntPtr): string;
begin
  Result:=FItems[Index];
end;

function TTFListBox.GetItemCount: IntPtr;
begin
  Result:=FItems.Count;
end;

constructor TTFListBox.Create(AParent: TTextForm);
begin
  FItems:=TStringList.Create;
  FItems.OnChange:=@ItemsChanged;
  inherited Create(AParent);
end;

destructor TTFListBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TTFListControl }

procedure TTFListControl.SetItemIndex(AValue: IntPtr);
begin
  if FItemIndex = AValue then
    Exit;
  UpdateRow(FItemIndex);
  FItemIndex := AValue;
  UpdateRow(FItemIndex);

  if Assigned(FOnSelect) then
    FOnSelect(Self);

  if (ItemIndex < 0) or (ItemIndex >= GetItemCount) then
    Exit;
  if (ItemIndex < TopRow) then
    TopRow := Max(0, ItemIndex - Height + 1)
  else if ItemIndex > TopRow + Height - 1 then
    TopRow := Min(ItemIndex, GetItemCount - Height);
  // fchanged gets set by UpdateRow
end;

procedure TTFListControl.SetSelectionBackground(AValue: TColor);
begin
  if FSelectionBG = AValue then
    Exit;
  FSelectionBG := AValue;
  FChanged := Focused;
  UpdateRow(ItemIndex);
end;

procedure TTFListControl.SetSelectionForeground(AValue: TColor);
begin
  if FSelectionFG = AValue then
    Exit;
  FSelectionFG := AValue;
  FChanged := Focused;
  UpdateRow(ItemIndex);
end;

procedure TTFListControl.SetTopRow(AValue: IntPtr);
begin
  if FTopRow = AValue then
    Exit;
  FTopRow := AValue;
  FullReadraw;
end;

procedure TTFListControl.FocusChanged;
begin
  inherited FocusChanged;
  UpdateRow(ItemIndex);
end;

procedure TTFListControl.DeltaDraw(ACanvas: TTextCanvas);
var
  i, j, x: IntPtr;
  s, str: string;
  fg, bg: TColor;
begin
  if Focused then
  begin
    fg:=SelectionForeground;
    bg:=SelectionBackground;
  end
  else
  begin
    fg:=FocusedForeground;
    bg:=FocusedBackground;
  end;
  for j := 0 to FUpdateRows.Count - 1 do
  begin
    i := IntPtr(FUpdateRows[j]);
    if (i < TopRow) or (i > Min(TopRow + Height, GetItemCount) - 1) then
      Continue;
    if (i < 0) or (i >= GetItemCount) then
      Continue;
    if i = ItemIndex then
      ACanvas.SetColor(fg, bg)
    else
      ACanvas.SetColor(Foreground, Background);
    str := GetItem(i);
    SetLength(s, Width-1);
    s[1] := ' ';
    for x := 1 to Width - 2 do
      if x <= str.Length then
        s[x + 1] := str[x]
      else
        s[x + 1] := ' ';
    ACanvas.TextOut(Left, Top + i - TopRow, s);
  end;
end;

procedure TTFListControl.Draw(ACanvas: TTextCanvas);
var
  i, j, x: IntPtr;
  s, str: string;
  fg, bg: TColor;
  d: Double;
  l: Integer;
  p: Integer;
begin
  inherited Draw(ACanvas);
  if Height<GetItemCount then
  begin
    d:=Height/GetItemCount;
    l:=Max(1,trunc(d*Height));
    p:=(TopRow div Height)*l;
    if TopRow=0 then p:=0
    else if TopRow=GetItemCount-Height then
      p:=Height-l-1;
    for i:=0 to l do
      ACanvas.TextOut(Left+Width-1, Top+p+i,'|');
  end;
  if Focused then
  begin
    fg:=SelectionForeground;
    bg:=SelectionBackground;
  end
  else
  begin
    fg:=FocusedForeground;
    bg:=FocusedBackground;
  end;
  for i := TopRow to Min(TopRow + Height, GetItemCount) - 1 do
  begin
    if i = ItemIndex then
      ACanvas.SetColor(fg, bg)
    else
      ACanvas.SetColor(Foreground, Background);
    str := GetItem(i);
    SetLength(s, Width-1);
    s[1] := ' ';
    for x := 1 to Width - 2 do
      if x <= str.Length then
        s[x + 1] := str[x]
      else
        s[x + 1] := ' ';
    ACanvas.TextOut(Left, Top + i - TopRow, s);
  end;
end;

procedure TTFListControl.UpdateRow(Row: IntPtr);
begin
  FUpdateRows.Add(Pointer(Row));
  FChanged := True;
end;

procedure TTFListControl.DrawDone;
begin
  inherited DrawDone;
  FUpdateRows.Clear;
end;

function TTFListControl.ProcessInput(inp: String): boolean;

  function Roll(p: integer): integer;
  begin
    if p >= 0 then
      Result := p mod GetItemCount
    else
      Result := p + GetItemCount;
  end;

begin
  Result := True;
  case GetArrow(inp) of
    akUp: ItemIndex := Roll(ItemIndex - 1);
    akDown: ItemIndex := Roll(ItemIndex + 1);
    else
      Result := False;
  end;
end;

constructor TTFListControl.Create(AParent: TTextForm);
begin
  FUpdateRows := TList.Create;
  FItemIndex := 0;
  inherited Create(AParent);  
  Width:=40;
  Height:=10;
  Background:=RGB(255,255,255);
  Foreground:=RGB(0,0,0);
  FocusedBackground:=RGB(192,192,192);
  FocusedForeground:=Foreground;
  SelectionBackground:=RGB(0,128,255);
  SelectionForeground:=Background;
  FullReadraw;
end;

destructor TTFListControl.Destroy;
begin
  FUpdateRows.Free;
  inherited Destroy;
end;

end.
