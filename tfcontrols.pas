unit TFControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFCanvas, fgl;

type
  TTextWinControl = class;

  { TTextControl }

  TTextControl = class
  private
    procedure SetParent(AValue: TTextWinControl);
  protected
    FParent: TTextWinControl;
    FRepaintRequired: Boolean;
  public
    procedure Repaint(Canvas: TTextCanvas);
    procedure DeltaPaint(Canvas: TTextCanvas);

    property RepaintRequired: Boolean read FRepaintRequired;
    property Parent: TTextWinControl read FParent write SetParent;
  end;

  TControlList = specialize TFPGObjectList<TTextControl>;

  { TTextWinControl }

  TTextWinControl = class(TTextControl)
  protected
    FControls: TControlList;
  public
    procedure Resize;
  end;


  TTextForm = class(TTextWinControl)
  private
  public
  end;

  TFormList = specialize TFPGObjectList<TTextForm>;

implementation

{ TTextWinControl }

procedure TTextWinControl.Resize;
begin

end;

{ TTextControl }

procedure TTextControl.SetParent(AValue: TTextWinControl);
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
end;

procedure TTextControl.Repaint(Canvas: TTextCanvas);
begin

end;

procedure TTextControl.DeltaPaint(Canvas: TTextCanvas);
begin

end;

end.

