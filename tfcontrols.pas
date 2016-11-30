unit TFControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFCanvas, fgl;

type
  TTextWinControl = class;

  TTextControl = class
  protected
    FParent: TTextWinControl;
    FRepaintRequired: Boolean;
  public
    procedure Repaint(Canvas: TTextCanvas);
    procedure DeltaPaint(Canvas: TTextCanvas);

    property RepaintRequired: Boolean read FRepaintRequired;
    property Parent: TTextWinControl read FParent write SetParent;
  end;

  TTextWinControl = class(TTextControl)
  protected
    FControls: TControlList;
  public
    procedure Resize;
  end;

  TControlList = specialize TFPGObjectList<TTextControl>;

  TTextForm = class(TTextWinControl)
  private
  public
  end;

  TFormList = specialize TFPGObjectList<TTextForm>;

implementation

end.

