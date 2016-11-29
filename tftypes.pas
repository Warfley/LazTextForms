unit TFTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // generic lists
  fgl, LinkedQueue;

type      

  TEventMethod = procedure (Data: PtrInt) of object;

  TEventInformation = record
    Method: TEventMethod;
    Data: PtrInt;
  end;

  TTextControl = class
  protected      
    FParent: TTextControl;
  public

  end;

  TTextWinControl = class(TTextControl)
  protected
    FControls: TControlList;
  public
    procedure Repaint;
    procedure DeltaPaint;
    procedure Resize
  end;

  TControlList = specialize TFPGObjectList<TTextControl>;

  TTextForm = class(TTextWinControl)
  private
  public
  end;

  TFormList = specialize TFPGObjectList<TTextForm>;
  TEventQueue = specialize TLinkedQueue<TEventInformation>;

implementation

end.

