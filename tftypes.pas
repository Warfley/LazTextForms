unit TFTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LinkedQueue;

type      

  TEventMethod = procedure (Data: PtrInt) of object;

  TEventInformation = record
    Method: TEventMethod;
    Data: PtrInt;
  end;

  TPrintColor = record
    Background, Foregrond: Byte;
  end;

  TPrintObject = record
    Color: TPrintColor;
    Value: Char;
  end;

  TPrintLine = record
    Changed: Boolean;
    Line: Array of TPrintObject;
  end;

  TPrintMap = array of TPrintObject;
  TEventQueue = specialize TLinkedQueue<TEventInformation>;

implementation

end.

