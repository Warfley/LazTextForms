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

  TEventQueue = specialize TLinkedQueue<TEventInformation>;

  TWindowSize = record
    Width, Height: Integer;
  end;

  {$IfDef Col24}
  TColor = record
    case Boolean of
    True: (Color: Integer);
    {$IfNDef ENDIAN_LITTLE}
    False: (Opc, R, G, B: Byte);
    {$Else}
    False: (B, G, R, Opc: Byte);
    {$EndIf}
  end;
  {$Else}
  TColor = Byte;
  {$EndIf}

  TPrintColor = record
    Background, Foreground: TColor;
  end;

  TPrintObject = record
    Color: TPrintColor;
    Value: Char;
  end;

  TPrintLine = record
    Changed: Boolean;
    Line: Array of TPrintObject;
  end;

  TPrintMap = array of TPrintLine;

implementation

end.

