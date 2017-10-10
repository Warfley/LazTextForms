unit TFTypes;

{$Include defines.inc}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TWindowSize = record
    Width, Height: Integer;
  end;

  TColor24 = record
    case Boolean of
    True: (Color: Integer);
    {$IfDef ENDIAN_LITTLE}
    False: (B, G, R, Opc: Byte);
    {$Else}
    False: (Opc, R, G, B: Byte);
    {$EndIf}
  end;

  TColor8 = record
    case Boolean of
    True: (Color: Word);
    {$IfDef ENDIAN_LITTLE}
    False: (Col, Opc: Byte);
    {$Else}
    False: (Opc, Col: Byte);
    {$EndIf}
  end;

  TColor4 = Byte;

  TColor =
  {$IfDef Col24}
    TColor24
  {$Else}
    {$IfDef Col8}
      TColor8
    {$Else}
      TColor4
    {$EndIf}
  {$EndIf};

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

    operator =(a,b: TColor24): Boolean;
    operator =(a,b: TColor8): Boolean;

implementation

operator=(a, b: TColor24): Boolean;
begin
  result:=a.Color=b.Color;
end;

operator=(a, b: TColor8): Boolean;
begin
  result:=a.Color=b.Color;
end;

end.

