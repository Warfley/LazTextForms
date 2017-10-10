program example;

{$mode objfpc}{$H+}

uses
  testform, TFCanvas;

var Canvas: TTextCanvas;
begin
  Canvas:=TTextCanvas.Create;
  with TTestForm.Create(Canvas) do
  try
    Show;
  finally
    Free;
    Canvas.Free;
  end;
end.

