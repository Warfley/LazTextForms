program TextFormsExample;


uses TFCanvas, TFTypes, sysutils;

function HSVtoRGB(H:Integer; S, V: Byte): TColor;
var
  ht, d, t1, t2, t3:Integer;
  R,G,B:Word;
begin
  if S = 0 then
   begin
    R := V; G := V; B := V;
   end
  else
   begin
    ht := H * 6;
    d := ht mod 360;

    t1 := round(V * (255 - S) / 255);
    t2 := round(V * (255 - S * d / 360) / 255);
    t3 := round(V * (255 - S * (360 - d) / 360) / 255);

    case ht div 360 of
    0:
      begin
        R := V; G := t3; B := t1;
      end;
    1:
      begin
        R := t2; G := V; B := t1;
      end;
    2:
      begin
        R := t1; G := V; B := t3;
      end;
    3:
      begin
        R := t1; G := t2; B := V;
      end;
    4:
      begin
        R := t3; G := t1; B := V;
      end;
    else
      begin
        R := V; G := t1; B := t2;
      end;
    end;
   end;
  Result:=RGB(R,G,B);
end;

var canvas: TTextCanvas;
  h: Integer;
begin
  Randomize;
  canvas:=TTextCanvas.Create;
  try
    canvas.Width:=20;
    canvas.Height:=9;
    canvas.SetColor(HSVtoRGB(Random(256), Random(256), Random(256)), HSVtoRGB(Random(256),Random(256),Random(256)));
    canvas.Ellipsis(0,0, 20, 9);
    canvas.SetColor(RGB(0,200,0), RGB(0,200,0));
    canvas.Line(0,0, 19, 8);
    canvas.Print();
    for h:=0 to 360 do
    begin
    canvas.SetColor(HSVtoRGB(h, 255, 255), Transparency);
    canvas.TextOut(7, 4, 'Test');
    canvas.Print();
    Sleep(20);
    end;
  finally
    canvas.Free;
  end;
end.

