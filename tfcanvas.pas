unit TFCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, Crt;

type

  { TTextCanvas }

  TTextCanvas = class
  private
    FWidth, FHeight: Integer;
    FGraphic: TPrintMap;
    FColor: TPrintColor;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    procedure Draw(X, Y: Integer; Graphic: TPrintMap);
    procedure TextOut(X, Y: Integer; Str: String);
    procedure Rectangle(X, Y, Width, Heigth: Integer);
    procedure Ellipsis(X, Y, Width, Height: Integer);
    procedure Line(X, Y, X2, Y2: Integer);
    constructor Create;
    destructor Destroy; override;
    procedure Resize(Width, Height: Integer);
    procedure SetColor(Foregrond, Background: Byte);
    procedure Clear;

    property Graphic: TPrintMap read FGraphic;
    property Color: TPrintColor read FColor write FColor;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

const Transparency = 255;

implementation

{ TTextCanvas }

procedure TTextCanvas.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TTextCanvas.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

procedure TTextCanvas.Draw(X, Y: Integer; Graphic: TPrintMap);
begin

end;

procedure TTextCanvas.TextOut(X, Y: Integer; Str: String);
begin

end;

procedure TTextCanvas.Rectangle(X, Y, Width, Heigth: Integer);
begin

end;

procedure TTextCanvas.Ellipsis(X, Y, Width, Height: Integer);
begin

end;

procedure TTextCanvas.Line(X, Y, X2, Y2: Integer);
begin

end;

constructor TTextCanvas.Create;
begin

end;

destructor TTextCanvas.Destroy;
begin
  inherited Destroy;
end;

procedure TTextCanvas.Resize(Width, Height: Integer);
begin

end;

procedure TTextCanvas.SetColor(Foregrond, Background: Byte);
begin

end;

procedure TTextCanvas.Clear;
begin

end;

end.

