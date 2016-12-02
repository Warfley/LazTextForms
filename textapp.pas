unit TextApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, TFTypes, TFCanvas, Crt;

type

  { TTextApplication }

  TTextApplication = class(TCustomApplication)
  protected
    FForms: TFormList;
    FActiveForm: Integer;
    FEventQueue:TEventQueue;
    FCanvas: TTextCanvas;
    procedure DoRun; override;
    procedure RePaint(FullRepaint: Boolean = False);
  public
    procedure QueueEvent(Event: TEventMethod; Data: PtrInt);
    procedure ProcessEvents;
    procedure AddForm(F: TTextForm);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TTextApplication }

procedure TTextApplication.DoRun;
begin
  inherited DoRun;

end;

procedure TTextApplication.RePaint(FullRepaint: Boolean = False);
var x, y: Integer;
begin
  for y:=0 to FCanvas.Height-1 do
  begin
    if Not (FullRepaint Or FCanvas.LineChanged[Y]) then Continue;
    GotoXY(1, y + 1);
    for x:=0 to FCanvas.Width-1 do
    begin
      TextColor(FCanvas.Obj[x,y].Color.Foregrond);
      TextBackground(FCanvas.Obj[x,y].Color.Background);
      Write(FCanvas.Obj[x,y].Value);
    end;
  end;
  GotoXY(FCanvas.Width, FCanvas.Height);
  WriteLn('');
end;

procedure TTextApplication.QueueEvent(Event: TEventMethod; Data: PtrInt);
begin

end;

procedure TTextApplication.ProcessEvents;
begin

end;

procedure TTextApplication.AddForm(F: TTextForm);
begin

end;

constructor TTextApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTextApplication.Destroy;
begin
  inherited Destroy;
end;

end.

