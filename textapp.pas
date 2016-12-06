unit TextApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, TFTypes, TFCanvas, TFControls, Crt;

type

  { TTextApplication }

  TTextApplication = class(TCustomApplication)
  protected
    FForms: TFormList;
    FActiveForm: integer;
    FEventQueue: TEventQueue;
    FCanvas: TTextCanvas;
    procedure DoRun; override;
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
