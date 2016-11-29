unit LinkedQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLinkedQueue }

  generic TLinkedQueue<T> = class
  private
    type
    PLinkItem = ^TLinkItem;

    TLinkItem = record
      Value: T;
      Next: PLinkItem
    end;
  var
    First: PLinkItem;
    Last: PLinkItem;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value: T);
    function Pop: T;
    function Peek: T;
    procedure Clear;
    property Count: integer read FCount;
  end;

implementation

{ TLinkedQueue }

constructor TLinkedQueue.Create;
begin
  First := Nil;
  Last := Nil;
  FCount:=0;
end;

destructor TLinkedQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLinkedQueue.Add(Value: T);
var n: PLinkItem;
begin
  new(n);
  n^.Value:=Value;
  n^.Next:=nil;
  Last^.Next:=n;
  Last:=n;
  inc(FCount);
end;

function TLinkedQueue.Pop: T;
var n: PLinkItem;
begin
  if First = nil then
    raise EListError.Create('No element found');
  n := First;
  First:=First^.Next;
  if First=Nil then Last:=Nil;
  Result:=n^.Value;
  Dispose(n);
  Dec(FCount);
end;

function TLinkedQueue.Peek: T;
begin
  if First = nil then
    raise EListError.Create('No element found');
  Result:=First^.Value;
end;

procedure TLinkedQueue.Clear;
begin
  while FCount>0 do
    Pop;
end;

end.
