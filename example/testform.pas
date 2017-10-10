unit testform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFCanvas, TFControls, TFBaseControls, TFLists;

type

  { TTestForm }

  TTestForm = class(TTextForm)
  private
    TestLabel: TTFLabel;
    TestCheckBox: TTFCheckBox;
    TestListBox: TTFListBox;
    TestCheckListBox: TTFCheckListBox;
    TestEdit: TTFEdit;
    TestButton: TTFButton;
    procedure ButtonClicked(Sender: TObject);
    procedure CheckBoxChanged(Sender: TObject);
    procedure EditEnterPressed(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(ACanvas: TTextCanvas); override;
  end;

implementation

{ TTestForm }

procedure TTestForm.ButtonClicked(Sender: TObject);
begin
  TestListBox.Background := RGB(Random(256), Random(256), Random(256));
end;

procedure TTestForm.CheckBoxChanged(Sender: TObject);
begin
  if TestCheckBox.Checked then
    TestLabel.Foreground := RGB(255, 0, 0)
  else
    TestLabel.Foreground := RGB(255, 255, 255);
end;

procedure TTestForm.EditEnterPressed(Sender: TObject);
begin
  TestCheckListBox.Filter := TestEdit.Text;
end;

procedure TTestForm.Resize;
var
  m: TPoint;
begin
  m := Point(Width div 2, Height div 2); // center
  // Position the controls in here

  TestLabel.Left := m.x - (TestLabel.Width div 2);
  TestLabel.Top := 0;

  TestCheckBox.Left := m.x - (TestCheckBox.Width div 2);
  TestCheckBox.Top := 1;

  TestListBox.Left := m.x - (TestListBox.Width div 2);
  TestListBox.Top := 3;

  TestCheckListBox.Left := m.x - (TestCheckListBox.Width div 2);
  TestCheckListBox.Top := 10;

  TestEdit.Left := m.x - (TestEdit.Width div 2);
  TestEdit.Top := 17;

  TestButton.Left := m.x - (TestButton.Width div 2);
  TestButton.Top := 19;

  // fires event
  inherited Resize;
end;

constructor TTestForm.Create(ACanvas: TTextCanvas);
begin
  inherited Create(ACanvas);
  Randomize;
  TestLabel := TTFLabel.Create(self);

  TestCheckBox := TTFCheckBox.Create(self);
  TestCheckBox.OnChange := @CheckBoxChanged;

  TestListBox := TTFListBox.Create(self);
  TestListBox.Height:=6;
  TestListBox.Items.Add('Item1');
  TestListBox.Items.Add('Item2');
  TestListBox.Items.Add('Item3');

  TestCheckListBox := TTFCheckListBox.Create(self);
  TestCheckListBox.Height:=6;
  TestCheckListBox.Items.Add('CheckItem1');
  TestCheckListBox.Items.Add('CheckItem2');
  TestCheckListBox.Items.Add('CheckItem3');

  TestEdit := TTFEdit.Create(self);
  TestEdit.OnEnterKey := @EditEnterPressed;

  TestButton := TTFButton.Create(self);
  TestButton.OnClick := @ButtonClicked;
end;

end.
