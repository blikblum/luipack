unit JSONElementUpdateMethodView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TJSONElementUpdateMethodForm }

  TJSONElementUpdateMethodForm = class(TForm)
    DoneButton: TBitBtn;
    Label1: TLabel;
    MergeRadioButton: TRadioButton;
    OverwriteRadioButton: TRadioButton;
    SkipRadioButton: TRadioButton;
    UseAsDefaultCheckBox: TCheckBox;
    InfoLabel: TLabel;
  private
    procedure SetPropertyName(const Value: String);
  public
    property PropertyName: String write SetPropertyName;
    function UpdateMethod: Integer;
    function UseAsDefault: Boolean;
  end;

var
  JSONElementUpdateMethodForm: TJSONElementUpdateMethodForm;

implementation

{$R *.lfm}

{ TJSONElementUpdateMethodForm }

procedure TJSONElementUpdateMethodForm.SetPropertyName(const Value: String);
begin
  InfoLabel.Caption := Format('An element associated with property %s already exists', [Value]);
end;

function TJSONElementUpdateMethodForm.UpdateMethod: Integer;
begin
  if SkipRadioButton.Checked then
    Result := 0
  else if MergeRadioButton.Checked then
    Result := 1
  else
    Result := 2;
end;

function TJSONElementUpdateMethodForm.UseAsDefault: Boolean;
begin
  Result := UseAsDefaultCheckBox.Checked;
end;

end.

