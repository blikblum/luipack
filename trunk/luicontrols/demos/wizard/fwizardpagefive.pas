unit fWizardPageFive;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, fpjson;

type

  { TPageFiveFrame }

  TPageFiveFrame = class(TFrame)
    LabelConfigData: TLabel;
    ReadMeCheckBox: TCheckBox;
    Label1: TLabel;
    procedure ReadMeCheckBoxChange(Sender: TObject);
  private
    FConfigData: TJSONObject;
    { private declarations }
  public
    { public declarations }
  published
    procedure PageShow;
    property ConfigData: TJSONObject write FConfigData;
  end; 

implementation

uses
  LuiJSONUtils;

{$R *.lfm}

{ TPageFiveFrame }

procedure TPageFiveFrame.ReadMeCheckBoxChange(Sender: TObject);
begin
  if FConfigData = nil then
    Exit;
  if ReadMeCheckBox.Checked then
    FConfigData.Booleans['readme'] := True
  else
    RemoveJSONProp(FConfigData, 'readme');
end;

procedure TPageFiveFrame.PageShow;
begin
  if FConfigData <> nil then
    LabelConfigData.Caption := 'Config: ' + FConfigData.Strings['name'];
end;

end.

