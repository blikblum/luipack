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
  private
    FConfigData: TJSONObject;
    { private declarations }
  public
    { public declarations }
  published
    procedure UpdateControl;
    property ConfigData: TJSONObject write FConfigData;
  end; 

implementation

{$R *.lfm}

{ TPageFiveFrame }

procedure TPageFiveFrame.UpdateControl;
begin
  if FConfigData <> nil then
    LabelConfigData.Caption := 'Config: ' + FConfigData.Strings['name'];
end;

end.

