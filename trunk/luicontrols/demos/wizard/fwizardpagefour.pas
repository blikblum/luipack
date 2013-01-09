unit fWizardPageFour;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, fpjson;

type

  { TPageFourFrame }

  TPageFourFrame = class(TFrame)
    ConfigEdit: TEdit;
    Label1: TLabel;
    procedure ConfigEditEditingDone(Sender: TObject);
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

{$R *.lfm}

{ TPageFourFrame }

procedure TPageFourFrame.ConfigEditEditingDone(Sender: TObject);
begin
  FConfigData.Strings['name'] := ConfigEdit.Text;
end;

procedure TPageFourFrame.PageShow;
begin
  if FConfigData <> nil then
    ConfigEdit.Text := FConfigData.Strings['name'];
end;

end.

