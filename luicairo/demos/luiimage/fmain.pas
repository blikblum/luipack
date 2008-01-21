unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, Spin, StdCtrls, LuiImage;

type

  { TMainForm }

  TMainForm = class(TForm)
    ViewStyleComboBox: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    AutoSizeCheckBox: TCheckBox;
    FileInfoPanel: TPanel;
    FileNameEdit: TFileNameEdit;
    ImagePanel: TPanel;
    ImageOptionsNotebook: TNotebook;
    BasicOptionsPage: TPage;
    Label1: TLabel;
    Label2: TLabel;
    Image: TLuiImage;
    RoundEdgeRadiusSpinEdit: TSpinEdit;
    OutlineWidthSpinEdit: TSpinEdit;
    procedure AutoSizeCheckBoxChange(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure OutlineWidthSpinEditChange(Sender: TObject);
    procedure RoundEdgeRadiusSpinEditChange(Sender: TObject);
    procedure ViewStyleComboBoxSelect(Sender: TObject);
  private
    { private declarations }
    procedure LoadViewStyleValues;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Image.Picture.LoadFromFile(Value);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadViewStyleValues;
end;

procedure TMainForm.AutoSizeCheckBoxChange(Sender: TObject);
begin
  if AutoSizeCheckBox.Checked then
    Image.Options := Image.Options + [lioAutoSize]
  else
    Image.Options := Image.Options - [lioAutoSize];
end;

procedure TMainForm.OutlineWidthSpinEditChange(Sender: TObject);
begin
  Image.OutLineWidth := OutlineWidthSpinEdit.Value;
end;

procedure TMainForm.RoundEdgeRadiusSpinEditChange(Sender: TObject);
begin
  Image.RoundEdgeRadius := RoundEdgeRadiusSpinEdit.Value;
end;

procedure TMainForm.ViewStyleComboBoxSelect(Sender: TObject);
begin
  case ViewStyleComboBox.ItemIndex of
    0: Image.ViewStyle := livNormal;
    1: Image.ViewStyle := livCenter;
    2: Image.ViewStyle := livScale;
    3: Image.ViewStyle := livAutoScale;
    4: Image.ViewStyle := livZoom;
    5: Image.ViewStyle := livTile;
  end;
end;

procedure TMainForm.LoadViewStyleValues;
begin
  with ViewStyleComboBox.Items do
  begin
    Add('Normal');
    Add('Center');
    Add('Scale');
    Add('Auto Scale');
    Add('Zoom');
    Add('Tile');
  end;
  ViewStyleComboBox.ItemIndex := 0;
end;


initialization
  {$I fmain.lrs}

end.

