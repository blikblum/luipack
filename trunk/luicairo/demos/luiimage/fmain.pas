unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, Spin, StdCtrls, LuiImage;

type

  { TMainForm }

  TMainForm = class(TForm)
    BorderOptionsPage: TPage;
    Label12: TLabel;
    OpacitySpinEdit: TFloatSpinEdit;
    Label11: TLabel;
    Label8: TLabel;
    PaddingGroupBox: TGroupBox;
    HorizontalScaleSpinEdit: TFloatSpinEdit;
    Label10: TLabel;
    Label9: TLabel;
    PaddingTopSpinEdit: TSpinEdit;
    PaddingLeftSpinEdit: TSpinEdit;
    PaddingRightSpinEdit: TSpinEdit;
    PaddingBottomSpinEdit: TSpinEdit;
    EffectsPage: TPage;
    VerticalScaleSpinEdit: TFloatSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    ScaleFactorGroupBox: TGroupBox;
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
    SizeOptionsPage: TPage;
    Label1: TLabel;
    Label2: TLabel;
    Image: TLuiImage;
    RoundEdgeRadiusSpinEdit: TSpinEdit;
    OutlineWidthSpinEdit: TSpinEdit;
    procedure AutoSizeCheckBoxChange(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure HeightSpinEditChange(Sender: TObject);
    procedure HorizontalScaleSpinEditChange(Sender: TObject);
    procedure OpacitySpinEditChange(Sender: TObject);
    procedure OutlineWidthSpinEditChange(Sender: TObject);
    procedure PaddingBottomSpinEditChange(Sender: TObject);
    procedure PaddingLeftSpinEditChange(Sender: TObject);
    procedure PaddingRightSpinEditChange(Sender: TObject);
    procedure PaddingTopSpinEditChange(Sender: TObject);
    procedure RoundEdgeRadiusSpinEditChange(Sender: TObject);
    procedure VerticalScaleSpinEditChange(Sender: TObject);
    procedure ViewStyleComboBoxSelect(Sender: TObject);
    procedure WidthSpinEditChange(Sender: TObject);
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
  Image.Colors.Background := clWhite;
end;

procedure TMainForm.HeightSpinEditChange(Sender: TObject);
begin
  Image.Height := HeightSpinEdit.Value;
end;

procedure TMainForm.HorizontalScaleSpinEditChange(Sender: TObject);
begin
  Image.ScaleFactor.Horizontal := HorizontalScaleSpinEdit.Value;
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

procedure TMainForm.PaddingBottomSpinEditChange(Sender: TObject);
begin
  Image.Padding.Bottom := PaddingBottomSpinEdit.Value;
end;

procedure TMainForm.PaddingLeftSpinEditChange(Sender: TObject);
begin
  Image.Padding.Left := PaddingLeftSpinEdit.Value;
end;

procedure TMainForm.PaddingRightSpinEditChange(Sender: TObject);
begin
  Image.Padding.Right := PaddingRightSpinEdit.Value;
end;

procedure TMainForm.PaddingTopSpinEditChange(Sender: TObject);
begin
  Image.Padding.Top := PaddingTopSpinEdit.Value;
end;

procedure TMainForm.RoundEdgeRadiusSpinEditChange(Sender: TObject);
begin
  Image.RoundRectRadius := RoundEdgeRadiusSpinEdit.Value;
end;

procedure TMainForm.VerticalScaleSpinEditChange(Sender: TObject);
begin
  Image.ScaleFactor.Vertical := VerticalScaleSpinEdit.Value;
end;

procedure TMainForm.ViewStyleComboBoxSelect(Sender: TObject);
begin
  case ViewStyleComboBox.ItemIndex of
    0: Image.ViewStyle := livNormal;
    1: Image.ViewStyle := livFitImage;
    2: Image.ViewStyle := livScale;
    3: Image.ViewStyle := livStretch;
    4: Image.ViewStyle := livTile;
  end;
end;

procedure TMainForm.WidthSpinEditChange(Sender: TObject);
begin
  Image.Width :=  WidthSpinEdit.Value;
end;

procedure TMainForm.LoadViewStyleValues;
begin
  with ViewStyleComboBox.Items do
  begin
    Add('Normal');
    Add('Fit Image');
    Add('Scale');
    Add('Stretch');
    Add('Tile');
  end;
  ViewStyleComboBox.ItemIndex := 0;
end;

procedure TMainForm.OpacitySpinEditChange(Sender: TObject);
begin
  Image.Opacity := OpacitySpinEdit.Value;
end;


initialization
  {$I fmain.lrs}

end.

