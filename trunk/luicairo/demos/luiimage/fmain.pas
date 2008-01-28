unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, Spin, StdCtrls, LuiImage, CairoImaging, CairoClasses, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    BorderOptionsPage: TPage;
    RotateButton: TButton;
    ReflectionButton: TButton;
    KeepAspectCheckBox: TCheckBox;
    Label14: TLabel;
    MaskColorButton: TColorButton;
    Label13: TLabel;
    CustomPage: TPage;
    RotateSpinEdit: TSpinEdit;
    TransparencyModeCombo: TComboBox;
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
    procedure DrawReflection(Sender: TLuiImage);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure HeightSpinEditChange(Sender: TObject);
    procedure HorizontalScaleSpinEditChange(Sender: TObject);
    procedure KeepAspectCheckBoxChange(Sender: TObject);
    procedure MaskColorButtonColorChanged(Sender: TObject);
    procedure NoClipPaint(Sender: TLuiImage);
    procedure OpacitySpinEditChange(Sender: TObject);
    procedure OutlineWidthSpinEditChange(Sender: TObject);
    procedure PaddingBottomSpinEditChange(Sender: TObject);
    procedure PaddingLeftSpinEditChange(Sender: TObject);
    procedure PaddingRightSpinEditChange(Sender: TObject);
    procedure PaddingTopSpinEditChange(Sender: TObject);
    procedure SetRotateMatrix(Sender: TLuiImage; const Matrix: TCairoMatrix);
    procedure ReflectionButtonClick(Sender: TObject);
    procedure RotateButtonClick(Sender: TObject);
    procedure RoundEdgeRadiusSpinEditChange(Sender: TObject);
    procedure TransparencyModeComboSelect(Sender: TObject);
    procedure VerticalScaleSpinEditChange(Sender: TObject);
    procedure ViewStyleComboBoxSelect(Sender: TObject);
    procedure WidthSpinEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  CairoUtils;

{ TMainForm }

procedure TMainForm.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Image.Picture.LoadFromFile(Value);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Image.Picture.Options := [cioAllowChangesAfterLoad];
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

procedure TMainForm.KeepAspectCheckBoxChange(Sender: TObject);
begin
  if KeepAspectCheckBox.Checked then
    Image.Options := Image.Options + [lioKeepAspectOnStretch]
  else
    Image.Options := Image.Options - [lioKeepAspectOnStretch];
end;

procedure TMainForm.MaskColorButtonColorChanged(Sender: TObject);
begin
  with Image.Picture.MaskColor do
    RedGreenBlue(ColorToRGB(MaskColorButton.ButtonColor), R, G, B);
  Image.Picture.UpdateMask;
end;

procedure TMainForm.NoClipPaint(Sender: TLuiImage);
begin
  //do nothing
end;

procedure TMainForm.AutoSizeCheckBoxChange(Sender: TObject);
begin
  if AutoSizeCheckBox.Checked then
    Image.Options := Image.Options + [lioAutoSize]
  else
    Image.Options := Image.Options - [lioAutoSize];
end;

procedure TMainForm.DrawReflection(Sender: TLuiImage);
var
  Gradient: TCairoLinearGradient;
  Matrix: TCairoMatrix;
begin
  with Image, Context do
  begin
    ResetClip;
    Translate(Padding.Left, (Picture.Data.Height*2 + Padding.Top + 1));
    Rotate(PI);
    Scale(-1, 1);
    SetSourceSurface(Picture.Surface, 0, 0);
    Gradient := TCairoLinearGradient.Create(0, 0, 0, Picture.Data.Height);
    Gradient.AddColorStopRgba(0, 0, 0, 0, 0);
    Gradient.AddColorStopRgba(1, 0, 0, 0, 0.8);
    Mask(Gradient);
    Gradient.Destroy;
  end;
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

procedure TMainForm.SetRotateMatrix(Sender: TLuiImage;
  const Matrix: TCairoMatrix);
begin
  //todo: properly calculate necessary padding values
  Matrix.InitTranslate(Max(Image.Picture.Width, Image.Picture.Height),
    Max(Image.Picture.Width, Image.Picture.Height));
  Matrix.Rotate(DegreeToRadian(RotateSpinEdit.Value));
end;

procedure TMainForm.ReflectionButtonClick(Sender: TObject);
begin
  with Image do
  begin
    BeginUpdate;
    AutoSizeCheckBox.Checked := True; //auto size
    ViewStyleComboBox.ItemIndex := 0; //normal style
    Padding.Bottom := Picture.Data.Height + 10; //area for reflection + padding
    Padding.Top := 10;
    Padding.Left := 10;
    Padding.Right := 10;
    OnAfterPaint := @DrawReflection;
    EndUpdate;
    OnAfterPaint := nil;
  end;
end;

procedure TMainForm.RotateButtonClick(Sender: TObject);
begin
  with Image do
  begin
    BeginUpdate;
    ViewStyleComboBox.ItemIndex := 0;
    OnPrepareMatrix := @SetRotateMatrix;
    OnBeforePaint := @NoClipPaint;
    EndUpdate;
    OnPrepareMatrix := nil;
    OnBeforePaint := nil;
  end;
end;

procedure TMainForm.RoundEdgeRadiusSpinEditChange(Sender: TObject);
begin
  Image.RoundRectRadius := RoundEdgeRadiusSpinEdit.Value;
end;

procedure TMainForm.TransparencyModeComboSelect(Sender: TObject);
begin
  case TransparencyModeCombo.ItemIndex of
    0: Image.Picture.TransparencyMode := citDefault;
    1: Image.Picture.TransparencyMode := citMaskNonAlpha;
    2: Image.Picture.TransparencyMode := citForceMaskColor;
    3: Image.Picture.TransparencyMode := citNone;
  end;
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

procedure TMainForm.OpacitySpinEditChange(Sender: TObject);
begin
  Image.Opacity := OpacitySpinEdit.Value;
end;


initialization
  {$I fmain.lrs}

end.

