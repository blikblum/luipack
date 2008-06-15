unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  StdCtrls, ExtCtrls, Spin;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonResetValues: TButton;
    CairoPaintBox: TCairoPaintBox;
    CheckDrawSharpLine: TCheckBox;
    CheckIndividualRadius: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EditWidth: TSpinEdit;
    EditY: TSpinEdit;
    EditHeight: TSpinEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditLineWidth: TSpinEdit;
    EditX: TSpinEdit;
    Label6: TLabel;
    EditTopLeftRadius: TSpinEdit;
    EditRadius: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    EditTopRightRadius: TSpinEdit;
    EditTopRadius: TSpinEdit;
    Label9: TLabel;
    EditLeftRadius: TSpinEdit;
    EditBottomRightRadius: TSpinEdit;
    EditBottomLeftRadius: TSpinEdit;
    EditBottomRadius: TSpinEdit;
    EditRightRadius: TSpinEdit;
    procedure ButtonResetValuesClick(Sender: TObject);
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure ValueChanged(Sender: TObject);
  private
    { private declarations }
    FUpdating: Boolean;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  CairoUtils;

{ TFormMain }

procedure TFormMain.ButtonResetValuesClick(Sender: TObject);
begin
  FUpdating := True;
  EditRadius.Value := 0;
  EditLineWidth.Value := 1;
  EditTopLeftRadius.Value := 0;
  EditTopRightRadius.Value := 0;
  EditBottomRightRadius.Value := 0;
  EditBottomLeftRadius.Value := 0;
  EditBottomRadius.Value := 0;
  EditTopRadius.Value := 0;
  EditRightRadius.Value := 0;
  EditLeftRadius.Value := 0;
  FUpdating := False;
  ValueChanged(nil);
end;

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
var
  RoundedInfo: TRoundedRectInfo;
  R: TDoubleRect;
begin
  with CairoPaintBox.Context do
  begin
    SetSourceRgb(1, 1, 1);
    Paint;
    LineWidth := EditLineWidth.Value;
    if CheckDrawSharpLine.Checked then
      CalculateSharpRect(EditX.Value, EditY.Value,
        EditWidth.Value, EditHeight.Value,
        EditLineWidth.Value, R)
    else
    begin
      R.Left := EditX.Value;
      R.Top := EditY.Value;
      R.Right := EditWidth.Value;
      R.Bottom := EditHeight.Value;
    end;
    if not CheckIndividualRadius.Checked then
    begin
      RoundedRectangle(CairoPaintBox.Context, R, EditRadius.Value);
    end
    else
    begin
      FillChar(RoundedInfo, Sizeof(RoundedInfo), 0);
      //corners
      RoundedInfo.BottomLeftRadius := EditBottomLeftRadius.Value;
      RoundedInfo.BottomRightRadius := EditBottomRightRadius.Value;
      RoundedInfo.TopLeftRadius := EditTopLeftRadius.Value;
      RoundedInfo.TopRightRadius := EditTopRightRadius.Value;
      //sides
      RoundedInfo.BottomRadius := EditBottomRadius.Value;
      RoundedInfo.TopRadius := EditTopRadius.Value;
      RoundedInfo.RightRadius := EditRightRadius.Value;
      RoundedInfo.LeftRadius := EditLeftRadius.Value;
      RoundedRectangle(CairoPaintBox.Context, R, RoundedInfo);
    end;
    SetSourceRgb(0, 0, 0);
    Stroke;
  end;
end;

procedure TFormMain.ValueChanged(Sender: TObject);
begin
  if not FUpdating then
    CairoPaintBox.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

