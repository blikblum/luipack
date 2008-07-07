program luiimage_demo_fpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, gui_form, gui_tab, gui_combobox, gui_label, gui_panel,
  gui_edit, gui_button, gui_dialogs, LuiImage, gfxbase, gui_checkbox;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: MainForm}
    pcProperties: TfpgPageControl;
    tsSize: TfpgTabSheet;
    cbViewStyle: TfpgComboBox;
    lblName1: TfpgLabel;
    pnlImage: TfpgPanel;
    edtFileName: TfpgEdit;
    lblName2: TfpgLabel;
    btnOpenFile: TfpgButton;
    Image1: TLuiImage;
    edtWidth: TfpgEditInteger;
    edtHeight: TfpgEditInteger;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    cbAutoSize: TfpgCheckBox;
    cbAspectRatio: TfpgCheckBox;
    bvlName1: TfpgBevel;
    lblName5: TfpgLabel;
    edtScaleHorizontal: TfpgEditFloat;
    edtScaleVertical: TfpgEditFloat;
    lblName6: TfpgLabel;
    lblName7: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    procedure btnOpenFileClick(Sender: TObject);
    procedure SizePropertyChanged(Sender: TObject);
    procedure ViewStyleChanged(Sender: TObject);
    procedure AfterCreate; override;
    // not GUI events
    procedure LoadImage;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnOpenFileClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.Filter := 'Image Files (*.bmp;*.jpg;*.jpeg;*.png)|*.bmp;*.jpg;*.jpeg;*.png|All Files (*)|*';
    if dlg.RunOpenFile then
    begin
      edtFileName.Text := dlg.FileName;
      LoadImage;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.SizePropertyChanged(Sender: TObject);
begin
  with Image1 do
  begin
    BeginUpdate;

    if cbAutoSize.Checked then
      Options := Options + [lioAutoSize]
    else
      Options := Options - [lioAutoSize];

    if cbAspectRatio.Checked then
      Options := Options + [lioKeepAspectOnStretch]
    else
      Options := Options - [lioKeepAspectOnStretch];

    if edtScaleHorizontal.Value > 0 then
      ScaleFactor.Horizontal := edtScaleHorizontal.Value;

    if edtScaleVertical.Value > 0 then
      ScaleFactor.Vertical := edtScaleVertical.Value;

    SetPosition(Left, Top, edtWidth.Value, edtHeight.Value);
    EndUpdate;
  end;
end;

procedure TMainForm.ViewStyleChanged(Sender: TObject);
begin
  case cbViewStyle.FocusItem of
    0: Image1.ViewStyle := livNormal;
    1: Image1.ViewStyle := livFitImage;
    2: Image1.ViewStyle := livScale;
    3: Image1.ViewStyle := livStretch;
    4: Image1.ViewStyle := livTile;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(432, 298, 728, 514);
  WindowTitle := 'LuiImage Demo (fpGui)';

  pcProperties := TfpgPageControl.Create(self);
  with pcProperties do
  begin
    Name := 'pcProperties';
    SetPosition(4, 64, 218, 432);
  end;

  tsSize := TfpgTabSheet.Create(pcProperties);
  with tsSize do
  begin
    Name := 'tsSize';
    SetPosition(4, 24, 208, 392);
    Text := 'Size';
  end;

  cbViewStyle := TfpgComboBox.Create(self);
  with cbViewStyle do
  begin
    Name := 'cbViewStyle';
    SetPosition(76, 36, 140, 22);
    FontDesc := '#List';
    Items.Add('Normal');
    Items.Add('Fit Image');
    Items.Add('Scale');
    Items.Add('Stretch');
    Items.Add('Tile');
    FocusItem := 0;
    TabOrder := 1;
    OnChange := @ViewStyleChanged;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 40, 68, 20);
    FontDesc := '#Label1';
    Text := 'View Style';
  end;

  pnlImage := TfpgPanel.Create(self);
  with pnlImage do
  begin
    Name := 'pnlImage';
    SetPosition(248, 68, 460, 436);
    Text := '';
  end;

  edtFileName := TfpgEdit.Create(self);
  with edtFileName do
  begin
    Name := 'edtFileName';
    SetPosition(76, 8, 380, 22);
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 12, 60, 16);
    FontDesc := '#Label1';
    Text := 'Image File';
  end;

  btnOpenFile := TfpgButton.Create(self);
  with btnOpenFile do
  begin
    Name := 'btnOpenFile';
    SetPosition(460, 8, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := 1;
    ImageName := 'stdimg.open';
    TabOrder := 6;
    OnClick := @btnOpenFileClick;
  end;

  Image1 := TLuiImage.Create(pnlImage);
  with Image1 do
  begin
    Name := 'Image1';
    //todo: implement color choose
    Colors.Background := clWhite;

    SetPosition(12, 12, 336, 312);
  end;

  edtWidth := TfpgEditInteger.Create(tsSize);
  with edtWidth do
  begin
    Name := 'edtWidth';
    SetPosition(64, 24, 96, 24);
    Value := Image1.Width;
    OnChange := @SizePropertyChanged;
  end;

  edtHeight := TfpgEditInteger.Create(tsSize);
  with edtHeight do
  begin
    Name := 'edtHeight';
    SetPosition(64, 52, 96, 24);
    Value := Image1.Height;
    OnChange := @SizePropertyChanged;
  end;

  lblName3 := TfpgLabel.Create(tsSize);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(24, 28, 36, 16);
    FontDesc := '#Label1';
    Text := 'Width';
  end;

  lblName4 := TfpgLabel.Create(tsSize);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(16, 56, 40, 16);
    FontDesc := '#Label1';
    Text := 'Height';
  end;

  cbAutoSize := TfpgCheckBox.Create(tsSize);
  with cbAutoSize do
  begin
    Name := 'cbAutoSize';
    SetPosition(4, 104, 120, 20);
    FontDesc := '#Label1';
    TabOrder := 4;
    Text := 'Auto Size';
    OnChange := @SizePropertyChanged;
  end;

  cbAspectRatio := TfpgCheckBox.Create(tsSize);
  with cbAspectRatio do
  begin
    Name := 'cbAspectRatio';
    SetPosition(4, 124, 200, 20);
    FontDesc := '#Label1';
    TabOrder := 5;
    Text := 'Keep Aspect Ratio On Stretch';
    OnChange := @SizePropertyChanged;
  end;

  bvlName1 := TfpgBevel.Create(tsSize);
  with bvlName1 do
  begin
    Name := 'bvlName1';
    SetPosition(8, 168, 184, 108);
    Style := bsLowered;
    Shape := bsFrame;
  end;

  lblName5 := TfpgLabel.Create(tsSize);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(8, 148, 80, 16);
    FontDesc := '#Label1';
    Text := 'Scale Factor';
  end;

  edtScaleHorizontal := TfpgEditFloat.Create(bvlName1);
  with edtScaleHorizontal do
  begin
    Name := 'edtScaleHorizontal';
    SetPosition(72, 12, 60, 24);
    Value := 1;
    OnChange := @SizePropertyChanged;
  end;

  edtScaleVertical := TfpgEditFloat.Create(bvlName1);
  with edtScaleVertical do
  begin
    Name := 'edtScaleVertical';
    SetPosition(72, 56, 60, 24);
    Value := 1;
    OnChange := @SizePropertyChanged;
  end;

  lblName6 := TfpgLabel.Create(bvlName1);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(8, 16, 60, 16);
    FontDesc := '#Label1';
    Text := 'Horizontal';
  end;

  lblName7 := TfpgLabel.Create(bvlName1);
  with lblName7 do
  begin
    Name := 'lblName7';
    SetPosition(20, 60, 52, 16);
    FontDesc := '#Label1';
    Text := 'Vertical';
  end;

  {@VFD_BODY_END: MainForm}
end;

procedure TMainForm.LoadImage;
begin
  Image1.Picture.LoadFromFile(edtFileName.Text);
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

