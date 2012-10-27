unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ToggleLabel, MenuButton, StdCtrls, Menus, Buttons, SearchEdit, AdvancedLabel,
  ComCtrls, Spin, DropDownBaseButtons;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ToggleAdvancedLabelHotTrackCheckBox: TCheckBox;
    ExampleAdvancedLabel: TAdvancedLabel;
    AutoLinkAdvancedLabel: TAdvancedLabel;
    ToggleLinkCheckBox: TCheckBox;
    Label4: TLabel;
    SetSearchEditTextButton: TButton;
    CheckUnderline: TCheckBox;
    CheckBold: TCheckBox;
    CheckPopupOnMouseUp: TCheckBox;
    CheckExecuteEmpty: TCheckBox;
    CheckExecuteKillFocus: TCheckBox;
    ButtonLabelColor: TColorButton;
    ButtonArrowColor: TColorButton;
    ButtonHighlightColor: TColorButton;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MemoSearchLog: TMemo;
    MenuButton1: TMenuButton;
    MenuButton2: TMenuButton;
    MenuButton3: TMenuButton;
    MenuButton4: TMenuButton;
    MenuButton5: TMenuButton;
    MenuButton6: TMenuButton;
    MenuButton7: TMenuButton;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MainPageControl: TPageControl;
    SearchEditTab: TTabSheet;
    MenuButtonTab: TTabSheet;
    MenuButtonMarginEdit: TSpinEdit;
    AdvancedLabelSheet: TTabSheet;
    ToggleLabelTab: TTabSheet;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    PopupMenu5: TPopupMenu;
    PopupMenu6: TPopupMenu;
    PopupMenu7: TPopupMenu;
    SearchEdit1: TSearchEdit;
    ToggleLabel1: TToggleLabel;
    ToggleLabel2: TToggleLabel;
    procedure AutoLinkAdvancedLabelClick(Sender: TObject);
    procedure ButtonArrowColorColorChanged(Sender: TObject);
    procedure ButtonHighlightColorColorChanged(Sender: TObject);
    procedure ButtonLabelColorColorChanged(Sender: TObject);
    procedure CheckBoldChange(Sender: TObject);
    procedure CheckExecuteEmptyClick(Sender: TObject);
    procedure CheckExecuteKillFocusClick(Sender: TObject);
    procedure CheckPopupOnMouseUpChange (Sender: TObject );
    procedure CheckUnderlineChange(Sender: TObject);
    procedure MenuButton1Click(Sender: TObject);
    procedure MenuButtonMarginEditChange(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure SearchEdit1Execute(Sender: TObject);
    procedure SetSearchEditTextButtonClick(Sender: TObject);
    procedure ToggleAdvancedLabelHotTrackCheckBoxChange(Sender: TObject);
    procedure ToggleLinkCheckBoxChange(Sender: TObject);
    procedure ToggleLabel2Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.ToggleLabel2Change(Sender: TObject);
begin
  Panel1.Visible := ToggleLabel2.Expanded;
end;

procedure TfrmMain.MenuButton1Click(Sender: TObject);
begin
  with TMenuButton(Sender) do
    ShowMessage('Clicked - (Flat: ' + BoolToStr(Flat, True)+ ')');
end;

procedure TfrmMain.MenuButtonMarginEditChange(Sender: TObject);
begin
  MenuButton5.Margin := MenuButtonMarginEdit.Value;
  MenuButton6.Margin := MenuButtonMarginEdit.Value;
end;

procedure TfrmMain.CheckExecuteEmptyClick(Sender: TObject);
begin
  if CheckExecuteEmpty.Checked then
    SearchEdit1.Options := SearchEdit1.Options + [seoExecuteEmpty]
  else
    SearchEdit1.Options := SearchEdit1.Options - [seoExecuteEmpty];
end;

procedure TfrmMain.ButtonLabelColorColorChanged(Sender: TObject);
begin
  ToggleLabel1.Color := ButtonLabelColor.ButtonColor;
  ToggleLabel2.Color := ButtonLabelColor.ButtonColor;
end;

procedure TfrmMain.CheckBoldChange(Sender: TObject);
begin
  if CheckBold.Checked then
  begin
    ToggleLabel1.Options := ToggleLabel1.Options + [tloBoldOnHover];
    ToggleLabel2.Options := ToggleLabel2.Options + [tloBoldOnHover];
  end
  else
  begin
    ToggleLabel1.Options := ToggleLabel1.Options - [tloBoldOnHover];
    ToggleLabel2.Options := ToggleLabel2.Options - [tloBoldOnHover];
  end;
end;

procedure TfrmMain.ButtonArrowColorColorChanged(Sender: TObject);
begin
  ToggleLabel1.ArrowColor := ButtonArrowColor.ButtonColor;
  ToggleLabel2.ArrowColor := ButtonArrowColor.ButtonColor;
end;

procedure TfrmMain.AutoLinkAdvancedLabelClick(Sender: TObject);
begin
  ShowMessage('AutoLink');
end;

procedure TfrmMain.ButtonHighlightColorColorChanged(Sender: TObject);
begin
  ToggleLabel1.HighlightColor := ButtonHighlightColor.ButtonColor;
  ToggleLabel2.HighlightColor := ButtonHighlightColor.ButtonColor;
end;

procedure TfrmMain.CheckExecuteKillFocusClick(Sender: TObject);
begin
  if CheckExecuteKillFocus.Checked then
    SearchEdit1.Options := SearchEdit1.Options + [seoExecuteOnKillFocus]
  else
    SearchEdit1.Options := SearchEdit1.Options - [seoExecuteOnKillFocus];
end;

procedure TfrmMain.CheckPopupOnMouseUpChange (Sender: TObject );
var
  i: Integer;
  AControl: TControl;
begin
  for i:= 0 to MenuButtonTab.ControlCount - 1 do
  begin
    AControl := MenuButtonTab.Controls[i];
    if AControl is TMenuButton then
    begin
      if CheckPopupOnMouseUp.Checked then
        TMenuButton(AControl).Options := TMenuButton(AControl).Options + [dboPopupOnMouseUp]
      else
        TMenuButton(AControl).Options := TMenuButton(AControl).Options - [dboPopupOnMouseUp];
    end;
  end;
end;

procedure TfrmMain.CheckUnderlineChange(Sender: TObject);
begin
  if CheckUnderline.Checked then
  begin
    ToggleLabel1.Options := ToggleLabel1.Options + [tloUnderlineOnHover];
    ToggleLabel2.Options := ToggleLabel2.Options + [tloUnderlineOnHover];
  end
  else
  begin
    ToggleLabel1.Options := ToggleLabel1.Options - [tloUnderlineOnHover];
    ToggleLabel2.Options := ToggleLabel2.Options - [tloUnderlineOnHover];
  end;
end;

procedure TfrmMain.MenuItem15Click(Sender: TObject);
begin
  with TMenuItem(Sender) do
    MenuButton5.Caption := Caption;
end;

procedure TfrmMain.SearchEdit1Execute(Sender: TObject);
begin
  MemoSearchLog.Lines.Add('Search Executed - Text: "' + SearchEdit1.Text + '"');
end;

procedure TfrmMain.SetSearchEditTextButtonClick(Sender: TObject);
begin
  SearchEdit1.Text := 'Text Set Through Code';
end;

procedure TfrmMain.ToggleAdvancedLabelHotTrackCheckBoxChange(Sender: TObject);
begin
  ExampleAdvancedLabel.HotTrack := not ExampleAdvancedLabel.HotTrack;
end;

procedure TfrmMain.ToggleLinkCheckBoxChange(Sender: TObject);
begin
  ExampleAdvancedLabel.Link := not ExampleAdvancedLabel.Link;
end;

end.

