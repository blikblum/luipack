unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ToggleLabel, MenuButton, StdCtrls, Menus, Buttons, SearchEdit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    NotebookMain: TNotebook;
    PageSearchEdit: TPage;
    PageMenuButton: TPage;
    PageToggleLabel: TPage;
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
    procedure MenuButton1Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure ToggleLabel2Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

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

procedure TfrmMain.MenuItem15Click(Sender: TObject);
begin
  with TMenuItem(Sender) do
    MenuButton5.Caption := Caption;
end;

initialization
  {$I fmain.lrs}

end.

