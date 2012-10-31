unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  VirtualPages;

type

  { TMainViewForm }

  TMainViewForm = class(TForm)
    PageListBox: TListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FPages: TVirtualPages;
    procedure LoadPageControl(Sender: TObject; Page: TVirtualPage);
  public
    { public declarations }
  end;

var
  MainViewForm: TMainViewForm;

implementation

uses
  TestView1;

{$R *.lfm}

{ TMainViewForm }

procedure TMainViewForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FPages := TVirtualPages.Create;
  FPages.OnLoadControl := @LoadPageControl;
  FPages.DisplayOptions.Parent := Self;
  FPages.DisplayOptions.BorderSpacing.Around := 2;
  FPages.Add('Control', Panel1);
  FPages.Add('ControlClass', TTestView1Frame, ['Caption', 'Loaded from Class type']);
  FPages.Add('ControlClassName', 'TTestView2Frame', ['Caption', 'Loaded from ClassName']);
  FPages.Add('CustomSize', TTestView1Frame, ['Caption', 'Loaded with a custom size']);
  PageListBox.Clear;
  for i := 0 to FPages.Count - 1 do
    PageListBox.AddItem(FPages[i].Caption, nil);
end;

procedure TMainViewForm.FormDestroy(Sender: TObject);
begin
  FPages.Destroy;
end;

procedure TMainViewForm.PageListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  FPages.PageIndex := PageListBox.ItemIndex;
end;

procedure TMainViewForm.LoadPageControl(Sender: TObject; Page: TVirtualPage);
begin
  if Page.Caption = 'CustomSize' then
    Page.Control.BorderSpacing.Around := 40;
end;

end.

