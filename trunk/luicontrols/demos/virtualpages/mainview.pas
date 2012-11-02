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
    VirtualPageManager1: TVirtualPageManager;
    procedure FormCreate(Sender: TObject);
    procedure LoadPage(Sender: TObject; Page: TVirtualPage);
    procedure PageListBoxSelectionChange(Sender: TObject; User: boolean);
  private
  public
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
  Page: TVirtualPage;
begin
  VirtualPageManager1.DisplayOptions.Parent := Self;
  //set dynamic values (only possible in runtime)
  Page := VirtualPageManager1.Pages.PageByName('class');
  Page.ControlClass := TTestView1Frame;
  Page.SetProperties(['Caption', 'Loaded using TControlClass']);
  Page := VirtualPageManager1.Pages.PageByName('customsize');
  Page.ControlClass := TTestView1Frame;
  Page.SetProperties(['Caption', 'Loaded with a custom size']);
  Page := VirtualPageManager1.Pages.PageByName('classname');
  Page.SetProperties(['Caption', 'Loaded using class name']);
  PageListBox.Items.Assign(VirtualPageManager1.Pages);
end;

procedure TMainViewForm.PageListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  VirtualPageManager1.PageIndex := PageListBox.ItemIndex;
end;

procedure TMainViewForm.LoadPage(Sender: TObject; Page: TVirtualPage);
begin
  if Page.Name = 'customsize' then
    Page.Control.BorderSpacing.Around := 40;
end;

end.

