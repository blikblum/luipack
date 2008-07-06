unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  VirtualTrees, StdCtrls, StringsController;

type

  { TMainForm }

  TMainForm = class(TForm)
    UpdateButton: TButton;
    EditStringsMemo: TMemo;
    TestTree: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    { private declarations }
    Controller: TStringsController;
    SourceStrings: TStrings;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Controller := TStringsController.Create(Self);
  //TMemo.Lines is buggy. Use a bridge list.
  //Controller.Strings := EditStringsMemo.Lines;
  SourceStrings := TStringList.Create;
  SourceStrings.Assign(EditStringsMemo.Lines);
  Controller.Strings := SourceStrings;
  Controller.Connect(TestTree);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SourceStrings.Destroy;
end;

procedure TMainForm.UpdateButtonClick(Sender: TObject);
begin
  SourceStrings.Assign(EditStringsMemo.Lines);
  Controller.UpdateTree;
end;

initialization
  {$I fmain.lrs}

end.

