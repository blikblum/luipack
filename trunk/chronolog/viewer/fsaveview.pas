unit fsaveview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfrmSaveView }

  TfrmSaveView = class(TForm)
    butOK: TButton;
    butCancel: TButton;
    editName: TEdit;
    editDescription: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure butCancelClick(Sender: TObject);
    procedure butOKClick(Sender: TObject);
  private
    { private declarations }
    function GetViewName: String;
    function GetDescription: String;
    procedure SetDataFile(const AValue: String);
  public
    { public declarations }
    property DataFile: String write SetDataFile;
    property ViewName: String read GetViewName;
    property Description: String read GetDescription;
  end; 

implementation

{ TfrmSaveView }

procedure TfrmSaveView.butOKClick(Sender: TObject);
begin
  if (editName.Text <> '') and (editDescription.Text<> '') then
    ModalResult:=mrOk
  else
    ShowMessage('Please insert a name and a description');
end;

procedure TfrmSaveView.butCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

function TfrmSaveView.GetViewName: String;
begin
  Result:=editName.Text;
end;

function TfrmSaveView.GetDescription: String;
begin
  Result:=editDescription.Text;
end;

procedure TfrmSaveView.SetDataFile(const AValue: String);
begin

end;

initialization
  {$I fsaveview.lrs}

end.

