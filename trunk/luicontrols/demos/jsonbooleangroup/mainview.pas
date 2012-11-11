unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  JSONMediators, fpjson;

type

  { TMainViewForm }

  TMainViewForm = class(TForm)
    BooleanGroupMediator: TJSONBooleanGroupMediator;
    SaveDataButton: TButton;
    ChildSizingCheckBox: TCheckBox;
    Panel1: TPanel;
    procedure ChildSizingCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveDataButtonClick(Sender: TObject);
  private
    FData: TJSONObject;
  public

  end;

var
  MainViewForm: TMainViewForm;

implementation

{$R *.lfm}

{ TMainViewForm }

procedure TMainViewForm.FormCreate(Sender: TObject);
begin
  FData := TJSONObject.Create(['prop1', True, 'prop3', True, 'propstr', 'XXX','prop5', 23]);
  BooleanGroupMediator.LoadData(FData);
end;

procedure TMainViewForm.ChildSizingCheckBoxChange(Sender: TObject);
begin
  if ChildSizingCheckBox.Checked then
    Panel1.ChildSizing.Layout := cclLeftToRightThenTopToBottom
  else
    Panel1.ChildSizing.Layout := cclNone;
end;

procedure TMainViewForm.FormDestroy(Sender: TObject);
begin
  FData.Destroy;
end;

procedure TMainViewForm.SaveDataButtonClick(Sender: TObject);
begin
  BooleanGroupMediator.SaveData(FData);
  ShowMessage(FData.FormatJSON);
end;

end.

