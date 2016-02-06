unit ModelEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, DataModel;

type

  { TModelEditorForm }

  TModelEditorForm = class(TForm)
    CloseButton: TBitBtn;
    Label1: TLabel;
    FieldsDefinitinsMemo: TMemo;
    Label2: TLabel;
    SaveButton: TBitBtn;
    NameEdit: TTIEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FEditModel: TDataModel;
  public
    class function EditModel(Model: TDataModel): Boolean;
    class function AddModel(Models: TDataModels): Boolean;
  end;

var
  ModelEditorForm: TModelEditorForm;

implementation

uses
  DataHubUtils, QuickDefinitionParser, fpjson, fpjsonrtti;

{$R *.lfm}

{ TModelEditorForm }

procedure TModelEditorForm.FormCreate(Sender: TObject);
begin
  FEditModel := TDataModel.Create(nil);
end;

procedure TModelEditorForm.FormDestroy(Sender: TObject);
begin
  FEditModel.Destroy;
end;

procedure TModelEditorForm.FormShow(Sender: TObject);
begin
  SetRTTILinkObject(Self, FEditModel);
end;

procedure TModelEditorForm.SaveButtonClick(Sender: TObject);
var
  Parser: TQuickFieldsDefinitionParser;
  Data: TJSONArray;
  DeStreamer: TJSONDeStreamer;
  CanSave: Boolean;
  i: Integer;
begin
  CanSave := True;
  Parser := TQuickFieldsDefinitionParser.Create(FieldsDefinitinsMemo.Lines);
  try
    try
      Data := Parser.Parse;
      if Data.Count > 0 then
      begin
        DeStreamer := TJSONDeStreamer.Create(nil);
        try
          for i := 0 to Data.Count - 1 do
            DeStreamer.JSONToObject(Data.Objects[i], FEditModel.Fields.Add);
        finally
          DeStreamer.Destroy;
        end;
      end;
    except
      on E:Exception do
      begin
        ShowMessage(E.Message);
        CanSave := False;
      end;
    end;
  finally
    Parser.Destroy;
  end;
  if CanSave then
    ModalResult := mrOK;
end;

class function TModelEditorForm.EditModel(Model: TDataModel): Boolean;
var
  Form: TModelEditorForm;
begin
  Form := TModelEditorForm.Create(nil);
  try
    Form.FEditModel.Assign(Model);
    Result := Form.ShowModal = mrOK;
    if Result then
      Model.Assign(Form.FEditModel);
  finally
    Form.Destroy;
  end;
end;

class function TModelEditorForm.AddModel(Models: TDataModels): Boolean;
var
  Form: TModelEditorForm;
begin
  Form := TModelEditorForm.Create(nil);
  try
    Result := Form.ShowModal = mrOK;
    if Result then
      Models.Add.Assign(Form.FEditModel);
  finally
    Form.Destroy;
  end;
end;

end.

