unit ModelFieldEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, DataModel;

type

  { TModelFieldEditorForm }

  TModelFieldEditorForm = class(TForm)
    CancelButton: TBitBtn;
    DisplayLabelEdit: TTIEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SaveButton: TBitBtn;
    FieldNameEdit: TTIEdit;
    FieldTypeComboBox: TTIComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEditField: TDataModelField;
  public
    class function EditField(Field: TDataModelField): Boolean;
    class function AddField(Fields: TDataModelFields): Boolean;
  end;

var
  ModelFieldEditorForm: TModelFieldEditorForm;

implementation

uses
  DataHubUtils;

{$R *.lfm}

{ TModelFieldEditorForm }

procedure TModelFieldEditorForm.FormCreate(Sender: TObject);
begin
  FEditField := TDataModelField.Create(nil);
end;

procedure TModelFieldEditorForm.FormDestroy(Sender: TObject);
begin
  FEditField.Destroy;
end;

procedure TModelFieldEditorForm.FormShow(Sender: TObject);
begin
  SetRTTILinkObject(Self, FEditField);
end;

class function TModelFieldEditorForm.EditField(Field: TDataModelField): Boolean;
var
  Form: TModelFieldEditorForm;
begin
  Assert(Field <> nil, 'TModelFieldEditorForm.EditField Field = nil');
  Form := TModelFieldEditorForm.Create(nil);
  try
    Form.FEditField.Assign(Field);
    Result := Form.ShowModal = mrOK;
    if Result then
      Field.Assign(Form.FEditField);
  finally
    Form.Destroy;
  end;
end;

class function TModelFieldEditorForm.AddField(Fields: TDataModelFields): Boolean;
var
  Form: TModelFieldEditorForm;
  NewField: TDataModelField;
begin
  Assert(Fields <> nil, 'TModelFieldEditorForm.AddField Fields = nil');
  Form := TModelFieldEditorForm.Create(nil);
  try
    Form.FEditField.FieldName := 'NewField';
    Result := Form.ShowModal = mrOK;
    if Result then
    begin
      NewField := Fields.Add;
      NewField.Assign(Form.FEditField);
    end;
  finally
    Form.Destroy;
  end;
end;

end.

