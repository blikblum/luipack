unit LuiDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, Controls, Forms, LCLType;

type

  { TExportDatasetDialog }

  TExportDatasetDialog = class(TComponent)
  private
    FDataset: TDataset;
    FExcludeFields: TStrings;
    FIncludeFields: TStrings;
    FOutputDirectory: String;
    FOutputFilename: String;
    procedure SetExcludeFields(const Value: TStrings);
    procedure SetIncludeFields(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(FormOwner: TControl = nil): Boolean;
  published
    property Dataset: TDataset read FDataset write FDataset;
    property OutputDirectory: String read FOutputDirectory write FOutputDirectory;
    property OutputFilename: String read FOutputFilename write FOutputFilename;
    property ExcludeFields: TStrings read FExcludeFields write SetExcludeFields;
    property IncludeFields: TStrings read FIncludeFields write SetIncludeFields;
  end;

function ShowExportDatasetDlg(AOwner: TWinControl; Dataset: TDataSet): Boolean;

function ShowForm(FormClass: TFormClass; Owner: TWinControl): TModalResult;

function ShowForm(FormClass: TFormClass; Owner: TWinControl; FormProperties: array of const): TModalResult;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl): TModalResult;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl;
  FrameProperties: array of const; ButtonCaptions: String = ''): TModalResult;

implementation

uses
  fExportDataset, fFrameEditor,
  StrUtils, FileUtil, LuiRTTIUtils, LuiStrUtils;

function ShowExportDatasetDlg(AOwner: TWinControl; Dataset: TDataSet): Boolean;
begin
  Result := False;
  if Dataset = nil then
    Exception.Create('ShowExportDatasetDlg: Dataset can not be nil');
  Result := TExportDatasetForm.Execute(AOwner, Dataset);
end;

function ShowForm(FormClass: TFormClass; Owner: TWinControl): TModalResult;
begin
  Result := ShowForm(FormClass, Owner, []);
end;

function ShowForm(FormClass: TFormClass; Owner: TWinControl;
  FormProperties: array of const): TModalResult;
var
  Form: TForm;
begin
  Form := FormClass.Create(Owner);
  try
    SetObjectProperties(Form, FormProperties);
    Result := Form.ShowModal;
  finally
    Form.Destroy;
  end;
end;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl): TModalResult;
begin
  Result := ShowFrame(FrameClass, Owner, []);
end;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl;
  FrameProperties: array of const; ButtonCaptions: String): TModalResult;
var
  Form: TFrameEditorForm;
begin
  Form := TFrameEditorForm.Create(Owner, FrameClass, FrameProperties, ButtonCaptions);
  try
    Result := Form.ShowModal;
  finally
    Form.Destroy;
  end;
end;

{ TExportDatasetDialog }

procedure TExportDatasetDialog.SetIncludeFields(const Value: TStrings);
begin
  FIncludeFields.Assign(Value);
end;

constructor TExportDatasetDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncludeFields := TStringList.Create;
  FExcludeFields := TStringList.Create;
end;

destructor TExportDatasetDialog.Destroy;
begin
  FIncludeFields.Destroy;
  FExcludeFields.Destroy;
  inherited Destroy;
end;

function TExportDatasetDialog.Execute(FormOwner: TControl = nil): Boolean;
var
  i, j: Integer;
  FieldList: TStrings;
  FieldText, FieldName: String;
  Field: TField;
begin
  Result := False;
  if FDataset = nil then
    Exception.Create(Self.Name + ': Dataset can not be nil');

  FieldList := TStringList.Create;
  try
    for i := 0 to FIncludeFields.Count - 1 do
    begin
      ExtractNameValue(FIncludeFields[i], FieldName, FieldText);
      Field := FDataset.FindField(FieldName);
      if Field <> nil then
        FieldList.AddObject(IfThen(FieldText = '', Field.DisplayLabel, FieldText), Field);
    end;
    if FieldList.Count = 0 then
      for i := 0 to FDataset.Fields.Count - 1 do
        FieldList.AddObject(FDataset.Fields[i].FieldName, FDataset.Fields[i]);
    for i := 0 to FExcludeFields.Count - 1 do
    begin
      j := FieldList.IndexOf(FExcludeFields[i]);
      if j <> -1 then
        FieldList.Delete(j);
    end;

    if (FormOwner = nil) and (Owner is TControl) then
      FormOwner := GetFirstParentForm(TControl(Owner));

    with TExportDatasetForm.Create(FormOwner) do
    try
      Dataset := FDataset;
      FieldsCheckListBox.Items.Assign(FieldList);
      FileNameEdit.Text := OutputFilename;
      DirectoryEdit.Directory := IfThen(OutputDirectory <> '', OutputDirectory, ExtractFileDir(ParamStrUTF8(0)));
      Result := ShowModal = mrOK;
    finally
      Destroy;
    end;

  finally
    FieldList.Destroy;
  end;
end;

procedure TExportDatasetDialog.SetExcludeFields(const Value: TStrings);
begin
  FExcludeFields.Assign(Value);
end;

end.

