unit LuiDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, Controls, Forms, LCLType,
  LuiLCLInterfaces, fExportDataset;

type

  TExportDatasetDialogSave = procedure(Sender: TObject; Dataset: TDataset;
    FileTypeIndex: Integer; FieldList: TStrings; const FileName: String) of object;

  { TExportDatasetDialog }

  //todo: refactor into TCustomExportDataDialog
  TExportDatasetDialog = class(TComponent)
  private
    FActiveForm: TExportDatasetForm; //todo implement as an interface
    FDataset: TDataset;
    FExcludeFields: TStrings;
    FIncludeFields: TStrings;
    FOnSave: TExportDatasetDialogSave;
    FOutputDirectory: String;
    FOutputFilename: String;
    FOutputFileTypes: TStrings;
    function CheckFilePath(const FilePath: String): Boolean;
    procedure SaveEvent(Sender: TObject);
    procedure SetExcludeFields(const Value: TStrings);
    procedure SetIncludeFields(const Value: TStrings);
    procedure SetOutputFileTypes(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(FormOwner: TControl = nil): Boolean;
  published
    property Dataset: TDataset read FDataset write FDataset;
    property OnSave: TExportDatasetDialogSave read FOnSave write FOnSave;
    property OutputFileTypes: TStrings read FOutputFileTypes write SetOutputFileTypes;
    property OutputDirectory: String read FOutputDirectory write FOutputDirectory;
    property OutputFilename: String read FOutputFilename write FOutputFilename;
    property ExcludeFields: TStrings read FExcludeFields write SetExcludeFields;
    property IncludeFields: TStrings read FIncludeFields write SetIncludeFields;
  end;

procedure SetFrameActionState(Controller: IFrameController; const ActionId: String; Enabled: Boolean);

function ShowExportDatasetDlg(AOwner: TWinControl; Dataset: TDataSet): Boolean;

function ShowForm(FormClass: TFormClass): TModalResult;

function ShowForm(FormClass: TFormClass; FormProperties: array of const): TModalResult;

function ShowForm(FormClass: TFormClass; Owner: TWinControl): TModalResult;

function ShowForm(FormClass: TFormClass; Owner: TWinControl; FormProperties: array of const): TModalResult;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl): TModalResult;

function ShowFrame(FrameClass: TCustomFrameClass; Owner: TWinControl;
  FrameProperties: array of const; const Options: String = ''): TModalResult;

implementation

uses
  StrUtils,
  Dialogs,
  LuiRTTIUtils, LuiStrUtils, LuiMiscUtils, LuiDBExport,
  fFrameEditor, LazUTF8, LazFileUtils;

procedure SetFrameActionState(Controller: IFrameController;
  const ActionId: String; Enabled: Boolean);
const
  FrameActionMsgMap: array[Boolean] of String = ('disable-action', 'enable-action');
begin
  Controller.FrameMessage(nil, FrameActionMsgMap[Enabled], ActionId);
end;

function ShowExportDatasetDlg(AOwner: TWinControl; Dataset: TDataSet): Boolean;
begin
  Result := False;
  if Dataset = nil then
    Exception.Create('ShowExportDatasetDlg: Dataset can not be nil');
  Result := TExportDatasetForm.Execute(AOwner, Dataset);
end;

function ShowForm(FormClass: TFormClass): TModalResult;
begin
  Result := ShowForm(FormClass, Screen.ActiveForm);
end;

function ShowForm(FormClass: TFormClass; FormProperties: array of const): TModalResult;
begin
  Result := ShowForm(FormClass, Screen.ActiveForm, FormProperties);
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
    CallMethod(Form, 'InitControl');
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
  FrameProperties: array of const; const Options: String): TModalResult;
var
  Form: TFrameEditorForm;
begin
  Form := TFrameEditorForm.Create(Owner, FrameClass, FrameProperties, Options);
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

procedure TExportDatasetDialog.SetOutputFileTypes(const Value: TStrings);
begin
  FOutputFileTypes.Assign(Value);
end;

constructor TExportDatasetDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncludeFields := TStringList.Create;
  FExcludeFields := TStringList.Create;
  FOutputFileTypes := TStringList.Create;
end;

destructor TExportDatasetDialog.Destroy;
begin
  FOutputFileTypes.Destroy;
  FIncludeFields.Destroy;
  FExcludeFields.Destroy;
  inherited Destroy;
end;

function TExportDatasetDialog.Execute(FormOwner: TControl = nil): Boolean;
var
  i, j: Integer;
  FieldList, FileTypes: TStrings;
  FieldText, FieldName: String;
  Field: TField;
begin
  Result := False;
  if FDataset = nil then
    Exception.Create(Self.Name + ': Dataset can not be nil');

  FileTypes := TStringList.Create;
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

    LoadExporters(FOutputFileTypes, FileTypes);

    if (FormOwner = nil) and (Owner is TControl) then
      FormOwner := GetFirstParentForm(TControl(Owner));

    FActiveForm := TExportDatasetForm.Create(FormOwner);
    with FActiveForm do
    try
      Dataset := FDataset;
      FileTypeGroup.Items.Assign(FileTypes);
      FieldsCheckListBox.Items.Assign(FieldList);
      FileNameEdit.Text := OutputFilename;
      DirectoryEdit.Directory := IfThen(OutputDirectory <> '', OutputDirectory, ExtractFileDir(ParamStrUTF8(0)));
      SaveFileButton.OnClick := @SaveEvent;
      Result := ShowModal = mrOK;
    finally
      FreeAndNil(FActiveForm);
    end;

  finally
    FieldList.Destroy;
    FileTypes.Destroy;
  end;
end;

function TExportDatasetDialog.CheckFilePath(const FilePath: String): Boolean;
begin
  //todo make this optional
  Result := not FileExistsUTF8(FilePath);
  if not Result then
  begin
    Result := MessageDlg('Confirmar Substituição de Arquivo',
      'O arquivo ' + FilePath + ' já existe' + LineEnding +
      'Deseja substituir o arquivo existente?', mtConfirmation, mbYesNo, 0) = mrYes;
    if Result then
    begin
      Result := DeleteFileUTF8(FilePath);
      if not Result then
        ShowMessage('Não foi possível apagar o arquivo' + LineEnding +
          'Verifique se ele está sendo usado por outro programa');
    end;
  end;
end;


procedure TExportDatasetDialog.SaveEvent(Sender: TObject);
var
  Exporter: TCustomDatasetExporterClass;
  FileTypeIndex: Integer;
  FieldList: TStrings;
  FilePath: String;
begin
  if FActiveForm = nil then
    Exit;
  FileTypeIndex := FActiveForm.FileTypeGroup.ItemIndex;
  if FileTypeIndex <> -1 then
  begin
    Exporter := TCustomDatasetExporterClass(FActiveForm.FileTypeGroup.Items.Objects[FileTypeIndex]);
    FilePath := IncludeTrailingPathDelimiter(FActiveForm.DirectoryEdit.Directory) +
      FActiveForm.FileNameEdit.Text;
    if Exporter <> nil then
      FilePath := FilePath + Exporter.Extension;
    if not CheckFilePath(FilePath) then
      Exit;
    FieldList := TStringList.Create;
    try
      FActiveForm.GetCheckedFields(FieldList);
      if Exporter <> nil then
        Exporter.Save(FDataset, FieldList, FilePath)
      else
      begin
        if FOnSave <> nil then
          FOnSave(Self, FDataset, FileTypeIndex, FieldList, FilePath);
      end;
    finally
      FieldList.Destroy;
    end;
    FActiveForm.ModalResult := mrOK;
  end;
end;

procedure TExportDatasetDialog.SetExcludeFields(const Value: TStrings);
begin
  FExcludeFields.Assign(Value);
end;

finalization

end.

