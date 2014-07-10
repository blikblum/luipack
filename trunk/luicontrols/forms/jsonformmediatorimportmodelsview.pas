unit JSONFormMediatorImportModelsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  EditBtn, StdCtrls, VirtualTrees, DataModel, JSONFormMediator, DataHubProject,
  CollectionVirtualTreeMediator;

type

  { TJSONFormMediatorImportModelsForm }

  TJSONFormMediatorImportModelsForm = class(TForm)
    CancelButton: TBitBtn;
    ModelsFileNameEdit: TFileNameEdit;
    ImportButton: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ModelFieldsLabel: TLabel;
    ModelListView: TVirtualStringTree;
    FieldListView: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure ModelListViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ModelsFileNameEditAcceptFileName(Sender: TObject;
      var Value: String);
  private
    FSelectedModel: TDataModel;
    FDataHubProject: TDataHubProject;
    FFieldsMediator: TCollectionVirtualTreeMediator;
    FMediator: TJSONFormMediator;
    FModelsMediator: TCollectionVirtualTreeMediator;
    procedure LoadDefaultModelDefs;
    procedure ModelDefsLoad(Sender: TObject);
    procedure SetSelectedModel(AValue: TDataModel);
    property SelectedModel: TDataModel read FSelectedModel write SetSelectedModel;
  public
    constructor Create(TheOwner: TComponent); override;
    property Mediator: TJSONFormMediator read FMediator write FMediator;
  end;

var
  JSONFormMediatorImportModelsForm: TJSONFormMediatorImportModelsForm;

implementation

uses
  LazIDEIntf, LazFileUtils, JSONElementUpdateMethodView;

{$R *.lfm}

function GetUpdateMethod(const PropertyName: String; var DefaultMethod: Integer): Integer;
var
  Form: TJSONElementUpdateMethodForm;
begin
  Form := TJSONElementUpdateMethodForm.Create(nil);
  try
    Form.PropertyName := PropertyName;
    Form.ShowModal;
    Result := Form.UpdateMethod;
    if Form.UseAsDefault then
      DefaultMethod := Result;
  finally
    Form.Destroy;
  end;
end;

procedure LoadElementsFromFields(Elements: TJSONFormElements; Fields: TDataModelFields);
var
  PropertyName: String;
  Field: TDataModelField;
  i, j, UpdateMethod, DefaultMethod: Integer;
  Element: TJSONFormElement;
begin
  DefaultMethod := -1;
  Elements.BeginUpdate;
  try
    for i := 0 to Fields.Count - 1 do
    begin
      Field := Fields[i];
      Element := Elements.FindElement(Field.FieldName, True);
      if Element <> nil then
      begin
        if DefaultMethod = -1 then
          UpdateMethod := GetUpdateMethod(PropertyName, DefaultMethod)
        else
          UpdateMethod := DefaultMethod;
        {0: skip / 1: merge / 2: overwrite}
        case UpdateMethod of
          1:
          begin
            if Element.Caption = '' then
              Element.Caption := Field.DisplayLabel;
          end;
          2:
          begin
            Element.Caption := Field.DisplayLabel;
          end;
        end;
      end
      else
      begin
        Element := Elements.Add;
        Element.Name := PropertyName;
        Element.PropertyName := PropertyName;
      end;
    end;
  finally
    Elements.EndUpdate;
  end;
end;

{ TJSONFormMediatorImportModelsForm }

procedure TJSONFormMediatorImportModelsForm.ModelsFileNameEditAcceptFileName(
  Sender: TObject; var Value: String);
begin
  try
    FDataHubProject.Models.BeginUpdate;
    try
      FDataHubProject.LoadFromFile(Value);
    finally
      FDataHubProject.Models.EndUpdate;
    end;
  except
    on E: Exception do
    begin
      ShowMessageFmt('Error loading definition file: %s', [E.Message]);
      Value := '';
    end;
  end;
end;

procedure TJSONFormMediatorImportModelsForm.SetSelectedModel(AValue: TDataModel);
begin
  if FSelectedModel = AValue then Exit;
  FSelectedModel := AValue;
  if FSelectedModel = nil then
  begin
    ModelFieldsLabel.Caption := '';
    FFieldsMediator.Collection := nil
  end
  else
  begin
    ModelFieldsLabel.Caption := Format('%s Fields', [FSelectedModel.Name]);
    FFieldsMediator.Collection := FSelectedModel.Fields;
  end;
end;

procedure TJSONFormMediatorImportModelsForm.FormShow(Sender: TObject);
begin
  LoadDefaultModelDefs;
end;

procedure TJSONFormMediatorImportModelsForm.ImportButtonClick(Sender: TObject);
var
  CheckedFields: TDataModelFields;
begin
  CheckedFields := TDataModelFields.Create(nil);
  try
    FFieldsMediator.LoadCheckedItems(CheckedFields);
    LoadElementsFromFields(Mediator.Elements, CheckedFields);
  finally
    CheckedFields.Destroy;
  end;
end;

procedure TJSONFormMediatorImportModelsForm.ModelListViewFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  SelectedModel := TDataModel(FModelsMediator.GetCollectionItem(Node));
end;

procedure TJSONFormMediatorImportModelsForm.ModelDefsLoad(Sender: TObject);
begin
  if FDataHubProject.Models.Count > 0 then
  begin
    SelectedModel := FDataHubProject.Models[0];
    ModelListView.FocusedNode := ModelListView.GetFirst;
    ModelListView.Selected[ModelListView.GetFirst] := True;
  end;
end;

procedure TJSONFormMediatorImportModelsForm.LoadDefaultModelDefs;
var
  DefaultModelsFileName: String;
  DefaultPath: String;
begin
  //todo: store model path in project config
  DefaultPath := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename);
  DefaultModelsFileName := DefaultPath + 'models.json';
  ModelsFileNameEdit.InitialDir := DefaultPath;
  if FileExistsUTF8(DefaultModelsFileName) then
    ModelsFileNameEdit.FileName := DefaultModelsFileName;
end;

constructor TJSONFormMediatorImportModelsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataHubProject := TDataHubProject.Create(Self);
  FDataHubProject.OnLoad := @ModelDefsLoad;
  FFieldsMediator := TCollectionVirtualTreeMediator.Create(Self);
  FFieldsMediator.DefaultProperty := 'FieldName';
  FFieldsMediator.Tree := FieldListView;
  FModelsMediator := TCollectionVirtualTreeMediator.Create(Self);
  FModelsMediator.DefaultProperty := 'Name';
  FModelsMediator.Collection := FDataHubProject.Models;
  FModelsMediator.Tree := ModelListView;
end;

end.

