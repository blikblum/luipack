unit DataModelImporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataModel, db;

const
  FieldTypeMap : Array [TFieldType] of TDataModelFieldType =
    (
      dftString,
      dftString,
      dftInteger,
      dftInteger,
      dftInteger,
      dftBoolean,
      dftFloat,
      dftCurrency,
      dftString, {BCD}
      dftDate,
      dftTime,
      dftDateTime,
      dftString, {bytes}
      dftString, {varbytes}
      dftInteger, {autoinc}
      dftBlob,
      dftMemo,
      dftBlob, {graphic}
      dftMemo,
      dftString,
      dftString,
      dftString,
      dftString,
      dftString,
      dftString,
      dftLargeInt,
      dftString,
      dftString,
      dftString,
      dftString,
      dftBlob,
      dftBlob,
      dftString,
      dftString,
      dftString,
      dftString,
      dftDateTime,
      dftString,
      dftString,
      dftMemo
    );

type

  TDataModelImporterClass = class of TDataModelImporter;

  { TDataModelImporter }

  TDataModelImporter = class
  public
    class function Description: String; virtual; abstract;
    class procedure Execute(Models: TDataModels); virtual; abstract;
  end;

  { TImporterClassStore }

  TImporterClassStore = class
  private
    FList: TFPList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TDataModelImporterClass;
  public
    procedure Register(ImporterClass: TDataModelImporterClass);
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TDataModelImporterClass read GetItems; default;
  end;

  procedure DatasetToDataModel(Dataset: TDataset; Model: TDataModel; DoClear: Boolean = True);

  function ImporterClassStore: TImporterClassStore;

implementation

var
  _ImporterStore: TImporterClassStore;

procedure DatasetToDataModel(Dataset: TDataset; Model: TDataModel;
  DoClear: Boolean);
var
  i: Integer;
  DBField: TField;
  ModelField: TDataModelField;
begin
  if DoClear then
    Model.Fields.Clear;
  for i := 0 to Dataset.Fields.Count - 1 do
  begin
    DBField := Dataset.Fields[i];
    ModelField := Model.Fields.Add;
    ModelField.FieldName := DBField.FieldName;
    ModelField.FieldType := FieldTypeMap[DBField.DataType];
    ModelField.DisplayLabel := DBField.DisplayLabel;
  end;
end;

function ImporterClassStore: TImporterClassStore;
begin
  Result := _ImporterStore;
end;

{ TImporterClassStore }

function TImporterClassStore.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TImporterClassStore.GetItems(Index: Integer): TDataModelImporterClass;
begin
  Result := TDataModelImporterClass(FList[Index]);
end;

procedure TImporterClassStore.Register(ImporterClass: TDataModelImporterClass);
begin
  Assert(ImporterClass <> nil, 'TImporterClassStore.Register: Importer class must be <> nil');
  FList.Add(ImporterClass);
end;

constructor TImporterClassStore.Create;
begin
  FList := TFPList.Create;
end;

destructor TImporterClassStore.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;


initialization
  _ImporterStore := TImporterClassStore.Create;

finalization
  _ImporterStore.Destroy;

end.

