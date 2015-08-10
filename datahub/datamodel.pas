unit DataModel;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, fpjson;

type
  TDataModel = class;

  TDataModelFieldType = (dftString, dftInteger, dftLargeInt, dftFloat, dftCurrency,
    dftBoolean, dftDateTime, dftDate, dftTime, dftMemo, dftBlob);

  { TDataModelField }

  TDataModelField = class(TCollectionItem)
  private
    FConstraints: String;
    FConstraintsData: TJSONObject;
    FDisplayLabel: String;
    FFieldID: String;
    FFieldName: String;
    FFieldType: TDataModelFieldType;
    function GetFieldTypeName: String;
    function GetModel: TDataModel;
    procedure SetConstraints(const Value: String);
  protected
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ConstraintsData: TJSONObject read FConstraintsData;
    property Model: TDataModel read GetModel;
  published
    property Constraints: String read FConstraints write SetConstraints;
    property DisplayLabel: String read FDisplayLabel write FDisplayLabel;
    property FieldID: String read FFieldID write FFieldID;
    property FieldName: String read FFieldName write FFieldName;
    property FieldType: TDataModelFieldType read FFieldType write FFieldType;
    property FieldTypeName: String read GetFieldTypeName;
  end;

  { TDataModelFields }

  TDataModelFields = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TDataModelField;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TDataModelField;
    property Items[Index: Integer]: TDataModelField read GetItems; default;
  end;

  TDataModel = class(TCollectionItem)
  private
    FFields: TDataModelFields;
    FName: String;
    procedure SetFields(Value: TDataModelFields);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fields: TDataModelFields read FFields write SetFields;
    property Name: String read FName write FName;
  end;

  { TDataModels }

  TDataModels = class(TCollection)
  private
    function GetItems(Index: Integer): TDataModel;
  public
    constructor Create;
    property Items[Index: Integer]: TDataModel read GetItems; default;
  end;

  { IFieldResolver }

  IFieldResolver = interface
    function Find(const ID: String): TDataModelField;
  end;



implementation

uses
  LuiJSONUtils;

{ TDataModels }

function TDataModels.GetItems(Index: Integer): TDataModel;
begin
  Result := TDataModel(inherited Items[Index]);
end;

constructor TDataModels.Create;
begin
  inherited Create(TDataModel);
end;

{ TDataModel }

procedure TDataModel.SetFields(Value: TDataModelFields);
begin
  FFields.Assign(Value);
end;

constructor TDataModel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFields := TDataModelFields.Create(Self);
end;

destructor TDataModel.Destroy;
begin
  FFields.Destroy;
  inherited Destroy;
end;

procedure TDataModel.Assign(Source: TPersistent);
begin
  if Source is TDataModel then
  begin
    Fields := TDataModel(Source).Fields;
    Name := TDataModel(Source).Name;
  end
  else
    inherited Assign(Source);
end;

function TDataModelFields.GetItems(Index: Integer): TDataModelField;
begin
  Result := TDataModelField(inherited Items[Index]);
end;

constructor TDataModelFields.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TDataModelField);
end;

function TDataModelFields.Add: TDataModelField;
var
  GUID: TGuid;
begin
  Result := TDataModelField(inherited Add);
  //register fieldID
  if CreateGUID(GUID) <> 0 then
    raise Exception.Create('Unable to create a GUID');
  Result.FFieldID := GUIDToString(GUID);
end;

const
  FieldTypeNames: array[TDataModelFieldType] of String =
   ('String', 'Integer', 'LargeInt', 'Float', 'Currency',
    'Boolean', 'DateTime', 'Date', 'Time', 'Memo', 'Blob');

{ TDataModelField }

function TDataModelField.GetFieldTypeName: String;
begin
  Result := FieldTypeNames[FFieldType];
end;

function TDataModelField.GetModel: TDataModel;
begin
  Result := nil;
  if Collection <> nil then
    Result := (Collection.Owner as TDataModel);
end;

procedure TDataModelField.SetConstraints(const Value: String);
begin
  if FConstraints = Value then Exit;
  FConstraints := Value;
  FreeAndNil(FConstraintsData);
  if not TryStrToJSON(Value, FConstraintsData) then
    raise Exception.CreateFmt('Invalid Constraints. Expected a JSON Object encoded string. Got "%s"', [Value]);
end;

function TDataModelField.GetDisplayName: string;
begin
  if FDisplayLabel <> '' then
    Result := FDisplayLabel
  else
    Result := FFieldName;
end;

destructor TDataModelField.Destroy;
begin
  FConstraintsData.Free;
  inherited Destroy;
end;

procedure TDataModelField.Assign(Source: TPersistent);
begin
  if Source is TDataModelField then
  begin
    FieldID := TDataModelField(Source).FieldID;
    DisplayLabel := TDataModelField(Source).DisplayLabel;
    FieldName := TDataModelField(Source).FieldName;
    FieldType := TDataModelField(Source).FieldType;
    Constraints := TDataModelField(Source).Constraints;
  end
  else
    inherited Assign(Source);
end;

end.

