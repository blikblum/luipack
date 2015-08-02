unit DataView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataModel;

type

  { TDataViewField }

  TDataViewField = class(TCollectionItem)
  private
    //FDisplayLabel: String;
    FFieldID: String;
    FReferenceField: TDataModelField;
    function GetFieldName: String;
    function GetFieldType: TDataModelFieldType;
    function GetModel: TDataModel;
    procedure LoadReferenceField;
    procedure ReferenceFieldNeeded;
    function GetDisplayLabel: String;
  public
    procedure Assign(Source: TPersistent); override;
    property DisplayLabel: String read GetDisplayLabel;
    property FieldName: String read GetFieldName;
    property FieldType: TDataModelFieldType read GetFieldType;
    property Model: TDataModel read GetModel;
  published
    property FieldID: String read FFieldID write FFieldID;
  end;

  { TDataViewFields }

  TDataViewFields = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TDataViewField;
  public
    property Items[Index: Integer]: TDataViewField read GetItems; default;
  end;

  { TDataView }

  TDataView = class(TCollectionItem)
  private
    FFields: TDataViewFields;
    FName: String;
    FFieldResolver: IFieldResolver;
    procedure SetFields(Value: TDataViewFields);
  public
    constructor Create(FieldResolver: IFieldResolver);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fields: TDataViewFields read FFields write SetFields;
    property Name: String read FName write FName;
  end;

  { TDataViews }

  TDataViews = class(TOwnedCollection)
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  end;


implementation

{ TDataViews }

procedure TDataViews.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if Action = cnAdded then
    (Item as TDataView).FFieldResolver := Owner as IFieldResolver;
end;

{ TDataViewFields }

function TDataViewFields.GetItems(Index: Integer): TDataViewField;
begin
  Result := TDataViewField(inherited Items[Index]);
end;

{ TDataViewField }

procedure TDataViewField.LoadReferenceField;
var
  OwnerView: TDataView;
begin
  OwnerView := ((Collection as TDataViewFields).Owner as TDataView);
  FReferenceField := OwnerView.FFieldResolver.Find(FFieldID);
  if FReferenceField = nil then
    raise Exception.CreateFmt('FieldID "%d" not Found', [FFieldID]);
end;

function TDataViewField.GetFieldName: String;
begin
  ReferenceFieldNeeded;
  Result := FReferenceField.FieldName;
end;

function TDataViewField.GetFieldType: TDataModelFieldType;
begin
  ReferenceFieldNeeded;
  Result := FReferenceField.FieldType;
end;

function TDataViewField.GetModel: TDataModel;
begin
  ReferenceFieldNeeded;
  Result := FReferenceField.Model;
end;

procedure TDataViewField.ReferenceFieldNeeded;
begin
  if FReferenceField = nil then
    LoadReferenceField;
end;

procedure TDataViewField.Assign(Source: TPersistent);
begin
  if Source is TDataViewField then
  begin
    FFieldID := TDataViewField(Source).FieldID;
    FReferenceField := nil;
    //FDisplayLabel := TDataViewField(Source).DisplayLabel;
  end
  else if Source is TDataModelField then
  begin
    FFieldID := TDataModelField(Source).FieldID;
    FReferenceField := nil;
  end
  else
    inherited Assign(Source);
end;

function TDataViewField.GetDisplayLabel: String;
begin
  ReferenceFieldNeeded;
  Result := FReferenceField.DisplayLabel;
end;


{ TDataView }

procedure TDataView.SetFields(Value: TDataViewFields);
begin
  FFields.Assign(Value);
end;

constructor TDataView.Create(FieldResolver: IFieldResolver);
begin
  FFieldResolver := FieldResolver;
  FFields := TDataViewFields.Create(Self, TDataViewField);
end;

destructor TDataView.Destroy;
begin
  FFields.Destroy;
  inherited Destroy;
end;

procedure TDataView.Assign(Source: TPersistent);
begin
  if Source is TDataView then
  begin
    Fields := TDataView(Source).Fields;
    Name := TDataView(Source).Name;
  end
  else
    inherited Assign(Source);
end;


end.

