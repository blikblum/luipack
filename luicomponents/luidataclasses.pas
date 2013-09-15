unit LuiDataClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpjson, db, LuiJSONClasses;

type

  IDataResource = interface(IInterface)
    ['{ADACF400-8E33-4F0B-A10A-6A739DCA4CB5}']
    function Fetch: Boolean; overload;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    function Save: Boolean; overload;
    property Params: TParams read GetParams;
  end;

  { IJSONObjectDataResource }

  IJSONObjectResource = interface(IDataResource)
    ['{25B16F6D-DCEB-4D36-A4D6-260E23271B7B}']
    function Delete: Boolean;
    function Fetch(IdValue: Variant): Boolean; overload;
    function GetData: TJSONObject;
    function Save(IdValue: Variant): Boolean; overload;
    procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
    property Data: TJSONObject read GetData;
  end;

  { IJSONArrayDataResource }

  IJSONArrayResource = interface(IDataResource)
    ['{D6A53F83-E973-4657-B763-087EF9BC518D}']
    function GetData: TJSONArray;
    property Data: TJSONArray read GetData;
  end;

  { TJSONResourceLookup }

  TJSONResourceLookup = class(TComponent)
  private
    FKeyProperty: String;
    FResource: IJSONArrayResource;
    FValueProperty: String;
    function FindValueData(const PropertyName: String; const KeyValue: Variant): TJSONData;
    function GetKeys(Index: Integer): Variant;
    function GetStrings(const KeyValue: Variant): String;
    function GetValues(const KeyValue: Variant): Variant;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const PropertyName: String; const KeyValue, Default: Variant): Variant;
    function IndexOf(const KeyValue: Variant): Integer;
    procedure Update;
    property KeyProperty: String read FKeyProperty write FKeyProperty;
    property Keys[Index: Integer]: Variant read GetKeys;
    property Resource: IJSONArrayResource read FResource write FResource;
    property Strings[KeyValue: Variant]: String read GetStrings;
    property ValueProperty: String read FValueProperty write FValueProperty;
    property Values[KeyValue: Variant]: Variant read GetValues;
  end;

  function SaveChanges(Resource: IJSONObjectResource; ChangeSet: TJSONChangeSet): Boolean;

implementation

uses
  LuiJSONUtils;

procedure ApplyChange(JSONObj: TJSONObject; Data: PtrInt; ChangeType: TJSONChangeType; var Continue: Boolean);
var
  Resource: IJSONObjectResource absolute Data;
begin
  Resource.SetData(JSONObj, False);
  if ChangeType <> jcDeleted then
    Continue := Resource.Save
  else
    Continue := Resource.Delete;
end;

function SaveChanges(Resource: IJSONObjectResource; ChangeSet: TJSONChangeSet): Boolean;
begin
  Assert(Resource <> nil, 'SaveChanges - Resource must be <> nil');
  Result := ChangeSet.ForEachCall(@ApplyChange, PtrInt(Resource));
end;

{ TJSONResourceLookup }

function TJSONResourceLookup.GetValues(const KeyValue: Variant): Variant;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(FValueProperty, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.Value
  else
    Result := Null;
end;

constructor TJSONResourceLookup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyProperty := 'id';
  FValueProperty := 'name';
end;

procedure TJSONResourceLookup.AssignTo(Dest: TPersistent);
var
  i: Integer;
  ItemData, ValueData: TJSONData;
  ResourceData: TJSONArray;
begin
  if Dest is TStrings then
  begin
    ResourceData := FResource.Data;
    TStrings(Dest).Clear;
    for i := 0 to ResourceData.Count - 1 do
    begin
      ItemData := ResourceData.Items[i];
      if ItemData.JSONType = jtObject then
      begin
        ValueData := TJSONObject(ItemData).Find(FValueProperty);
        if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
          TStrings(Dest).Add(ValueData.AsString);
      end;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

function TJSONResourceLookup.GetValue(const PropertyName: String; const KeyValue, Default: Variant): Variant;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(PropertyName, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.Value
  else
    Result := Default;
end;

function TJSONResourceLookup.IndexOf(const KeyValue: Variant): Integer;
begin
  Result := GetJSONIndexOf(FResource.Data, [FKeyProperty, KeyValue]);
end;

procedure TJSONResourceLookup.Update;
begin
  FResource.Fetch;
end;

function TJSONResourceLookup.FindValueData(const PropertyName: String; const KeyValue: Variant
  ): TJSONData;
var
  ItemData: TJSONObject;
begin
  ItemData := FindJSONObject(FResource.Data, [FKeyProperty, KeyValue]);
  if ItemData <> nil then
    Result := ItemData.Find(PropertyName)
  else
    Result := nil;
end;

function TJSONResourceLookup.GetKeys(Index: Integer): Variant;
var
  ResourceData: TJSONArray;
  ItemData: TJSONData;
begin
  Result := Null;
  ResourceData := FResource.Data;
  if (Index >= 0) and (Index < ResourceData.Count) then
  begin
    ItemData := ResourceData.Items[Index];
    if ItemData.JSONType = jtObject then
      Result := TJSONObject(ItemData).Get(FKeyProperty);
  end;
end;

function TJSONResourceLookup.GetStrings(const KeyValue: Variant): String;
var
  ValueData: TJSONData;
begin
  ValueData := FindValueData(FValueProperty, KeyValue);
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtArray, jtObject]) then
    Result := ValueData.AsString
  else
    Result := '';
end;

end.

