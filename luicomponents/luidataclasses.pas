unit LuiDataClasses;
//todo: rename to LuiDataResources

{$mode objfpc}{$H+}

interface

uses
  Classes, fpjson, db, LuiJSONClasses;

type

  TSaveOption = (soPatch);

  TSaveOptions = set of TSaveOption;

  //todo: implement Resource.ExtractData??

  IDataResource = interface(IInterface)
    ['{ADACF400-8E33-4F0B-A10A-6A739DCA4CB5}']
    function Fetch: Boolean; overload;
    function GetParams: TParams;
    function ParamByName(const ParamName: String): TParam;
    function Save(Options: TSaveOptions = []): Boolean; overload;
    property Params: TParams read GetParams;
  end;

  { IJSONObjectDataResource }

  IJSONObjectResource = interface(IDataResource)
    ['{25B16F6D-DCEB-4D36-A4D6-260E23271B7B}']
    function Delete: Boolean;
    function Delete(IdValue: Variant): Boolean;
    function Fetch(IdValue: Variant): Boolean; overload;
    function GetData: TJSONObject;
    function Save(IdValue: Variant; Options: TSaveOptions = []): Boolean; overload;
    procedure SetData(JSONObj: TJSONObject; OwnsData: Boolean);
    property Data: TJSONObject read GetData;
  end;

  { IJSONArrayDataResource }

  IJSONArrayResource = interface(IDataResource)
    ['{D6A53F83-E973-4657-B763-087EF9BC518D}']
    function GetData: TJSONArray;
    property Data: TJSONArray read GetData;
  end;

  IDatasetResource = interface(IDataResource)
    ['{F5F63C3A-1CA1-4FD8-A6B8-387FD2C10129}']
    function GetDataset: TDataSet;
    function Fetch(IdValue: Variant): Boolean; overload;
    property Dataset: TDataSet read GetDataset;
  end;

  IResourceClient = interface
    ['{254EE65D-14ED-4597-860D-757532BA51CE}']
    function Connected: Boolean;
    function GetDataset(const ModelName: String): IDatasetResource;
    function GetJSONArray(const ModelName: String): IJSONArrayResource;
    function GetJSONObject(const ModelName: String): IJSONObjectResource;
    function HasModel(const ModelName: String): Boolean;
    procedure InvalidateCache(const ModelName: String);
  end;

  function SaveChanges(Resource: IJSONObjectResource; ChangeSet: TJSONChangeSet): Boolean;

implementation

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

end.

