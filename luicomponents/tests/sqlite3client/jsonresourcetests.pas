unit JSONResourceTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, LuiDataClasses, JSONTestHelpers, LuiFPTestHelpers;

type

  { TJSONResourceTests }

  TJSONResourceTests = class(TTestCase)
  private
    class var FClient: IResourceClient;
  protected
    FObject: IJSONObjectResource;
    FArray: IJSONArrayResource;
  public
    class procedure SetClient(Client: IResourceClient);
    function GetLastContactId: Integer;
    function GetResourceCount(const ModelName: String): Integer;
  published
    procedure Fetch;
    procedure AddRecord;
    procedure FetchWithParam;
    procedure AddRecordWithParam;
    procedure ArbitraryData;
    procedure FieldsWithComplexData;
    procedure Patch;
    procedure Delete;
  end;

implementation

uses
  fpjson, LuiJSONUtils;

function CreateRandomObject: TJSONObject;
begin
  Result := TJSONObject.Create([
      IntToStr(Random(100)), 'x',
      IntToStr(Random(1000)), 1,
      IntToStr(Random(10000)), nil
    ]);
end;

function CreateRandomArray: TJSONArray;
begin
  Result := TJSONArray.Create([Random(100), IntToStr(Random(1000)), Random(10000)]);
end;

class procedure TJSONResourceTests.SetClient(Client: IResourceClient);
begin
  FClient := Client;
end;

function TJSONResourceTests.GetLastContactId: Integer;
begin
  FArray := FClient.GetJSONArray('contact');
  FArray.Fetch;
  CheckNotEquals(0, FArray.Data.Count);
  Result := FArray.Data.Objects[FArray.Data.Count - 1].Get('id', -1);
  CheckNotEquals(-1, Result);
end;

function TJSONResourceTests.GetResourceCount(const ModelName: String): Integer;
begin
  FArray := FClient.GetJSONArray(ModelName);
  FArray.Fetch;
  Result := FArray.Data.Count;
end;

procedure TJSONResourceTests.Fetch;
begin
  FArray := FClient.GetJSONArray('contact');
  CheckTrue(FArray.Fetch);

  FArray := FClient.GetJSONArray('category');
  CheckTrue(FArray.Fetch);
end;

procedure TJSONResourceTests.AddRecord;
var
  CategoryId, ContactId, CategoryCount, ContactCount: Integer;
begin
  CategoryCount := GetResourceCount('category');
  FObject := FClient.GetJSONObject('category');
  FObject.Data.CopyFrom(['name', 'Cat1']);
  CheckTrue(FObject.Save, 'Save should succeed');

  CategoryId := FObject.Data.Get('id', -1);
  CheckNotEquals(-1, CategoryId, 'Id should be set');

  FArray := FClient.GetJSONArray('category');
  FArray.Fetch;
  CheckEquals(CategoryCount + 1, FArray.Data.Count, 'Should have one more record');

  ContactCount := GetResourceCount('contact');

  FObject := FClient.GetJSONObject('contact');
  FObject.Data.CopyFrom(['name', 'Luiz Américo', 'categoryid', CategoryId]);
  CheckTrue(FObject.Save, 'Save should succeed');

  ContactId := FObject.Data.Get('id', -1);
  CheckNotEquals(-1, ContactId, 'Id should be valid');

  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Fetch(ContactId));

  FArray := FClient.GetJSONArray('contact');
  FArray.Fetch;
  CheckEquals(ContactCount + 1, FArray.Data.Count, 'Should have one more record');

  FObject := FClient.GetJSONObject('contact');
  FObject.Data.CopyFrom(['name', 'Luiz999']);
  CheckTrue(FObject.Save(999), 'Save with explicit id should succeed');

  ContactId := FObject.Data.Get('id', -1);
  CheckEquals(999, ContactId, 'Id should be equal saved');

  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Fetch(ContactId));
  CheckEquals('Luiz999', FObject.Data.Get('name', ''));
end;

procedure TJSONResourceTests.FetchWithParam;
var
  Id: Integer;
begin
  Id := GetLastContactId;
  FArray := FClient.GetJSONArray('contactphone');
  FArray.ParamByName('contactid').AsInteger := Id;
  CheckTrue(FArray.Fetch);
end;

procedure TJSONResourceTests.AddRecordWithParam;
var
  ContactId, PhoneId, PhoneCount: Integer;
begin
  ContactId := GetLastContactId;

  FArray := FClient.GetJSONArray('contactphone');
  FArray.ParamByName('contactid').AsInteger := ContactId;
  FArray.Fetch;
  PhoneCount := FArray.Data.Count;

  FObject := FClient.GetJSONObject('contactphone');
  FObject.ParamByName('contactid').AsInteger := ContactId;
  FObject.Data.CopyFrom(['number', '555']);
  CheckTrue(FObject.Save);

  PhoneId := FObject.Data.Get('id', -1);
  CheckNotEquals(-1, PhoneId);

  FObject := FClient.GetJSONObject('contactphone');
  FObject.ParamByName('contactid').AsInteger := ContactId;
  CheckTrue(FObject.Fetch(PhoneId));

  FArray := FClient.GetJSONArray('contactphone');
  FArray.ParamByName('contactid').AsInteger := ContactId;
  CheckTrue(FArray.Fetch);
  CheckEquals(PhoneCount + 1, FArray.Data.Count);
end;

procedure TJSONResourceTests.ArbitraryData;
var
  Key: String;
  GUID: TGuid;
  Obj: TJSONObject;
begin
  CreateGUID(GUID);
  Key := GUIDToString(GUID);
  Obj := CreateRandomObject;
  FObject := FClient.GetJSONObject('systemdata');
  FObject.Data.CopyFrom(Obj);
  CheckTrue(FObject.Save(Key));

  FObject := FClient.GetJSONObject('systemdata');
  FObject.Fetch(Key);
  CheckTrue(SameJSONObject(Obj, FObject.Data), 'Saved data should be equal');

  //reuse the key
  FObject.Data.Clear;
  Obj.Clear;
  Obj.Strings['x'] := 'y';
  FObject.Data.CopyFrom(Obj);
  //do not pass key in save
  CheckTrue(FObject.Save);

  FObject := FClient.GetJSONObject('systemdata');
  FObject.Fetch(Key);
  CheckTrue(SameJSONObject(Obj, FObject.Data), 'Saved data should be equal');
end;

procedure TJSONResourceTests.FieldsWithComplexData;
var
  ContactId: Integer;
  Arr: TJSONArray;
  Obj: TJSONObject;
begin
  FObject := FClient.GetJSONObject('contact');
  FObject.Data.Strings['name'] := 'new';
  FObject.Save;
  ContactId := FObject.Data.Get('id', 0);

  FObject := FClient.GetJSONObject('contactdetail');
  CheckTrue(FObject.Fetch(ContactId));
  CheckEquals(0, FObject.Data.Count);
  FObject.Save;

  FObject := FClient.GetJSONObject('contactdetail');
  CheckTrue(FObject.Fetch(ContactId));
  CheckTrue(FObject.Data.Count > 0);
  CheckTrue(FObject.Data.Elements['arraydata'].JSONType = jtArray);
  CheckTrue(FObject.Data.Elements['objectdata'].JSONType = jtObject);
  CheckTrue(FObject.Data.Elements['arraydata'].Count = 0);
  CheckTrue(FObject.Data.Elements['objectdata'].Count = 0);

  Obj := CreateRandomObject;
  Arr := CreateRandomArray;
  FObject.Data.Elements['arraydata'] := Arr.Clone;
  FObject.Data.Elements['objectdata'] := Obj.Clone;
  FObject.Save;

  FObject := FClient.GetJSONObject('contactdetail');
  CheckTrue(FObject.Fetch(ContactId));
  CheckEqualsJSON(Arr, FObject.Data.Elements['arraydata']);
  CheckEqualsJSON(Obj, FObject.Data.Elements['objectdata']);
  Arr.Destroy;
  Obj.Destroy;
end;

procedure TJSONResourceTests.Patch;
var
  ContactId, CategoryId: Integer;
  PristineData: TJSONObject;
begin
  ContactId := GetLastContactId;
  FObject := FClient.GetJSONObject('contact');
  FObject.Fetch(ContactId);
  PristineData := TJSONObject.Create;
  PristineData.CopyFrom(FObject.Data);

  FObject := FClient.GetJSONObject('category');
  FObject.Data.Strings['name'] := 'New Category';
  FObject.Save;
  CategoryId := FObject.Data.Get('id', 0);

  FObject := FClient.GetJSONObject('contact');
  FObject.Data.Integers['categoryid'] := CategoryId;
  CheckTrue(FObject.Save(ContactId, [soPatch]));

  FObject := FClient.GetJSONObject('contact');
  FObject.Fetch(ContactId);
  PristineData.Integers['categoryid'] := CategoryId;
  CheckEqualsJSON(PristineData, FObject.Data, 'Should modify only one field');
  PristineData.Destroy;
end;

procedure TJSONResourceTests.Delete;
var
  ContactId, ContactCount: Integer;
begin
  ContactCount := GetResourceCount('contact');
  ContactId := GetLastContactId;
  //delete with existing resource
  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Fetch(ContactId));
  CheckNotEquals(0, FObject.Data.Count);
  CheckTrue(FObject.Delete);

  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Fetch(ContactId));
  CheckEquals(0, FObject.Data.Count);

  //delete with id
  ContactId := GetLastContactId;
  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Delete(ContactId));

  FObject := FClient.GetJSONObject('contact');
  CheckTrue(FObject.Fetch(ContactId));
  CheckEquals(0, FObject.Data.Count);

  FArray := FClient.GetJSONArray('contact');
  FArray.Fetch;
  CheckEquals(ContactCount - 2, FArray.Data.Count, 'Should have one more record');
end;

initialization
  RegisterTest(TJSONResourceTests.Suite);
end.

