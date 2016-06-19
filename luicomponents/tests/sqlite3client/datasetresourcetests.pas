unit DatasetResourceTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, LuiDataClasses, JSONTestHelpers, LuiFPTestHelpers;

type

  { TDatasetResourceTests }

  TDatasetResourceTests = class(TTestCase)
  private
    class var FClient: IResourceClient;
  protected
    FResource: IDatasetResource;
  public
    class procedure SetClient(Client: IResourceClient);
    function GetLastContactId: Integer;
    function GetResourceCount(const ModelName: String): Integer;
  published
    procedure Fetch;
    procedure AddRecord;
    procedure FetchWithParam;
    procedure AddRecordWithParam;
    procedure Delete;
  end;

implementation

uses
  fpjson;

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

class procedure TDatasetResourceTests.SetClient(Client: IResourceClient);
begin
  FClient := Client;
end;

function TDatasetResourceTests.GetLastContactId: Integer;
begin
  FResource := FClient.GetDataset('contact');
  FResource.Fetch;
  CheckNotEquals(0, FResource.Dataset.RecordCount);
  Result := FResource.Dataset.FieldByName('id').AsInteger;
  CheckNotEquals(-1, Result);
end;

function TDatasetResourceTests.GetResourceCount(const ModelName: String): Integer;
begin
  FResource := FClient.GetDataset(ModelName);
  FResource.Fetch;
  Result := FResource.Dataset.RecordCount;
end;

procedure TDatasetResourceTests.Fetch;
begin
  FResource := FClient.GetDataset('contact');
  CheckTrue(FResource.Fetch);

  FResource := FClient.GetDataset('category');
  CheckTrue(FResource.Fetch);
end;

procedure TDatasetResourceTests.AddRecord;
var
  CategoryId, ContactId, CategoryCount, ContactCount: Integer;
begin
  CategoryCount := GetResourceCount('category');
  FResource := FClient.GetDataset('category');
  FResource.Fetch;
  FResource.Dataset.Append;
  FResource.Dataset.FieldByName('name').AsString := 'Cat2';
  FResource.Dataset.Post;
  CheckTrue(FResource.Save, 'Save should succeed');

  //refetch since id is not automatically set
  FResource.Fetch;
  FResource.Dataset.Last;
  CategoryId := FResource.Dataset.FieldByName('id').AsInteger;
  CheckNotEquals(0, CategoryId, 'Id should be set');

  FResource := FClient.GetDataset('category');
  FResource.Fetch;
  CheckEquals(CategoryCount + 1, FResource.Dataset.RecordCount, 'Should have one more category');

  ContactCount := GetResourceCount('contact');

  FResource := FClient.GetDataset('contact');
  FResource.Fetch;
  FResource.Dataset.Append;
  FResource.Dataset.FieldByName('name').AsString := 'Luiz Américo P Câmara';
  FResource.Dataset.FieldByName('categoryid').AsInteger := CategoryId;
  FResource.Dataset.Post;
  CheckTrue(FResource.Save, 'Save should succeed');

  FResource.Fetch;
  FResource.Dataset.Last;
  ContactId := FResource.Dataset.FieldByName('id').AsInteger;
  CheckNotEquals(0, ContactId, 'Id should be valid');

  FResource := FClient.GetDataset('contact');
  CheckTrue(FResource.Fetch);

  CheckEquals(ContactCount + 1, FResource.Dataset.RecordCount, 'Should have one more contact');

  FResource := FClient.GetDataset('contact');
  CheckTrue(FResource.Fetch(ContactId));
  CheckEquals('Luiz Américo P Câmara', FResource.Dataset.FieldByName('name').AsString);
end;

procedure TDatasetResourceTests.FetchWithParam;
var
  Id: Integer;
begin
  Id := GetLastContactId;
  FResource := FClient.GetDataset('contactphone');
  FResource.ParamByName('contactid').AsInteger := Id;
  CheckTrue(FResource.Fetch);
end;

procedure TDatasetResourceTests.AddRecordWithParam;
var
  ContactId, PhoneId, PhoneCount: Integer;
begin
  ContactId := GetLastContactId;

  FResource := FClient.GetDataset('contactphone');
  FResource.ParamByName('contactid').AsInteger := ContactId;
  FResource.Fetch;
  PhoneCount := FResource.Dataset.RecordCount;

  FResource := FClient.GetDataset('contactphone');
  FResource.ParamByName('contactid').AsInteger := ContactId;
  FResource.Fetch;
  FResource.Dataset.Append;
  FResource.Dataset.FieldByName('number').AsString := '666';
  FResource.Dataset.Post;
  CheckTrue(FResource.Save);

  //refetch since id is not automatically set
  FResource.Fetch;
  FResource.Dataset.Last;
  PhoneId := FResource.Dataset.FieldByName('id').AsInteger;
  CheckNotEquals(0, PhoneId);

  FResource := FClient.GetDataset('contactphone');
  FResource.ParamByName('contactid').AsInteger := ContactId;
  CheckTrue(FResource.Fetch(PhoneId));

  FResource := FClient.GetDataset('contactphone');
  FResource.ParamByName('contactid').AsInteger := ContactId;
  CheckTrue(FResource.Fetch);
  CheckEquals(PhoneCount + 1, FResource.Dataset.RecordCount);
end;

procedure TDatasetResourceTests.Delete;
var
  ContactId, ContactCount: Integer;
begin
  ContactCount := GetResourceCount('contact');
  ContactId := GetLastContactId;
  //delete with existing resource
  FResource := FClient.GetDataset('contact');
  CheckTrue(FResource.Fetch(ContactId));
  CheckNotEquals(0, FResource.Dataset.RecordCount);
  FResource.Dataset.Delete;
  CheckTrue(FResource.Save);

  FResource := FClient.GetDataset('contact');
  CheckTrue(FResource.Fetch(ContactId));
  CheckEquals(0, FResource.Dataset.RecordCount);

  FResource := FClient.GetDataset('contact');
  FResource.Fetch;
  CheckEquals(ContactCount - 1, FResource.Dataset.RecordCount, 'Should have one less record');
end;

initialization
  RegisterTest(TDatasetResourceTests.Suite);
end.

