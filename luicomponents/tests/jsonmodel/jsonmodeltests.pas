unit JSONModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, LuiJSONModel, fpjson, CallLogClient;

type

  { TJSONModelTests }

  TJSONModelTests = class(TTestCase)
  private
    FModel: TJSONModel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; overload; override;
  published
    procedure CreateInstance;
    procedure Save;
  end;

  { TJSONCollectionTests }

  TJSONCollectionTests = class(TTestCase)
  private
    FCollection: TJSONCollection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddItem;
    procedure DeleteSavedItem;
    procedure DeleteNewItem;
  end;

implementation

uses
  LuiJSONHelpers, LuiFPTestHelpers;

type

  { TModelWithCreateData }

  TModelWithCreateData = class(TJSONModel)
  protected
    function CreateData: TJSONObject; override;
  end;

{ TJSONCollectionTests }

procedure TJSONCollectionTests.SetUp;
begin
  Calls.Reset;
end;

procedure TJSONCollectionTests.TearDown;
begin
  inherited TearDown;
  FCollection.Free;
end;

procedure TJSONCollectionTests.AddItem;
begin
  FCollection := TJSONCollection.Create;
  FCollection.CreateItem();
  CheckEquals(1, FCollection.Data.Count);
  CheckNotNull(FCollection.Items[0]);
end;

procedure TJSONCollectionTests.DeleteSavedItem;
begin
  FCollection := TJSONCollection.Create;
  FCollection.CreateItem();
  FCollection.Items[0].Save;
  FCollection.Items[0].Delete;
  CheckEquals(0, FCollection.Count);
  CheckTrue(Calls.CalledOnce('delete'));
end;

procedure TJSONCollectionTests.DeleteNewItem;
begin
  FCollection := TJSONCollection.Create;
  FCollection.CreateItem();
  FCollection.Items[0].Delete;
  CheckEquals(0, FCollection.Count);
  CheckFalse(Calls.Called('delete'));
end;

{ TModelWithCreateData }

function TModelWithCreateData.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create(['a', 1, 'x', 'b']);
end;

procedure TJSONModelTests.SetUp;
begin
  Calls.Reset;
end;

procedure TJSONModelTests.TearDown;
begin
  FModel.Free;
end;

constructor TJSONModelTests.Create;
begin
  inherited Create;
  TJSONModel.DefaultResourceClient := TCallLogClient.Create(nil);
end;

procedure TJSONModelTests.CreateInstance;
begin
  FModel := TJSONModel.Create;
  CheckNotNull(FModel.Data, 'Data property should be not null');
  CheckNull(FModel.Collection, 'Collection should be null');

  FModel.Free;
  FModel := TModelWithCreateData.Create;
  CheckEqualsJSON('{"a": 1, "x": "b"}', FModel.Data, 'should initialize data through CreateData');
end;

procedure TJSONModelTests.Save;
begin
  FModel := TJSONModel.Create;
  FModel.Data.Strings['x'] := 'a';
  FModel.Data.Integers['y'] := 2;


  FModel.Save;
  CheckTrue(Calls.CalledOnce('save'));
  CheckTrue(Calls.CalledWith('save', [nil]));
end;

initialization
  RegisterTests([TJSONModelTests.Suite, TJSONCollectionTests.Suite]);
end.

