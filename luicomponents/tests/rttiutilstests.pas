unit RTTIUtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, fpjson;

type

  { TRTTIClass }

  TRTTIClass = class
  private
    FJSONArray: TJSONArray;
    FJSONData: TJSONData;
    FStr: String;
  published
    property Str: String read FStr write FStr;
    property JSONData: TJSONData read FJSONData write FJSONData;
    property JSONArray: TJSONArray read FJSONArray write FJSONArray;
  end;

  { TSetPropertiesTestCase }

  TSetPropertiesTestCase = class(TTestCase)
  private
    FObj: TRTTIClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SetDescendantClass;
    procedure SetNonDescendantClass;
  end;

implementation

uses
  LuiRTTIUtils;

procedure TSetPropertiesTestCase.SetUp;
begin
  FObj := TRTTIClass.Create;
end;

procedure TSetPropertiesTestCase.TearDown;
begin
  FreeAndNil(FObj);
end;

procedure TSetPropertiesTestCase.SetDescendantClass;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONArray.Create;

  SetObjectProperties(FObj, ['JSONArray', JSONArray]);
  CheckSame(JSONArray, FObj.JSONArray, 'Data must match property');

  SetObjectProperties(FObj, ['JSONData', JSONArray]);
  CheckSame(JSONArray, FObj.JSONData, 'Data must match property');

  JSONArray.Destroy;
end;

procedure TSetPropertiesTestCase.SetNonDescendantClass;
var
  JSONObject: TJSONObject;
  Raised: Boolean;
begin
  Raised := False;
  JSONObject := TJSONObject.Create;
  try
    SetObjectProperties(FObj, ['JSONArray', JSONObject]);
  except
    Raised := True;
  end;
  CheckTrue(Raised);
  JSONObject.Destroy;
end;



initialization
  ProjectRegisterTests('RTTIUtils', [TSetPropertiesTestCase.Suite]);

end.

