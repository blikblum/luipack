unit RTTIUtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson;

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
    procedure SetClassPropertyWithDerivedClass_PropertySet;
    procedure SetClassPropertyWithNonDerivedClass_Exception;
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

procedure TSetPropertiesTestCase.SetClassPropertyWithDerivedClass_PropertySet;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONArray.Create;

  SetObjectProperties(FObj, ['JSONArray', JSONArray]);
  CheckSame(JSONArray, FObj.JSONArray);

  SetObjectProperties(FObj, ['JSONData', JSONArray]);
  CheckSame(JSONArray, FObj.JSONData);

  JSONArray.Destroy;
end;

procedure TSetPropertiesTestCase.SetClassPropertyWithNonDerivedClass_Exception;
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
  RegisterTest('RTTIUtils', TSetPropertiesTestCase);

end.

