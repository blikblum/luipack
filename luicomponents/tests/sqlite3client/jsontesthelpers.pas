unit JSONTestHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONUtils;

type

  { TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
    procedure CopyFrom(const Elements : Array of Const);
    procedure CopyFrom(SrcData: TJSONObject);
  end;

implementation

{ TJSONObjectHelper }

procedure TJSONObjectHelper.CopyFrom(const Elements: array of const);
var
  SrcData: TJSONObject;
begin
  SrcData := TJSONObject.Create(Elements);
  CopyJSONObject(SrcData, Self);
  SrcData.Destroy;
end;

procedure TJSONObjectHelper.CopyFrom(SrcData: TJSONObject);
begin
  CopyJSONObject(SrcData, Self);
end;

end.

