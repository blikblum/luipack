unit LuiJSONUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  { TJSONFile }

  TJSONFile = class
  private
    FData: TJSONData;
    FFileName: String;
  public
    destructor Destroy; override;
    procedure Load;
    property Data: TJSONData read FData ;
    property FileName: String read FFileName write FFileName;
  end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String): TJSONData;

function StringToJSONData(const JSONStr: TJSONStringType): TJSONData;

implementation

uses
  jsonparser;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Boolean): Boolean;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsBoolean
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String; Default: Integer): Integer;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsInteger
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName, Default: String): String;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i].AsString
  else
    Result := Default;
end;

function GetJSONProp(JSONObj: TJSONObject; const PropName: String): TJSONData;
var
  i: Integer;
begin
  i := JSONObj.IndexOfName(PropName);
  if i <> -1 then
    Result := JSONObj.Items[i]
  else
    Result := nil;
end;

function StringToJSONData(const JSONStr: TJSONStringType): TJSONData;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSONStr);
  try
    Result := Parser.Parse;
  finally
    Parser.Destroy;
  end;
end;

{ TJSONFile }

destructor TJSONFile.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TJSONFile.Load;
var
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  //todo: handle UTF-8
  if not FileExists(FFileName) then
    raise Exception.CreateFmt('TJSONFile - File "%s" does not exist', [FFileName]);
  FreeAndNil(FData);
  Stream := nil;
  Parser := nil;
  try
    try
      Stream := TFileStream.Create(FFileName, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      FData := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      raise Exception.CreateFmt('TJSONFile - Error loading "%s" : %s', [FFileName, E.Message]);
    on E: EJSONScanner do
    begin
       FData.Free;
       raise Exception.CreateFmt('TJSONFile - Error parsing "%s" : %s', [FFileName, E.Message]);
    end;
  end;
end;

end.

