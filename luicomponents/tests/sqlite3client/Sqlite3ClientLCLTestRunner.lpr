program Sqlite3ClientLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, sysutils, GUITestRunner, JSONResourceTests, LuiSqlite3Client, fpjsonrtti, LuiJSONUtils,
  fpjson, Sqlite3DS;

{$R *.res}

var
  Resources: TSqlite3ResourceClient;

function CreateRandomFileName: String;
begin
  Result := 'Test_' + FormatDateTime('hhnnssz',Now())+'_'+inttostr(random(99))+'.db';
end;

procedure InitializeSqliteClient;
var
  DeStreamer: TJSONDeStreamer;
  ModelDefsData: TJSONArray;
  Dataset: TSqlite3Dataset;
begin
  Resources := TSqlite3ResourceClient.Create(Application);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    if TryReadJSONFile('resourcedefs.json', ModelDefsData) then
    begin
      DeStreamer.JSONToCollection(ModelDefsData, Resources.ModelDefs);
      ModelDefsData.Free;
    end
    else
      raise Exception.Create('Unable to load file');
  finally
    DeStreamer.Destroy;
  end;
  Resources.Database := CreateRandomFileName;
  Dataset := TSqlite3Dataset.Create(nil);
  try
    Dataset.FileName := Resources.Database;
    Dataset.SQLList.LoadFromFile('ddl.sql');
    Dataset.ExecSQL('BEGIN;');
    Dataset.ExecSQLList;
    Dataset.ExecSQL('COMMIT;');
  finally
    Dataset.Destroy;
  end;
end;

begin
  InitializeSqliteClient;
  TJSONResourceTests.SetClient(Resources);
  Application.Initialize;
  RunRegisteredTests;
end.

