program SQLDbClientLCLTestRunner;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, sysutils,  GUITestRunner, sqldb, sqlite3conn, LuiJSONUtils, fpjsonrtti,
  Sqlite3DS, LuiSQLDbClient, JSONResourceTests, DatasetResourceTests, fpjson;

{$R *.res}

var
  Resources: TSQLDbResourceClient;

function CreateRandomFileName: String;
begin
  Result := 'Test_' + FormatDateTime('hhnnssz',Now())+'_'+inttostr(random(99))+'.db';
end;

procedure InitializeSQLDbClient;
var
  Dataset: TSqlite3Dataset;
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  Resources := TSQLDbResourceClient.Create(Application);
  Resources.ModelDefs.LoadFromFile('resourcedefs.json');
  Connection := TSQLite3Connection.Create(Application);
  Connection.DatabaseName := CreateRandomFileName;
  Connection.Params.Add('foreign_keys=1');

  Transaction := TSQLTransaction.Create(Application);
  Transaction.DataBase := Connection;
  Resources.Connection := Connection;
  Dataset := TSqlite3Dataset.Create(nil);
  try
    Dataset.FileName := Connection.DatabaseName;
    Dataset.SQLList.LoadFromFile('sqlite3ddl.sql');
    Dataset.ExecSQL('BEGIN;');
    Dataset.ExecSQLList;
    Dataset.ExecSQL('COMMIT;');
  finally
    Dataset.Destroy;
  end;
end;

begin
  InitializeSQLDbClient;
  TJSONResourceTests.SetClient(Resources);
  TDatasetResourceTests.SetClient(Resources);
  Application.Initialize;
  RunRegisteredTests;
end.

