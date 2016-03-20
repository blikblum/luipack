unit LuiSqlite3Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, LuiDataClasses, db, fpjson, SQLClientBase;

type


   { TSqlite3DatasetAdapter }

   TSqlite3DatasetAdapter = class(TDatasetAdapter)
   public
     function ApplyUpdates(Dataset: TDataSet): Boolean; override;
     function CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet; override;
     function GetLastInsertId(Dataset: TDataSet): Int64; override;
     procedure SetSQL(Dataset: TDataSet; const SQL: String); override;
   end;

   { TSqlite3ResourceClient }

   TSqlite3ResourceClient = class(TSQLResourceClient)
   private
     FDatabase: String;
   protected
     function CreateAdapter: TDatasetAdapter; override;
   public
   published
     property Database: String read FDatabase write FDatabase;
   end;


implementation

uses
  LuiJSONUtils, variants;

{ TSqlite3DatasetAdapter }

function TSqlite3DatasetAdapter.ApplyUpdates(Dataset: TDataSet): Boolean;
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.ApplyUpdates;
end;

function TSqlite3DatasetAdapter.CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet;
var
  DS: TSqlite3Dataset absolute Result;
begin
  DS := TSqlite3Dataset.Create(nil);
  DS.PrimaryKey := ModelDef.PrimaryKey;
  DS.TableName := ModelDef.TableName;
  DS.FileName := TSqlite3ResourceClient(Client).Database;
  DS.ExecSQL('PRAGMA foreign_keys = ON');
end;

function TSqlite3DatasetAdapter.GetLastInsertId(Dataset: TDataSet): Int64;
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  Result := DS.LastInsertRowId;
end;

procedure TSqlite3DatasetAdapter.SetSQL(Dataset: TDataSet; const SQL: String);
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.SQL := SQL;
end;

{ TSqlite3ResourceClient }

function TSqlite3ResourceClient.CreateAdapter: TDatasetAdapter;
begin
  Result := TSqlite3DatasetAdapter.Create;
end;


end.

