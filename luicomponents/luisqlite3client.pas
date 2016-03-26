unit LuiSqlite3Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, db, SQLClientBase;

type


   { TSqlite3DatasetAdapter }

   TSqlite3DatasetAdapter = class(TDatasetAdapter)
   public
     function ApplyUpdates(Dataset: TDataSet): Boolean; override;
     function BindParams(const SQL: String; Params: TParams): String; override;
     function CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet; override;
     function CreateParams(Dataset: TDataSet): TParams; override;
     function InsertRecord(Dataset: TDataSet; Client: TSQLResourceClient; ModelDef: TSQLModelDef): Int64; override;
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

function TSqlite3DatasetAdapter.BindParams(const SQL: String; Params: TParams): String;
var
  Param: TParam;
  i: Integer;
begin
  Result := SQL;
  for i := 0 to Params.Count - 1 do
  begin
    //todo: handle InputFields
    Param := Params.Items[i];
    Result := StringReplace(Result, ':' + Param.Name, Param.AsString,
      [rfReplaceAll, rfIgnoreCase]);
  end;
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

function TSqlite3DatasetAdapter.CreateParams(Dataset: TDataSet): TParams;
begin
  Result := TParams.Create(TParam);
end;

function TSqlite3DatasetAdapter.InsertRecord(Dataset: TDataSet; Client: TSQLResourceClient;
  ModelDef: TSQLModelDef): Int64;
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.ApplyUpdates;
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

