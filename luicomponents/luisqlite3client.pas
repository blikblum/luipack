unit LuiSqlite3Client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3DS, db, SQLClientBase;

type


   { TSqlite3DatasetAdapter }

   TSqlite3DatasetAdapter = class(TDatasetAdapter)
   public
     class function ApplyUpdates(Dataset: TDataSet): Boolean; override;
     class function BindParams(const SQL: String; Params: TParams): String; override;
     class function CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet; override;
     class function CreateParams(Dataset: TDataSet): TParams; override;
     class procedure DestroyParams(Params: TParams); override;
     class function InsertRecord(Dataset: TDataSet; ModelDef: TSQLModelDef): Int64; override;
     class procedure SetSQL(Dataset: TDataSet; const SQL: String); override;
   end;

   { TSqlite3ResourceClient }

   TSqlite3ResourceClient = class(TSQLResourceClient)
   private
     FDatabase: String;
   protected
     function GetAdapter: TDatasetAdapterClass; override;
   public
   published
     property Database: String read FDatabase write FDatabase;
   end;


implementation

uses
  variants;

{ TSqlite3DatasetAdapter }

class function TSqlite3DatasetAdapter.ApplyUpdates(Dataset: TDataSet): Boolean;
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.ApplyUpdates;
end;

class function TSqlite3DatasetAdapter.BindParams(const SQL: String; Params: TParams): String;
var
  Param: TParam;
  ParamStr: String;
  i: Integer;
begin
  Result := SQL;
  for i := 0 to Params.Count - 1 do
  begin
    //todo: handle InputFields
    Param := Params.Items[i];
    if Param.IsNull then
      ParamStr := 'NULL'
    else
    begin
      ParamStr := Param.AsString;
      if Param.DataType = ftString then
        ParamStr := QuotedStr(ParamStr);
    end;
    Result := StringReplace(Result, ':' + Param.Name, ParamStr,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

class function TSqlite3DatasetAdapter.CreateDataset(Client: TSQLResourceClient;
  ModelDef: TSQLModelDef): TDataSet;
var
  DS: TSqlite3Dataset absolute Result;
begin
  DS := TSqlite3Dataset.Create(nil);
  DS.PrimaryKey := ModelDef.PrimaryKey;
  DS.TableName := ModelDef.TableName;
  DS.FileName := TSqlite3ResourceClient(Client).Database;
  DS.ExecSQL('PRAGMA foreign_keys = ON');
end;

class function TSqlite3DatasetAdapter.CreateParams(Dataset: TDataSet): TParams;
begin
  Result := TParams.Create(TParam);
end;

class procedure TSqlite3DatasetAdapter.DestroyParams(Params: TParams);
begin
  Params.Destroy;
end;

class function TSqlite3DatasetAdapter.InsertRecord(Dataset: TDataSet; ModelDef: TSQLModelDef): Int64;
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.ApplyUpdates;
  Result := DS.LastInsertRowId;
end;

class procedure TSqlite3DatasetAdapter.SetSQL(Dataset: TDataSet; const SQL: String);
var
  DS: TSqlite3Dataset absolute Dataset;
begin
  DS.SQL := SQL;
end;

{ TSqlite3ResourceClient }

function TSqlite3ResourceClient.GetAdapter: TDatasetAdapterClass;
begin
  Result := TSqlite3DatasetAdapter;
end;


end.

