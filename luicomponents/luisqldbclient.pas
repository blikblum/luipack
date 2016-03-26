unit LuiSQLDbClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, SQLClientBase;

type

   { TSQLDbAdapter }

   TSQLDbAdapter = class(TDatasetAdapter)
   public
     function ApplyUpdates(Dataset: TDataSet): Boolean; override;
     function CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet; override;
     function CreateParams(Dataset: TDataSet): TParams; override;
     function InsertRecord(Dataset: TDataSet; Client: TSQLResourceClient; ModelDef: TSQLModelDef): Int64; override;
     procedure SetSQL(Dataset: TDataSet; const SQL: String); override;
   end;

   { TSQLDbResourceClient }

   TSQLDbResourceClient = class(TSQLResourceClient)
   private
     FConnection: TSQLConnection;
   protected
     function CreateAdapter: TDatasetAdapter; override;
   public
   published
     property Connection: TSQLConnection read FConnection write FConnection;
   end;


implementation

uses
  LuiJSONUtils, variants, dbconst, pqconnection, IBConnection, mysql51conn,
  {$ifdef USE_SQLITE3_SLIM}
  sqlite3slimconn;
  {$else}
  sqlite3conn
  {$endif}                                ;

type
  TSQLConnectionAccess = class(TSQLConnection)

  end;

  TSQLConnectorAccess = class(TSQLConnector)

  end;

//adapted from sqldb
function CreateInsertQuery(Query: TSQLQuery; const TableName: String; FieldNamesQuoteChars : TQuoteChars): TSQLQuery;
var
  x          : integer;
  sql_fields : string;
  sql_values : string;
  Field: TField;
  Param: TParam;
begin
  sql_fields := '';
  sql_values := '';
  for x := 0 to Query.Fields.Count -1 do
  begin
    Field := Query.Fields[x];
    if (not Field.IsNull) and (pfInUpdate in Field.ProviderFlags) and (not Field.ReadOnly) then
    begin
      sql_fields := sql_fields + FieldNamesQuoteChars[0] + Field.FieldName + FieldNamesQuoteChars[1] + ',';
      sql_values := sql_values + ':"' + Field.FieldName + '",';
    end;
  end;
  if length(sql_fields) = 0 then
    DatabaseErrorFmt(sNoUpdateFields,['insert'],Query);
  Result := TSQLQuery.Create(nil);
  Result.DataBase := Query.DataBase;
  Result.Transaction := Query.Transaction;
  Result.ParseSQL := False;
  setlength(sql_fields,length(sql_fields)-1);
  setlength(sql_values,length(sql_values)-1);
  Result.SQL.Add('insert into ' + TableName + ' (' + sql_fields + ') values (' + sql_values + ')');
  for x := 0 to Result.Params.Count - 1 do
  begin
    Param := Result.Params[x];
    Field := Query.FieldByName(Param.Name);
    Param.AssignFieldValue(Field, Field.Value);
  end;
end;


{ TSQLDbAdapter }

function TSQLDbAdapter.ApplyUpdates(Dataset: TDataSet): Boolean;
var
  DS: TSQLQuery absolute Dataset;
begin
  Result := False;
  DS.ApplyUpdates;
  //todo: add param to control retaining
  TSQLTransaction(DS.Transaction).CommitRetaining;
  //if there's no exception reult will be true
  Result := True;
end;

function TSQLDbAdapter.CreateDataset(Client: TSQLResourceClient; ModelDef: TSQLModelDef): TDataSet;
var
  DS: TSQLQuery absolute Result;
begin
  DS := TSQLQuery.Create(nil);
  //DS.PrimaryKey := ModelDef.PrimaryKey;
  DS.DataBase := TSQLDbResourceClient(Client).Connection;
end;

function TSQLDbAdapter.CreateParams(Dataset: TDataSet): TParams;
var
  Query: TSQLQuery absolute Dataset;
begin
  Result := Query.Params;
end;

function TSQLDbAdapter.InsertRecord(Dataset: TDataSet; Client: TSQLResourceClient;
  ModelDef: TSQLModelDef): Int64;
var
  Query: TSQLQuery absolute Dataset;
  SqldbClient: TSQLDbResourceClient absolute Client;
  Info: TSQLStatementInfo;
  Connection: TSQLConnection;
  InsertQuery: TSQLQuery;
begin
  Result := -1;
  Connection := SqldbClient.Connection;
  if (Connection is TSQLConnector) then
    Connection := TSQLConnectorAccess(Connection).Proxy;
  {$IF FPC_FULLVERSION >= 30000}
  Info := TSQLConnectionAccess(Connection).GetStatementInfo(Query.SQL.Text);
  {$ELSE}
  Info := TSQLConnectionAccess(Connection).GetStatementInfo(Query.SQL.Text, True, stNoSchema);
  {$ENDIF}
  InsertQuery := CreateInsertQuery(Query, Info.TableName, Connection.FieldNameQuoteChars);
  try
    if (Connection is TPQConnection) or (Connection is TIBConnection) then
    begin
      InsertQuery.SQL.Add(Format('Returning %s', [ModelDef.PrimaryKey]));
      InsertQuery.Open;
      if InsertQuery.RecordCount > 0 then
        Result := InsertQuery.Fields[0].AsLargeInt;
      Connection.Transaction.CommitRetaining;
    end
    else
    begin
      InsertQuery.ExecSQL;
      Connection.Transaction.CommitRetaining;
      if (Connection is TSQLite3Connection) then
        Result := TSQLite3Connection(Connection).GetInsertID
      else if (Connection is TConnectionName{MySql}) then
        Result := TConnectionName(Connection).GetInsertID;
    end;
  finally
    InsertQuery.Destroy;
  end;
end;

procedure TSQLDbAdapter.SetSQL(Dataset: TDataSet; const SQL: String);
var
  DS: TSQLQuery absolute Dataset;
begin
  DS.SQL.Text := SQL;
end;

{ TSQLDbResourceClient }

function TSQLDbResourceClient.CreateAdapter: TDatasetAdapter;
begin
  Result := TSQLDbAdapter.Create;
end;

end.

