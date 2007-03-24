unit sqlite3wrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3;
  
const
  SQLITE_OK = sqlite3.SQLITE_OK;
  SQLITE_ROW = sqlite3.SQLITE_ROW;

type
  TSqlite3DataReader = class;
  
  { TSqlite3Query }

  TSqlite3Query = class
  private
    FStatement: Pointer;
    procedure Reset(stm: Pointer);
  public
    destructor Destroy; override;
    function GetInteger: Integer;
    procedure GetList(List: TStrings; FillObjects: Boolean = False);
    function GetString: String;
    function IsNull: Boolean;
  end;

  { TSqlite3Connection }

  TSqlite3Connection = class (TComponent)
  private
    FFileName: String;
    FHandle: Pointer;
    FReturnCode: Integer;
    FSharedHandle: Boolean;
    FQuery: TSqlite3Query;
    procedure SetFileName(const AValue: String);
    procedure SetHandle(AValue: Pointer);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Close;
    function Query(const SQL: String): TSqlite3Query;
    function Open: Boolean;
    procedure ExecSql (const SQL: String);
    procedure Prepare (const SQL: String; Reader: TSqlite3DataReader);
    function ReturnString: String;
    property FileName: String read FFileName write SetFileName;
    property Handle: Pointer read FHandle write SetHandle;
    property ReturnCode: Integer read FReturnCode;
  end;

  { TSqlite3DataReader }

  TSqlite3DataReader = class
  private
    FStatement: Pointer;
    function GetFieldNames(Index: Integer): String;
  protected
    function GetFieldCount: Integer;
  public
    destructor Destroy; override;
    procedure Finalize;
    function GetFieldIndex(const FieldName: String): Integer;
    function GetInteger(AFieldIndex: Integer): Integer; //inline;
    function GetInteger(const FieldName: String): Integer;
    function GetString(AFieldIndex: Integer): String; //inline;
    function GetString(const FieldName: String): String;
    function Step: Boolean;
    property FieldCount: Integer read GetFieldCount;
    property FieldNames[Index: Integer]: String read GetFieldNames;
  end;

implementation

{ TSqlite3Connection }

procedure TSqlite3Connection.SetFileName(const AValue: String);
begin
  if FFileName <> AValue then
  begin
    Close;
    FFileName:=AValue;
  end;
end;

procedure TSqlite3Connection.SetHandle(AValue: Pointer);
begin
  if (FHandle <> AValue) and (FHandle <> nil) then
  begin
    Close;
    FHandle:= AValue;
    FSharedHandle:= True;
  end;
end;

constructor TSqlite3Connection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery:=TSqlite3Query.Create;
end;

destructor TSqlite3Connection.Destroy;
begin
  FQuery.Destroy;
  Close;
  inherited Destroy;
end;

procedure TSqlite3Connection.Close;
begin
  if (FHandle <> nil) then
  begin
    if not FSharedHandle then
      sqlite3_close(FHandle);
    FHandle:=nil;
  end;
end;

function TSqlite3Connection.Query(const SQL: String): TSqlite3Query;
var
  stm: Pointer;
begin
  FReturnCode := sqlite3_prepare(FHandle,PChar(SQL),-1,@stm,nil);
  if FReturnCode = SQLITE_OK then
  begin
    FQuery.Reset(stm);
    Result:=FQuery;
  end
  else
    raise Exception.Create('Error in Query: '+ReturnString);
end;

function TSqlite3Connection.Open: Boolean;
var
  stm: Pointer;
begin
  if FHandle <> nil then
  begin
    Result:=True;
    Exit;
  end;
  Result:=False;
  if FileExists(FFileName) then
  begin
    FReturnCode:= sqlite3_open(PChar(FFileName),@FHandle);
    //additional check
    if FReturnCode = SQLITE_OK then
    begin
      FReturnCode:= sqlite3_prepare(FHandle,'Select Name from sqlite_master LIMIT 1',-1, @stm,nil);
      sqlite3_finalize(stm);
      Result:= FReturnCode = SQLITE_OK;
    end;
    FSharedHandle:= False;
  end;
end;

procedure TSqlite3Connection.ExecSql(const SQL: String);
var
  stm: Pointer;
begin
  FReturnCode:=sqlite3_prepare(FHandle,PChar(SQL),-1,@stm,nil);
  if FReturnCode = SQLITE_OK then
  begin
    FReturnCode:=sqlite3_step(stm);
    sqlite3_finalize(stm);
  end
  else
    raise Exception.Create('Error in ExecSql: '+ReturnString);
end;

procedure TSqlite3Connection.Prepare(const SQL: String; Reader: TSqlite3DataReader);
begin
  FReturnCode:= sqlite3_prepare(FHandle,PChar(SQL),-1,@Reader.FStatement,nil);
  if FReturnCode <> SQLITE_OK then
    raise Exception.Create('Error in Prepare: '+ReturnString);
end;

function TSqlite3Connection.ReturnString: String;
begin
  case FReturnCode of
    SQLITE_OK           : Result := 'SQLITE_OK';
    SQLITE_ERROR        : Result := 'SQLITE_ERROR';
    SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL';
    SQLITE_PERM         : Result := 'SQLITE_PERM';
    SQLITE_ABORT        : Result := 'SQLITE_ABORT';
    SQLITE_BUSY         : Result := 'SQLITE_BUSY';
    SQLITE_LOCKED       : Result := 'SQLITE_LOCKED';
    SQLITE_NOMEM        : Result := 'SQLITE_NOMEM';
    SQLITE_READONLY     : Result := 'SQLITE_READONLY';
    SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT';
    SQLITE_IOERR        : Result := 'SQLITE_IOERR';
    SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT';
    SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND';
    SQLITE_FULL         : Result := 'SQLITE_FULL';
    SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN';
    SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL';
    SQLITE_EMPTY        : Result := 'SQLITE_EMPTY';
    SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA';
    SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG';
    SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT';
    SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH';
    SQLITE_MISUSE       : Result := 'SQLITE_MISUSE';
    SQLITE_NOLFS        : Result := 'SQLITE_NOLFS';
    SQLITE_AUTH         : Result := 'SQLITE_AUTH';
    SQLITE_FORMAT       : Result := 'SQLITE_FORMAT';
    SQLITE_RANGE        : Result := 'SQLITE_RANGE';
    SQLITE_ROW          : Result := 'SQLITE_ROW';
    SQLITE_NOTADB       : Result := 'SQLITE_NOTADB';
    SQLITE_DONE         : Result := 'SQLITE_DONE';
  else
    Result:='Unknow Return Value';
  end;
  Result:=Result+' - '+sqlite3_errmsg(FHandle);
end;

function TSqlite3DataReader.Step: Boolean;
begin
  Result:= sqlite3_step(FStatement) = SQLITE_ROW;
end;

function TSqlite3DataReader.GetFieldIndex(const FieldName: String): Integer;
var
  i: Integer;
begin
  for i:= 0 to FieldCount - 1 do
    if stricomp(PChar(FieldName),sqlite3_column_name(FStatement,i)) = 0 then
    begin
      Result:=i;
      Exit;
    end;
  Result:= -1;
end;

function TSqlite3DataReader.GetFieldNames(Index: Integer): String;
begin
  if (Index >= 0) and (Index < FieldCount) then
    Result:=sqlite3_column_name(FStatement,Index)
  else
    raise Exception.Create('GetFieldNames - Index out of bounds');
end;

function TSqlite3DataReader.GetFieldCount: Integer;
begin
  Result:=sqlite3_column_count(FStatement);
end;

destructor TSqlite3DataReader.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TSqlite3DataReader.Finalize;
begin
  if FStatement <> nil then
  begin
    sqlite3_finalize(FStatement);
    FStatement := nil;
  end;
end;

function TSqlite3DataReader.GetInteger(AFieldIndex: Integer): Integer;
begin
  Result:= sqlite3_column_int(FStatement,AFieldIndex);
end;

function TSqlite3DataReader.GetInteger(const FieldName: String): Integer;
var
  i: Integer;
begin
  i:= GetFieldIndex(FieldName);
  if i <> -1 then
    Result:= sqlite3_column_int(FStatement,i)
  else
    raise Exception.Create('TSqlite3Wrapper - Field "'+FieldName+'" not found');
end;

function TSqlite3DataReader.GetString(AFieldIndex: Integer): String;
begin
  Result:= StrPas(sqlite3_column_text(FStatement,AFieldIndex));
end;

function TSqlite3DataReader.GetString(const FieldName: String): String;
var
  i: Integer;
begin
  i:= GetFieldIndex(FieldName);
  if i <> -1 then
    Result:= StrPas(sqlite3_column_text(FStatement,i))
  else
    raise Exception.Create('TSqlite3Wrapper - Field "'+FieldName+'" not found');
end;

{ TSqlite3Query }

procedure TSqlite3Query.Reset(stm: Pointer);
begin
  if FStatement <> nil then
    sqlite3_finalize(FStatement);
  FStatement:=stm;
  sqlite3_step(FStatement);
end;

destructor TSqlite3Query.Destroy;
begin
  if FStatement <> nil then
    sqlite3_finalize(FStatement);
end;

function TSqlite3Query.GetInteger: Integer;
begin
  Result:= sqlite3_column_int(FStatement,0);
end;

procedure TSqlite3Query.GetList(List: TStrings; FillObjects: Boolean);
begin
  //todo
end;

function TSqlite3Query.GetString: String;
begin
  Result:= StrPas(sqlite3_column_text(FStatement,0));
end;

function TSqlite3Query.IsNull: Boolean;
begin
  Result:= sqlite3_column_type(FStatement,0) = SQLITE_NULL;
end;

end.

