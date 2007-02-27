program testdatapacket2db;

{$mode objfpc}
{$H+}

uses 
  sysutils, classes, dbutils, sqlite3ds;
  
var 
  ConvertDb: TDatapacketDumper;  
  ADb:TSqlite3DataSet;

procedure CreateTable;
begin
  ADb.CreateTable;
end;  

begin
  ADb:=TSqlite3DataSet.Create(nil);
  with ADb do
  begin
    FileName:='customers.db';
    TableName:='customers';
    PrimaryKey:='CustNo';
    SaveOnClose:=True;   
  end;  
  
  ConvertDb:= TDatapacketDumper.Create;
  with ConvertDb do
  begin
    SourceFile:='test.xml';
    DataSet:=ADb;
    DumpFieldDefs:=True;
    AfterDumpFieldDefs:= @CreateTable;
    try
      DumpData;
    finally
      Destroy;
      ADb.Destroy;
    end;
  end;
end.
