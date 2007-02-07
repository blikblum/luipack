program delphi2fpc;

{$mode objfpc}
{$H+}

uses 
  sysutils,classes,dbutils, sqliteds;
  
var 
  ConvertDb: TDatapacketDumper;  
  ADb:TSqliteDataSet;  

procedure CreateTable;
begin
  ADb.CreateTable;
end;  

begin
  ADb:=TSqliteDataSet.Create(nil);
  with ADb do
  begin
    FileName:='categoria.db';
    TableName:='categoria';
    PrimaryKey:='CodCategoria';
    SaveOnClose:=True;   
  end;  
  
	ConvertDb:= TDatapacketDumper.Create;
	with ConvertDb do
	begin
	  SourceFile:='categoria.xml';
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