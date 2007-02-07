program testcsv2dataset;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, miscutils, db, sqlite3ds;
  
type

  { TDummy }

  TDummy = class
    procedure NewColumn(const Value: String; Index: Integer);
    procedure StartRow (RowNo: Integer);
    procedure EndRow (RowNo: Integer);
  end;


var
  ADummy: TDummy;
  ACsv: TCSVFile;
  ADb: TSqlite3Dataset;


{ TDummy }

procedure TDummy.NewColumn(const Value: String; Index: Integer);
begin
  ADb.Fields[Index].AsString:=Value;
end;

procedure TDummy.StartRow(RowNo: Integer);
begin
  ADb.Append;
end;

procedure TDummy.EndRow(RowNo: Integer);
begin
  ADb.Post;
end;


begin
  ADb:=TSqlite3Dataset.Create(nil);
  with Adb do
  begin
    FileName:='turkish.db3';
    TableName:='musteriler_lite';
    PrimaryKey:='ind';
    if not TableExists then
    begin
      FieldDefs.Add('ind',ftString);
      FieldDefs.Add('musteri_no',ftString);
      FieldDefs.Add('isim',ftString);
      CreateTable;
    end;
    Open;
  end;
  ACsv:=TCSVFile.Create;
  ADummy:=TDummy.Create;
  with ACsv do
  begin
    Filename:='turkish.csv';
    Delimiter:=',';
    OnStartRow:=@ADummy.StartRow;
    OnEndRow:=@ADummy.EndRow;
    OnColumn:=@ADummy.NewColumn;
    Parse;
    ADb.ApplyUpdates;
    Destroy;
  end;
  ADummy.Destroy;
  ADb.Destroy;
end.

