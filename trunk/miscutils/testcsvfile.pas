program testcsvfile;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, miscutils;
  
type

  { TDummy }

  TDummy = class
    procedure NewColumn(const Value: String; Index: Integer);
    procedure NewRow (RowNo: Integer);
  end;

{ TDummy }

procedure TDummy.NewColumn(const Value: String; Index: Integer);
begin
  write(Value,';');
end;

procedure TDummy.NewRow(RowNo: Integer);
begin
  writeln;
end;
  
var
  ADummy: TDummy;
  ACsv: TCSVFile;
begin
  ACsv:=TCSVFile.Create;
  ADummy:=TDummy.Create;
  with ACsv do
  begin
    Filename:='turkish.db';
    Delimiter:=',';
    OnRow:=@ADummy.NewRow;
    OnColumn:=@ADummy.NewColumn;
    Parse;
    Destroy;
  end;
  ADummy.Destroy;
end.

