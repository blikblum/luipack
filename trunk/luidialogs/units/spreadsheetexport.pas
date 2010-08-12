unit SpreadsheetExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpspreadsheet, xlsbiff8, fpsopendocument, db;


procedure ConvertDatasetToSpreadSheet(Dataset: TDataset; FieldList: TStrings; const FileName: String;
  Format: TsSpreadsheetFormat);

implementation

uses
  FileUtil;

procedure ConvertDatasetToSpreadSheet(Dataset: TDataset; FieldList: TStrings; const FileName: String;
  Format: TsSpreadsheetFormat);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  Col, Row, OldRecNo: Integer;
  Field: TField;
begin
  MyWorkbook := TsWorkbook.Create;
  try
    MyWorksheet := MyWorkbook.AddWorksheet('relatorio');
    for Col := 0 to FieldList.Count - 1 do
      MyWorksheet.WriteUTF8Text(0, Col, FieldList[Col]);
    OldRecNo := Dataset.RecNo;
    Dataset.DisableControls;
    try
      Dataset.First;
      Row := 1;
      while not Dataset.EOF do
      begin
        for Col := 0 to FieldList.Count - 1 do
        begin
          Field := TField(FieldList.Objects[Col]);
          case Field.DataType of
            ftInteger, ftFloat, ftSmallint, ftLargeint, ftWord:
              MyWorksheet.WriteNumber(Row, Col, Field.AsFloat)
          else
            MyWorksheet.WriteUTF8Text(Row, Col, Field.Text);
          end;
        end;
        Inc(Row);
        Dataset.Next;
      end;
    finally
      if OldRecNo <> -1 then
        Dataset.RecNo := OldRecNo;
      Dataset.EnableControls;
    end;
    MyWorkbook.WriteToFile(UTF8ToSys(FileName), Format);
  finally
    MyWorkbook.Destroy;
  end;
end;



end.

