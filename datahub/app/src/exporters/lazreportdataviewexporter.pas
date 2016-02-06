unit LazReportDataViewExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataModel, DataViewExporter, DataView;

type

  { TLazReportBooleanGroupDataViewExporter }

  TLazReportBooleanGroupDataViewExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

  { TJSONLazReportDataViewExporter }

  TJSONLazReportDataViewExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

implementation

uses
  LazReportExporterConfigView;

{ TJSONLazReportDataViewExporter }

class function TJSONLazReportDataViewExporter.Description: String;
begin
  Result := 'JSON LazReport (Quick and dirty)';
end;

class procedure TJSONLazReportDataViewExporter.Execute(DataView: TDataView);
var
  ConfigForm: TLazReportExporterConfigForm;
  Output: TStringList;
  Field: TDataViewField;
  i: Integer;
begin
  ConfigForm := TLazReportExporterConfigForm.Create(nil);
  try
    ConfigForm.View := DataView;
    ConfigForm.ShowModal;
  finally
    ConfigForm.Destroy;
  end;
  Exit;

  Output := TStringList.Create;
  for i := 0 to DataView.Fields.Count -1 do
  begin
    Field := DataView.Fields[i];
    case Field.FieldType of
      dftBoolean:
        Output.Add(Format('([IFNULL(%s,'' '', ''X'')]) %s', [LowerCase(Field.FieldName), Field.DisplayLabel]));
    else
        Output.Add(Format('%s: [%s]', [Field.DisplayLabel, LowerCase(Field.FieldName)]));
    end;
  end;
  Output.SaveToFile('c:\temp\jsonlazreporttest.txt');
  Output.Destroy;
end;

{ TLazReportBooleanGroupDataViewExporter }

class function TLazReportBooleanGroupDataViewExporter.Description: String;
begin
  Result := 'LazReport Boolean Group (Quick and Dirty)';
end;

class procedure TLazReportBooleanGroupDataViewExporter.Execute(DataView: TDataView);
var
  Output: TStringList;
  Field: TDataViewField;
  i: Integer;
begin
  Output := TStringList.Create;
  for i := 0 to DataView.Fields.Count -1 do
  begin
    Field := DataView.Fields[i];
    Output.Add(Format('%s ([IFNULL(%s, 0, IFTHEN(%1:s, 1, 2))])', [Field.DisplayLabel, Field.FieldName]));
  end;
  Output.SaveToFile('lazreportgrouptest.txt');
  Output.Destroy;
end;

end.

