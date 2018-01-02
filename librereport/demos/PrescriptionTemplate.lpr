program PrescriptionTemplate;

{$mode objfpc}{$H+}

uses
  Classes, LibreReport, fpjson, LuiJSONUtils;

var
  Report: TLibreReport;
  PrescriptionData: TJSONObject;

begin
  if TryReadJSONFile('prescription-data.json', PrescriptionData) then
  begin
    Report := TLibreReport.Create(nil);
    try
      Report.LoadFromFile('prescription-template.odt');
      Report.Render(PrescriptionData);
      Report.ExportTo('Luiz Américo.odt');
    finally
      Report.Destroy;
    end;
  end;
end.

