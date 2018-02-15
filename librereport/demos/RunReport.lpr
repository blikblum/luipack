program RunReport;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, LibreReport, fpjson, LuiJSONUtils;

var
  Report: TLibreReport;
  ConfigData: TJSONObject;
  TargetName: String;

begin
  TargetName := 'prescription';
  if ParamCount > 0 then
    TargetName := ParamStr(1);

  if TryReadJSONFile(Format('%s-config.json', [TargetName]), ConfigData) then
  begin
    Report := TLibreReport.Create(nil);
    try
      Report.LoadFromFile('prescription-template.odt');
      Report.Render(ConfigData.Objects['data']);
      Report.ExportTo(ConfigData.Get('output', Format('%s-output.odt', [TargetName])));
    finally
      Report.Destroy;
    end;
  end;
end.

