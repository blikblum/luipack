unit LCLJSONDataViewExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataViewExporter, DataModel, DataView;

type

  { TLCLJSONBooleanGroupExporter }

  TLCLJSONBooleanGroupExporter = class(TDataViewExporter)
  public
    class function Description: String; override;
    class procedure Execute(DataView: TDataView); override;
  end;

implementation

uses
  LCLViewBuilder;

{ TLCLJSONBooleanGroupExporter }

class function TLCLJSONBooleanGroupExporter.Description: String;
begin
  Result := 'JSON Boolean Group'
end;

class procedure TLCLJSONBooleanGroupExporter.Execute(DataView: TDataView);
var
  JSONBooleanGroupView: TLCLJSONBooleanGroupView;
begin
  JSONBooleanGroupView := TLCLJSONBooleanGroupView.Create;
  try
    JSONBooleanGroupView.View := DataView;
    JSONBooleanGroupView.Save('testjsonboolean');
  finally
    JSONBooleanGroupView.Destroy;
  end;
end;

end.

