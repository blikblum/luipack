program RESTClientLCLTestRunner;

{$mode objfpc}{$H+}

uses
  SysUtils, Interfaces, Forms, GUITestRunner, LuiRESTClient, fpjsonrtti, fpjson, LuiJSONUtils,
  JSONResourceTests, DatasetResourceTests;

{$R *.res}
var
  Resources: TRESTResourceClient;

procedure InitializeRESTClient;
var
  DeStreamer: TJSONDeStreamer;
  ModelDefsData: TJSONArray;
begin
  Resources := TRESTResourceClient.Create(Application);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    if TryReadJSONFile('resourcedefs.json', ModelDefsData) then
    begin
      DeStreamer.JSONToCollection(ModelDefsData, Resources.ModelDefs);
      ModelDefsData.Free;
    end
    else
      raise Exception.Create('Unable to load file');
  finally
    DeStreamer.Destroy;
  end;
  Resources.BaseURL := 'http://localhost/cgi-bin/addressbook/addressbook.cgi';
end;

begin
  InitializeRESTClient;
  TJSONResourceTests.SetClient(Resources);
  TDatasetResourceTests.SetClient(Resources);
  Application.Initialize;
  RunRegisteredTests;
end.

