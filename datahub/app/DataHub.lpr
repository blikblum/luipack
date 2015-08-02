program DataHub;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main,
  DataModelImporter, DataViewExporter,
  LCLDBDataViewExporter, LazReportDataViewExporter, LCLJSONDataViewExporter, HTMLFormExport,
  Sqlite3DataModelImporter;

{$R *.res}


begin
  //register exporters...
  ExporterClassStore.Register(TLCLDBDataViewExporter);
  ExporterClassStore.Register(TLCLDataViewExporter);
  ExporterClassStore.Register(TLCLJSONBooleanGroupExporter);
  ExporterClassStore.Register(TLazReportBooleanGroupDataViewExporter);
  ExporterClassStore.Register(TJSONLazReportDataViewExporter);
  ExporterClassStore.Register(THTMLDataViewExporter);

  //...and importers
  ImporterClassStore.Register(TSqlite3DataModelImporter);

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

