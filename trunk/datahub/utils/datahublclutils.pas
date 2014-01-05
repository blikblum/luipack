unit DataHubLCLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus;

procedure BuildExporterMenu(Menu: TPopupMenu; Event: TNotifyEvent);

procedure BuildImporterMenu(Menu: TPopupMenu; Event: TNotifyEvent);

implementation

uses
  DataViewExporter, DataModelImporter;

procedure BuildExporterMenu(Menu: TPopupMenu; Event: TNotifyEvent);
var
  i: Integer;
  ExporterClass: TDataViewExporterClass;
  MI: TMenuItem;
begin
  for i := 0 to ExporterClassStore.Count - 1 do
  begin
    ExporterClass := ExporterClassStore[i];
    MI := TMenuItem.Create(Menu);
    MI.Caption := ExporterClass.Description;
    MI.Tag := PtrInt(ExporterClass);
    MI.OnClick := Event;
    Menu.Items.Add(MI);
  end;
end;

procedure BuildImporterMenu(Menu: TPopupMenu; Event: TNotifyEvent);
var
  i: Integer;
  ImporterClass: TDataModelImporterClass;
  MI: TMenuItem;
begin
  for i := 0 to ImporterClassStore.Count - 1 do
  begin
    ImporterClass := ImporterClassStore[i];
    MI := TMenuItem.Create(Menu);
    MI.Caption := ImporterClass.Description;
    MI.Tag := PtrInt(ImporterClass);
    MI.OnClick := Event;
    Menu.Items.Add(MI);
  end;
end;

end.

