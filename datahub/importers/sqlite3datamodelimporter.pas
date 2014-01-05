unit Sqlite3DataModelImporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataModel, DataModelImporter;

type

  { TSqlite3DataModelImporter }

  TSqlite3DataModelImporter = class(TDataModelImporter)
  public
    class function Description: String; override;
    class procedure Execute(Models: TDataModels); override;
  end;

implementation

uses
  Sqlite3TableLoaderView, LuiDialogs, Controls;

{ TSqlite3DataModelImporter }

class function TSqlite3DataModelImporter.Description: String;
begin
  Result := 'Sqlite3 Database';
end;

class procedure TSqlite3DataModelImporter.Execute(Models: TDataModels);
var
  Model: TDataModel;
  Result: Boolean;
begin
  Model := TDataModel.Create(nil);
  Model.Name := 'NewModel';
  try
    Result := ShowForm(TSqlite3TableLoaderViewForm, ['Model', Model]) = mrOk;
    if Result then
      Models.Add.Assign(Model);
  finally
    Model.Free;
  end;
end;

end.

