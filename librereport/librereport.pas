unit LibreReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  { TLibreReport }

  TLibreReport = class(TComponent)
  private
    FDocument: Variant;
    FReflection: Variant;
    FServiceManager: Variant;
    function CreateStruct(const strTypeName: WideString): Variant;
    function GetConfigData: TJSONObject;
    procedure RenderTable(Data: TJSONObject; Table: Variant; const Path: String; Row: Integer);
    procedure RenderTables(Data, ConfigData: TJSONObject);
    procedure ServiceNeeded;
  public
    procedure ExportTo(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure Render(Data: TJSONObject);
  end;

implementation

uses
  variants, comobj, LuiJSONUtils, LuiJSONHelpers;

const
  ServiceName = 'com.sun.star.ServiceManager';

{ TLibreReport }

function TLibreReport.CreateStruct(const strTypeName: WideString): Variant;
var
  IdlClass: Variant;
begin
  IdlClass := FReflection.forName(strTypeName);
  IdlClass.CreateObject(Result);
end;

function TLibreReport.GetConfigData: TJSONObject;
var
  DocDescription: WideString;
begin
  DocDescription := FDocument.GetDocumentProperties().Description;
  if not TryStrToJSON(DocDescription, Result) then
    Result := TJSONObject.Create;
end;

procedure TLibreReport.RenderTable(Data: TJSONObject; Table: Variant;
  const Path: String; Row: Integer);
var
  ItemsData: TJSONArray;
  Rows: Variant;
begin
  Rows := Table.GetRows();
  if not Data.FindPath(Path, ItemsData) or (ItemsData.Count = 0) then
  begin
    Rows.RemoveByIndex(Row, 1);
  end
  else
  begin
    // Todo find a way to iterate through a table row
    // Rows.GetByIndex returns TextTableRow that do not have methods to do it
    // https://api.libreoffice.org/docs/idl/ref/servicecom_1_1sun_1_1star_1_1text_1_1TextTableRow.html
    // use table text cursor?

  end;
end;

procedure TLibreReport.RenderTables(Data, ConfigData: TJSONObject);
var
  TablesConfigData, TableConfigObj: TJSONObject;
  TableConfigData: TJSONData;
  AllTables, Table: Variant;
  i: Integer;
begin
  if ConfigData.Find('tables', TablesConfigData) then
  begin
    AllTables := Document.GetTextTables();
    for i := 0 to AllTables.GetCount() - 1 do
    begin
      Table := AllTables.GetByIndex(i);
      if TablesConfigData.Find(Table.GetName(), TableConfigData) then
      begin
        case TableConfigData.JSONType of
          jtObject:
            RenderTable(Data, Table, TableConfigObj.Get('path', ''), TableConfigObj.Get('row', -1));
        end;
      end;
    end;
  end;
end;

procedure TLibreReport.LoadFromFile(const FileName: String);
var
  Desktop, LoadParams: Variant;
begin
  ServiceNeeded;
  Desktop := ServiceManager.CreateInstance('com.sun.star.frame.Desktop');
  LoadParams := VarArrayCreate([0, 1], varVariant);
  LoadParams[0] := CreateStruct('com.sun.star.beans.PropertyValue');
  LoadParams[0].Name := 'AsTemplate';
  LoadParams[0].Value := True;
  FDocument := Desktop.LoadComponentFromURL(FileNameToUrl(FileName), '_blank', 0, LoadParams);
end;

procedure TLibreReport.Render(Data: TJSONObject);
var
  ConfigData: TJSONObject;
begin
  if VarIsClear(FDocument) then
    raise Exception.Create('Document template not loaded');
  ConfigData := GetConfigData;
  try
    RenderTables(Data, ConfigData);
  finally
    ConfigData.Destroy;
  end;
end;

procedure TLibreReport.ServiceNeeded;
begin
  if VarIsNull(FServiceManager) then
  begin
    try
      FServiceManager := CreateOleObject(ServiceName);
      FReflection := FServiceManager.CreateInstance('com.sun.star.reflection.CoreReflection');
    except
      raise Exception.Create('Unable to load OpenOffice or LibreOffice service');
    end;
  end;
end;

procedure TLibreReport.ExportTo(const FileName: String);
begin

end;

end.

