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
    procedure RenderBody(Data: TJSONObject);
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
  variants, comobj, LuiJSONUtils, LuiJSONHelpers, Handlebars, LazFileUtils;

const
  ServiceName = 'com.sun.star.ServiceManager';

function FileNameToUrl(const FileName: String): WideString;
begin
  Result := UTF8Decode(Format('file:///%s', [StringReplace(ExpandFileName(FileName), '\', '/', [rfReplaceAll])]));
end;

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
  if not TryStrToJSON(UTF8Encode(DocDescription), Result) then
    Result := TJSONObject.Create;
end;

procedure TLibreReport.LoadFromFile(const FileName: String);
var
  Desktop, LoadParams: Variant;
begin
  if not FileExistsUTF8(FileName) then
    raise Exception.CreateFmt('LibreReport: file "%s" not found', [FileName]);
  ServiceNeeded;
  Desktop := FServiceManager.CreateInstance('com.sun.star.frame.Desktop');
  LoadParams := VarArrayCreate([0, 1], varVariant);
  LoadParams[0] := CreateStruct('com.sun.star.beans.PropertyValue');
  LoadParams[0].Name := 'AsTemplate';
  LoadParams[0].Value := True;
  FDocument := Desktop.LoadComponentFromURL(FileNameToUrl(FileName), '_blank', 0, LoadParams);
end;

function CreateColumnMap(Table: Variant; Row: Integer): TJSONObject;
var
  RowStr, ColumnText, ColumnTemplate: String;
  CellName: WideString;
  Cursor, Cell: Variant;
  FirstPass: Boolean;
begin
  Result := TJSONObject.Create;
  RowStr := IntToStr(Row);
  CellName := WideString('A' + RowStr);
  Cursor := Table.CreateCursorByCellName(CellName);
  FirstPass := True;
  repeat
    // walk by all columns in the row to be copied...
    CellName := Cursor.getRangeName();
    if not FirstPass and (CellName <> '') and (CellName[1] = 'A') then
      Break;
    FirstPass := False;
    Cell := Table.getCellByName(CellName);
    ColumnText := UTF8Encode(Cell.GetString());
    // ...if is not empty create a column template
    if ColumnText <> '' then
    begin
      ColumnTemplate := StringReplace(CellName, RowStr, '%d', []);
      Result.Add(ColumnTemplate, ColumnText);
    end;
  until not Cursor.GoRight(1, False);
end;

procedure RenderRow(Table: Variant; Row: Integer; RowData, ColumnMapData: TJSONObject);
var
  i: Integer;
  CellName, CellText: WideString;
  Cell: Variant;
begin
  for i := 0 to ColumnMapData.Count - 1 do
  begin
    CellName := Format(ColumnMapData.Names[i], [Row]);
    Cell := Table.GetCellByName(CellName);
    if not VarIsClear(Cell) then
    begin
      CellText := UTF8Decode(RenderTemplate(ColumnMapData.Items[i].AsString, RowData));
      Cell.SetString(CellText);
    end;
  end;
end;

procedure TLibreReport.RenderTable(Data: TJSONObject; Table: Variant;
  const Path: String; Row: Integer);
var
  ItemsData: TJSONArray;
  ItemData: TJSONData;
  Rows: Variant;
  ColumnMapData: TJSONObject;
  i: Integer;
begin
  Rows := Table.GetRows();
  if not Data.FindPath(Path, ItemsData) or (ItemsData.Count = 0) then
  begin
    Rows.RemoveByIndex(Row, 1);
  end
  else
  begin
    Rows.InsertByIndex(Row, ItemsData.Count - 1);
    ColumnMapData := CreateColumnMap(Table, Row);
    try
      for i := 0 to ItemsData.Count - 1 do
      begin
        ItemData := ItemsData.Items[i];
        if ItemData.JSONType = jtObject then
          RenderRow(Table, Row + i, TJSONObject(ItemData), ColumnMapData);
      end;
    finally
      ColumnMapData.Destroy;
    end;
  end;
end;

procedure TLibreReport.RenderTables(Data, ConfigData: TJSONObject);
var
  TablesConfigData: TJSONObject;
  TableConfigData: TJSONData;
  TableConfigObj: TJSONObject absolute TableConfigData;
  AllTables, Table: Variant;
  i: Integer;
begin
  if ConfigData.Find('tables', TablesConfigData) then
  begin
    AllTables := FDocument.GetTextTables();
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

procedure TLibreReport.RenderBody(Data: TJSONObject);
var
  SearchDescriptor, SearchResult: Variant;
  Path: String;
  PathData: TJSONData;
  ReplaceText: WideString;
begin
  SearchDescriptor := FDocument.CreateSearchDescriptor();
  SearchDescriptor.SetSearchString('\{\{.+?\}\}');
  SearchDescriptor.SearchRegularExpression := True;

  SearchResult := FDocument.FindFirst(SearchDescriptor);
  while not VarIsClear(SearchResult) do
  begin
    Path := UTF8Encode(SearchResult.getString());
    Path := Trim(Copy(Path, 3, Length(Path) - 4));
    ReplaceText := '';
    if Data.FindPath(Path, PathData) then
    begin
      case PathData.JSONType of
        jtString:
          ReplaceText := UTF8Decode(StringReplace(PathData.AsString, #13#10, #13, [rfReplaceAll]));
        jtNumber, jtBoolean:
          ReplaceText := UTF8Decode(PathData.AsString);
      end;
    end;

    SearchResult.SetString(ReplaceText);
    SearchResult := FDocument.FindNext(SearchResult.getEnd(), SearchDescriptor);
  end;
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
    RenderBody(Data);
  finally
    ConfigData.Destroy;
  end;
end;

procedure TLibreReport.ServiceNeeded;
begin
  if VarIsEmpty(FServiceManager) then
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
var
  LoadParams: Variant;
begin
  LoadParams := VarArrayCreate([0, 0], varVariant);
  FDocument.storeAsURL(FileNameToUrl(FileName), LoadParams);
end;

end.

