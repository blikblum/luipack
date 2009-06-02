unit SqliteUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomSqliteDS, fgl, strutils, db;

type

  TSqliteQueryBuilderOption = (sqoSaveToSqlList);

  TSqliteQueryBuilderOptions = set of TSqliteQueryBuilderOption;

  TIntegerList = specialize TFPGList <Integer>;

  { TSqliteQueryBuilder }

  TSqliteQueryBuilder = class
  private
    FDataset: TCustomSqliteDataset;
    FExcludeFields: TStrings;
    FIncludeFields: TStrings;
    FOptions: TSqliteQueryBuilderOptions;
    FPrimaryKey: TStrings;
    FRecordStates: TRecordStateSet;
    FSQL: TStrings;
    FFieldList: TIntegerList;
    FPrimaryKeyList: TIntegerList;
    FTableName: String;
    FWorkSql: TStrings;
    FWorkTableName: String;
    //templates
    FInsertTemplate: String;
    FUpdateTemplate: String;
    FPrimaryKeyTemplate: String;
    procedure BuildTemplates;
    procedure SetDataset(const AValue: TCustomSqliteDataset);
    procedure SetOptions(const AValue: TSqliteQueryBuilderOptions);
    procedure ParseUpdate(UserData: Pointer; Values: PPChar; ABookmark: TBookmark; State: TRecordState);
    procedure SetRecordStates(const AValue: TRecordStateSet);
    procedure SetTableName(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Dataset: TCustomSqliteDataset read FDataset write SetDataset;
    property ExcludeFields: TStrings read FExcludeFields;
    property IncludeFields: TStrings read FIncludeFields;
    property Options: TSqliteQueryBuilderOptions read FOptions write SetOptions;
    property PrimaryKey: TStrings read FPrimaryKey;
    property RecordStates: TRecordStateSet read FRecordStates write SetRecordStates;
    property SQL: TStrings read FSQL;
    property TableName: String read FTableName write SetTableName;
  end;

implementation

{ TSqliteQueryBuilder }

procedure TSqliteQueryBuilder.BuildTemplates;
var
  i: Integer;
  FieldName: String;
begin
  FInsertTemplate := 'INSERT INTO ' + FWorkTableName + '(';
  FUpdateTemplate := 'UPDATE ' + FWorkTableName + ' SET ';
  for i := 0 to FFieldList.Count - 2 do
  begin
    FieldName := FDataset.FieldDefs[i].Name;
    FInsertTemplate := FInsertTemplate + FieldName + ',';
    FUpdateTemplate := FUpdateTemplate + FieldName + ' = %' + IntToStr(i) + ',';
  end;
  //set the last field
  i := FFieldList.Count - 1;
  FieldName := FDataset.FieldDefs[i].Name;
  FInsertTemplate := FInsertTemplate + FieldName + ') VALUES (%v);';
  FUpdateTemplate := FUpdateTemplate + FieldName + '= %' + IntToStr(i) + ' WHERE ';

  //primary key
  FPrimaryKeyTemplate := '';
  for i := 0 to FPrimaryKeyList.Count - 1 do
  begin
    FieldName := FDataset.FieldDefs[i].Name;
    FPrimaryKeyTemplate := FPrimaryKeyTemplate + FieldName + ' = %' + IntToStr(i);
    if i < FPrimaryKeyList.Count - 1 then
      FPrimaryKeyTemplate := FPrimaryKeyTemplate + ' AND '
  end;
  FPrimaryKeyTemplate := FPrimaryKeyTemplate + ';';
end;

procedure TSqliteQueryBuilder.SetDataset(const AValue: TCustomSqliteDataset);
begin
  if FDataset = AValue then exit;
  FDataset := AValue;
end;

procedure TSqliteQueryBuilder.SetOptions(
  const AValue: TSqliteQueryBuilderOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
end;

procedure TSqliteQueryBuilder.ParseUpdate(UserData: Pointer; Values: PPChar;
  ABookmark: TBookmark; State: TRecordState);
var
  i: Integer;

begin
  case State of
    rsAdded:
    begin

    end;
    rsDeleted:
    begin

    end;
    rsUpdated:
    begin


    end;
  end;
end;

procedure TSqliteQueryBuilder.SetRecordStates(const AValue: TRecordStateSet);
begin
  if FRecordStates = AValue then exit;
  FRecordStates := AValue;
end;

procedure TSqliteQueryBuilder.SetTableName(const AValue: String);
begin
  if FTableName = AValue then exit;
  FTableName := AValue;
end;

constructor TSqliteQueryBuilder.Create;
begin
  FIncludeFields := TStringList.Create;
  FExcludeFields := TStringList.Create;
  FSQL := TStringList.Create;
  FFieldList := TIntegerList.Create;
  FPrimaryKeyList := TIntegerList.Create;
  FPrimaryKey := TStringList.Create;
  FPrimaryKey.Delimiter := ',';
  FPrimaryKey.StrictDelimiter := True;
  FRecordStates := [rsAdded, rsUpdated, rsDeleted];
end;

destructor TSqliteQueryBuilder.Destroy;
begin
  FIncludeFields.Destroy;
  FExcludeFields.Destroy;
  FSQL.Destroy;
  FFieldList.Destroy;
  FPrimaryKeyList.Destroy;
  FPrimaryKey.Destroy;
  inherited Destroy;
end;

procedure TSqliteQueryBuilder.Execute;
var
  i, j: Integer;
  EmptyPrimaryKey: Boolean;
begin
  if FDataset = nil then
    raise Exception.Create('Dataset not set');

  FFieldList.Clear;
  //add the fields of the include list
  for i := 0 to FIncludeFields.Count - 1 do
  begin
    j := FDataset.FieldDefs.IndexOf(FIncludeFields[i]);
    if j <> -1 then
      FFieldList.Add(j);
  end;
  //if include list is empty or if no field is found, add all
  if FFieldList.Count = 0 then
  begin
    for i := 0 to FDataset.FieldDefs.Count - 1 do
      FFieldList.Add(i);
  end;
  //now remove the fields of the exclude list
  for i := 0 to FExcludeFields.Count - 1 do
    FFieldList.Remove(FDataset.FieldDefs.IndexOf(FExcludeFields[i]));
  if FFieldList.Count = 0 then
    raise Exception.Create('No field specified');

  //build primary key list
  EmptyPrimaryKey := FPrimaryKey.Count = 0;
  if EmptyPrimaryKey then
    FPrimaryKey.DelimitedText := FDataset.PrimaryKey;
  FPrimaryKeyList.Clear;
  for i := 0 to FPrimaryKey.Count - 1 do
  begin
    j := FDataset.FieldDefs.IndexOf(FPrimaryKey[i]);
    if j <> -1 then
      FPrimaryKeyList.Add(j);
  end;
  if ([rsUpdated, rsDeleted] * FRecordStates <> []) and (FPrimaryKeyList.Count = 0) then
    Exception.Create('A primary key must be set for updated or deleted records');
  if EmptyPrimaryKey then
    FPrimaryKey.Clear;

  if sqoSaveToSqlList in FOptions then
    FWorkSql := FDataset.SQLList
  else
    FWorkSql := FSQL;

  if FTableName = '' then
    FWorkTableName := FDataset.TableName
  else
    FWorkTableName := FTableName;

  BuildTemplates;
  FWorkSql.Clear;
  FWorkSql.Add('BEGIN;');
  FDataset.QueryUpdates(FRecordStates, @ParseUpdate);
  FWorkSql.Add('COMMIT;');
end;

end.

