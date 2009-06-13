unit SqliteUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomSqliteDS, fgl, db;

type

  TSqliteQueryBuilderOption = (sqoSaveToSqlList);

  TSqliteQueryBuilderOptions = set of TSqliteQueryBuilderOption;

  TIntegerList = specialize TFPGList <Integer>;

  { TSqliteQueryBuilder }

  TSqliteQueryBuilder = class(TComponent)
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
    FDeleteTemplate: String;
    FInsertTemplate: String;
    FUpdateTemplate: String;
    FPrimaryKeyTemplate: String;
    FFieldValues: array of String;
    FFieldPrimaryKey: array of String;
    procedure BuildTemplates;
    function FormatPrimaryKeyList(Values: PPChar): String;
    function FormatValueList(const Template: String; Values: PPChar): String;
    procedure ParseUpdate(UserData: Pointer; Values: PPChar; ABookmark: TBookmark; State: TRecordState);
    procedure SetExcludeFields(const AValue: TStrings);
    procedure SetIncludeFields(const AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property SQL: TStrings read FSQL;
  published
    property Dataset: TCustomSqliteDataset read FDataset write FDataset;
    property ExcludeFields: TStrings read FExcludeFields write SetExcludeFields;
    property IncludeFields: TStrings read FIncludeFields write SetIncludeFields;
    property Options: TSqliteQueryBuilderOptions read FOptions write FOptions default [sqoSaveToSqlList];
    property PrimaryKey: TStrings read FPrimaryKey;
    property RecordStates: TRecordStateSet read FRecordStates write FRecordStates default [rsAdded, rsUpdated, rsDeleted];
    property TableName: String read FTableName write FTableName;
  end;

implementation

//replace parameters
//modified from StrUtils StringsReplace
function ReplaceParams(const S: string; Pattern: array of string): string;

var pc,pcc,lastpc : pchar;
    strcount      : integer;
    ResStr,
    CompStr,
    ParamIndex    : string;
    Found         : Boolean;
    sc            : integer;

begin
  sc := length(Pattern);
  dec(sc);

  CompStr := s;

  ResStr := '';
  pc := @CompStr[1];
  pcc := @s[1];
  lastpc := pc+Length(S);

  while pc < lastpc do
    begin
    Found := False;
    for strcount := 0 to sc do
      begin
      ParamIndex := '%' + IntToStr(strcount) + '%';
      if (ParamIndex[1]=pc^) and
         (Length(ParamIndex) <= (lastpc-pc)) and
         (CompareByte(ParamIndex[1],pc^,Length(ParamIndex))=0) then
        begin
        ResStr := ResStr + Pattern[strcount];
        pc := pc+Length(ParamIndex);
        pcc := pcc+Length(ParamIndex);
        Found := true;
        end
      end;
    if not found then
      begin
      ResStr := ResStr + pcc^;
      inc(pc);
      inc(pcc);
      end;
    end;
  Result := ResStr;
end;

{ TSqliteQueryBuilder }

procedure TSqliteQueryBuilder.BuildTemplates;
var
  i: Integer;
  FieldName, ValueList: String;
begin
  FDeleteTemplate := 'DELETE FROM ' + FWorkTableName + ' WHERE ';
  FInsertTemplate := 'INSERT INTO ' + FWorkTableName + '(';
  FUpdateTemplate := 'UPDATE ' + FWorkTableName + ' SET ';
  ValueList := '';
  for i := 0 to FFieldList.Count - 2 do
  begin
    ValueList := ValueList + '%' + IntToStr(i) + '%,';
    FieldName := FDataset.FieldDefs[i].Name;
    FInsertTemplate := FInsertTemplate + FieldName + ',';
    FUpdateTemplate := FUpdateTemplate + FieldName + ' = %' + IntToStr(i) + '%,';
  end;
  //set the last field
  i := FFieldList.Count - 1;
  FieldName := FDataset.FieldDefs[i].Name;
  ValueList := ValueList + '%' + IntToStr(i) + '%';
  FInsertTemplate := FInsertTemplate + FieldName + ') VALUES (' + ValueList + ');';
  FUpdateTemplate := FUpdateTemplate + FieldName + '= %' + IntToStr(i) + '% WHERE ';

  //primary key
  FPrimaryKeyTemplate := '';
  for i := 0 to FPrimaryKeyList.Count - 1 do
  begin
    FieldName := FDataset.FieldDefs[i].Name;
    FPrimaryKeyTemplate := FPrimaryKeyTemplate + FieldName + ' = %' + IntToStr(i) + '%';
    if i < FPrimaryKeyList.Count - 1 then
      FPrimaryKeyTemplate := FPrimaryKeyTemplate + ' AND '
  end;
  FPrimaryKeyTemplate := FPrimaryKeyTemplate + ';';
end;

function TSqliteQueryBuilder.FormatPrimaryKeyList(Values: PPChar): String;
var
  i: Integer;
begin
  for i := 0 to FPrimaryKeyList.Count - 1 do
    FFieldPrimaryKey[i] := FDataset.GetSQLValue(Values, FPrimaryKeyList[i]);
  Result := ReplaceParams(FPrimaryKeyTemplate, FFieldPrimaryKey);
end;

function TSqliteQueryBuilder.FormatValueList(const Template: String;
  Values: PPChar): String;
var
  i: Integer;
begin
  for i := 0 to FFieldList.Count - 1 do
    FFieldValues[i] := FDataset.GetSQLValue(Values, FFieldList[i]);
  Result := ReplaceParams(Template, FFieldValues);
end;

procedure TSqliteQueryBuilder.ParseUpdate(UserData: Pointer; Values: PPChar;
  ABookmark: TBookmark; State: TRecordState);
begin
  case State of
    rsAdded:
    begin
      FWorkSql.Add(FormatValueList(FInsertTemplate, Values));
    end;
    rsDeleted:
    begin
      FWorkSql.Add(FDeleteTemplate + FormatPrimaryKeyList(Values));
    end;
    rsUpdated:
    begin
      FWorkSql.Add(FormatValueList(FUpdateTemplate, Values) +
        FormatPrimaryKeyList(Values));
    end;
  end;
end;

procedure TSqliteQueryBuilder.SetExcludeFields(const AValue: TStrings);
begin
  FExcludeFields.Assign(AValue);
end;

procedure TSqliteQueryBuilder.SetIncludeFields(const AValue: TStrings);
begin
  FIncludeFields.Assign(AValue);
end;

constructor TSqliteQueryBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  SetLength(FFieldValues, FFieldList.Count);

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
  SetLength(FFieldPrimaryKey, FPrimaryKeyList.Count);

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

