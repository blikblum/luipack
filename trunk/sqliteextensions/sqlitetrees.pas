unit sqlitetrees;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, customsqliteds, sqlite3wrapper, db;

type
  TCustomTreeIterator = class;

  TRecordNotify = procedure (Sender: TCustomTreeIterator; HasChild: Boolean) of Object;
  TParentChangeNotify = procedure (Sender: TCustomTreeIterator; Parent: Integer) of Object;

  { TCustomTreeIterator }

  TCustomTreeIterator = class (TComponent)
  private
    FBreakRecursion: Boolean;
    FFilter: String;
    FIndexFieldName: String;
    FLevel: Integer;
    FOnParentChange: TParentChangeNotify;
    FOnRecord: TRecordNotify;
    FParentFieldName: String;
    FTableName: String;
    FSqlTemplate: String;
    FHasChildSql: String;
  protected
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); virtual; abstract;
    procedure GetChild(Parent: Integer);
  public
    Data: Pointer;
    procedure Run (Parent: Integer; Recurse: Boolean = True); virtual; abstract;
    property TableName: String read FTableName write FTableName;
    property ParentFieldName: String read FParentFieldName write FParentFieldName;
    property IndexFieldName: String read FIndexFieldName write FIndexFieldName;
    property Filter: String read FFilter write FFilter;
    property Level: Integer read FLevel;
    property BreakRecursion: Boolean read FBreakRecursion write FBreakRecursion;
    property OnRecord: TRecordNotify read FOnRecord write FOnRecord;
    property OnParentChange: TParentChangeNotify read FOnParentChange write FOnParentChange;
  end;

  { TDatasetTreeIterator }

  TDatasetTreeIterator = class (TCustomTreeIterator)
  private
    FDataset: TCustomSqliteDataset;
    FIndexField: TField;
  protected
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); override;
    procedure BuildSqlTemplate;
  public
    procedure Run (Parent: Integer; Recurse: Boolean = True); override;
    property Dataset: TCustomSqliteDataset read FDataset write FDataset;
  end;


  { TSqlite3TreeIterator }

  TSqlite3TreeIterator = class (TCustomTreeIterator)
  private
    FFieldNames: String;
    FConnection: TSqlite3Connection;
    FIndexField: Integer;
    FReader: TSqlite3DataReader;
  protected
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property FieldNames: String read FFieldNames write FFieldNames;
    procedure Run (Parent: Integer; Recurse: Boolean = True); override;
    property Connection: TSqlite3Connection read FConnection write FConnection;
    property Reader: TSqlite3DataReader read FReader;
  end;


implementation

{ TDatasetTreeIterator }

type
  TCustomSqliteAccess = class (TCustomSqliteDataset)
  end;

procedure TDatasetTreeIterator.DoBrowseRecords(Parent: Integer; ChildList: TFpList);
var
  HasChild: Boolean;
  CurrentLinkValue: Integer;
begin
  with FDataset do
  begin
    Sql:=FSqlTemplate + IntToStr(Parent) + FFilter;
    RefetchData;
    while not Eof do
    begin
      CurrentLinkValue:=FIndexField.AsInteger;
      ExecuteDirect(FHasChildSql + IntToStr(CurrentLinkValue) + ' Limit 1');
      HasChild:=ReturnCode = 100 {SQLITE_ROW};
      FOnRecord(Self,HasChild);
      if HasChild then
        ChildList.Add(Pointer(PtrInt(CurrentLinkValue)));
      Next;
    end;
  end;
end;

procedure TDatasetTreeIterator.BuildSqlTemplate;
var
  i: Integer;
begin
  if FTableName = Dataset.TableName then
    FSqlTemplate:=TCustomSqliteAccess(Dataset).FSelectSqlStr+' Where '
  else
  begin
    with Dataset do
    begin
      FSqlTemplate:='SELECT ';
      for i:= 0 to FieldDefs.Count - 2 do
        FSqlTemplate:=FSqlTemplate+FieldDefs[i].Name+',';
      FSqlTemplate:=FSqlTemplate+FieldDefs[FieldDefs.Count - 1].Name+
        ' FROM '+FTableName+' Where ';
    end;
  end;
end;

procedure TDatasetTreeIterator.Run (Parent: Integer; Recurse: Boolean = True);
var
  OldSaveOnRefetch,OldActive: Boolean;
  OldSql: String;
  OldRecNo: Integer;
begin
  if not Assigned(FOnRecord) then
    raise Exception.Create('OnRecord notify function not set');
  with Dataset do
  begin
    //save ds state
    OldSaveOnRefetch:=SaveOnRefetch;
    SaveOnRefetch:=False;
    OldSql:=Sql;
    OldActive:=Active;
    //prepare template
    FSqlTemplate:='';
    if OldActive then
    begin
      ApplyUpdates;
      OldRecNo:=RecNo;
      BuildSqlTemplate;
    end
    else
      FSqlTemplate:='Select * from '+FTableName+ ' Where ';
    FHasChildSql:='Select _ROWID_ from '+ FTableName + ' Where ' + FParentFieldName + ' = ';
    DisableControls;
    //dummy open to allow refetchdata
    Sql:= FSqlTemplate + '1 = 0';
    Close;
    Open;
    //finish SqlTemplate
    FSqlTemplate:=FSqlTemplate + FParentFieldName + ' = ';
    FIndexField:=FieldByName(FIndexFieldName);
    FLevel:=0;
    FBreakRecursion:=not Recurse;
    //start iteration
    GetChild(Parent);
    //restore ds state
    SaveOnRefetch:=OldSaveOnRefetch;
    Sql:=OldSql;
    Active:=OldActive;
    if Active then
      RecNo:=OldRecNo;
    EnableControls;
  end;
end;

{ TSqlite3TreeIterator }

procedure TSqlite3TreeIterator.DoBrowseRecords(Parent: Integer; ChildList: TFpList);
var
  HasChild: Boolean;
  CurrentLinkValue: Integer;
begin
  with FConnection, FReader do
  begin
    Prepare(FSqlTemplate + IntToStr(Parent) + FFilter,FReader);
    while Step do
    begin
      CurrentLinkValue:= GetInteger(FIndexField);
      ExecSql(FHasChildSql + IntToStr(CurrentLinkValue) + ' Limit 1');
      HasChild:=ReturnCode = 100 {SQLITE_ROW};
      FOnRecord(Self,HasChild);
      if HasChild then
        ChildList.Add(Pointer(PtrInt(CurrentLinkValue)));
    end;
    Finalize;
  end;
end;

constructor TSqlite3TreeIterator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReader:= TSqlite3DataReader.Create;
end;

destructor TSqlite3TreeIterator.Destroy;
begin
  FReader.Destroy;
  inherited Destroy;
end;

procedure TSqlite3TreeIterator.Run(Parent: Integer; Recurse: Boolean);
begin
  if not Assigned(FOnRecord) then
    raise Exception.Create('OnRecord notify function not set');
  with FConnection do
  begin
    if FFieldNames = '' then
      FSqlTemplate:='Select * from '
    else
      FSqlTemplate:='Select '+FFieldNames+' from ';
    FSqlTemplate:=FSqlTemplate+FTableName+ ' Where ';
    Open;
    Prepare(FSqlTemplate+'1 = 0',FReader);
    FIndexField:=FReader.GetFieldIndex(FIndexFieldName);
    FReader.Finalize;
    if FIndexField = -1 then
      raise Exception.Create('Index Field "'+FIndexFieldName+'" Not Found');
    FSqlTemplate:=FSqlTemplate + FParentFieldName + ' = ';
    FHasChildSql:='Select _ROWID_ from '+ FTableName + ' Where ' + FParentFieldName + ' = ';
    FLevel:=0;
    FBreakRecursion:=not Recurse;
    //start iteration
    GetChild(Parent);
  end;
end;

{ TCustomTreeIterator }

procedure TCustomTreeIterator.GetChild(Parent: Integer);
var
  i: Integer;
  HasChildList: TFpList;
begin
  if Assigned(FOnParentChange) then
    FOnParentChange(Self,Parent);
  HasChildList:=TFPList.Create;
  DoBrowseRecords(Parent, HasChildList);
  inc(FLevel);
  for i:= 0 to HasChildList.Count - 1 do
    if not FBreakRecursion then
      GetChild(PtrInt(HasChildList[i]));
  dec(FLevel);
  HasChildList.Destroy;
end;

end.

