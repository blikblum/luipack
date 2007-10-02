unit ChronoDataProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3ds;

type

  { TChronoDataProvider }

  TChronoDataProvider = class
    FActiveGroup: String;
  private
    FGroupIndex: Integer;
    FDataset: TSqlite3Dataset;
    FGroupList: TStrings;
    FRowList: TStrings;
    FGroupByFieldsStr: String;
    FGroupByWhereStr : String;
    FGroupByStr: String;
    FScaleIndex: Integer;
    FWhereStr: String;
    FScaleStr: String;
    FAverageStr: String;
    FMinStr: String;
    FMaxStr: String;
    FMedianStr: String;
    FMedianStrEnd: String;
    FCountStr: String;
    function GetActionList: TStrings;
    function GetSessionList: TStrings;
    procedure SetActiveGroup(const AValue: String);
    procedure SetGroupIndex(const AValue: Integer);
    procedure LoadList(List: TStrings; const Field, Table: String);
    procedure SetScaleIndex(const AValue: Integer);
    procedure UpdateSqlTemplates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RefreshDataset;
    function GetCount(RowIndex: Integer): String;
    function GetMin(RowIndex: Integer): String;
    function GetMax(RowIndex: Integer): String;
    function GetMedian(RowIndex: Integer): String;
    function GetAvg(RowIndex: Integer): String;
    function GetAvg(const ARow, AGroup: String): String;
    property Dataset: TSqlite3Dataset read FDataset write FDataset;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property ScaleIndex: Integer read FScaleIndex write SetScaleIndex;
    property ActiveGroup: String read FActiveGroup write SetActiveGroup;
    property RowList: TStrings read FRowList;
    property GroupList: TStrings read FGroupList;
    property ActionList: TStrings read GetActionList;
    property SessionList: TStrings read GetSessionList;
  end;

implementation

const
  FAverageTemplate = 'Select ifnull(Cast(Round(Avg(Time)/%s) as Integer),0)';
  FMinTemplate = 'Select Cast(Round(Min(Time)/%s) as Integer)';
  FMaxTemplate = 'Select Cast(Round(Max(Time)/%s) as Integer)';
  FMedianTemplate = 'Select Cast(Round(Time/%s) as Integer)';

  FSessionFieldsTemplate = 'Select Action , Cast (Round(Time/%s) as Integer), Date, Comments from Results, Sessions';
  FSessionWhereTemplate = ' Where Sessions.Name = "%s" and Sessions.Code = Results.SessionId Order by Time;';
  FActionFieldsTemplate = 'Select Name , Cast(Round(Time/%s) as Integer), Date, Comments from Results, Sessions';
  FActionWhereTemplate = ' Where Results.Action = "%s" and Results.SessionId = Sessions.Code Order by Time;';
  FWhereTemplate = ' where  %s = "%s" and sessionid = sessions.code and %s = "%s"';
  FFromTables = ' from results , sessions';

{ TChronoDataProvider }

procedure TChronoDataProvider.SetGroupIndex(const AValue: Integer);
begin
  if FGroupIndex = AValue then
    Exit;
  FGroupIndex := AValue;
  if FGroupIndex = 0 then // group by session
  begin
    LoadList(FGroupList, 'Name', 'Sessions');
    LoadList(FRowList, 'Action', 'Results');
    FGroupByFieldsStr := FSessionFieldsTemplate;
    FGroupByWhereStr := FSessionWhereTemplate;
    FWhereStr := Format(FWhereTemplate,['action','%s','name','%s']);
  end
  else
  begin
    LoadList(FRowList, 'Name', 'Sessions');
    LoadList(FGroupList, 'Action', 'Results');
    FGroupByFieldsStr := FActionFieldsTemplate;
    FGroupByWhereStr := FActionWhereTemplate;
    FWhereStr := Format(FWhereTemplate,['name','%s','action','%s']);
  end;
  UpdateSqlTemplates;
end;

procedure TChronoDataProvider.SetActiveGroup(const AValue: String);
begin
  if FActiveGroup=AValue then exit;
  FActiveGroup:=AValue;
end;

function TChronoDataProvider.GetActionList: TStrings;
begin
  if FGroupIndex = 0 then
    Result := FRowList
  else
    Result := FGroupList;
end;

function TChronoDataProvider.GetSessionList: TStrings;
begin
  if FGroupIndex = 0 then
    Result := FGroupList
  else
    Result := FRowList;
end;

procedure TChronoDataProvider.LoadList(List: TStrings; const Field, Table: String);
begin
  List.Clear;
  FDataset.QuickQuery('Select Distinct ' + Field + ' from ' + Table, List);
end;

procedure TChronoDataProvider.SetScaleIndex(const AValue: Integer);
begin
  if FScaleIndex = AValue then
    Exit;
  FScaleIndex := AValue;
  case FScaleIndex of
    0:FScaleStr := '1';
    1:FScaleStr := '1000';
    2:FScaleStr := '1000000';
  end;
  UpdateSqlTemplates;
end;

procedure TChronoDataProvider.UpdateSqlTemplates;
begin
  FMinStr := Format(FMinTemplate, [FScaleStr]) + FFromTables + FWhereStr;
  FMaxStr := Format(FMaxTemplate, [FScaleStr]) + FFromTables + FWhereStr;
  FAverageStr := Format(FAverageTemplate,[FScaleStr]) + FFromTables + FWhereStr;
  FMedianStr := Format(FMedianTemplate,[FScaleStr]) + FFromTables + FWhereStr;
  FCountStr := 'Select Count(*)' + FFromTables + FWhereStr;
  FGroupByStr := Format(FGroupByFieldsStr,[FScaleStr]) + FGroupByWhereStr;
  //todo: Update queries here
  {
  writeln('FMinStr: ',FMinStr);
  writeln('FMaxStr: ',FMaxStr);
  writeln('FAverageStr: ',FAverageStr);
  writeln('FGroupByStr: ',FGroupByStr);
  }
end;


constructor TChronoDataProvider.Create;
begin
  FGroupIndex := -1;
  FGroupList := TStringList.Create;
  FRowList := TStringList.Create;
end;

destructor TChronoDataProvider.Destroy;
begin
  inherited Destroy;
  FGroupList.Destroy;
  FRowList.Destroy;
end;

procedure TChronoDataProvider.RefreshDataset;
begin
  with FDataset do
  begin
    Sql := Format(FGroupByStr,[FActiveGroup]);
    Close;
    Open;
  end;
end;

function TChronoDataProvider.GetCount(RowIndex: Integer): String;
begin
  Result := FDataset.QuickQuery(Format(FCountStr, [FRowList[RowIndex], FActiveGroup]));
end;

function TChronoDataProvider.GetMin(RowIndex: Integer): String;
begin
  Result := FDataset.QuickQuery(Format(FMinStr, [FRowList[RowIndex], FActiveGroup]));
end;

function TChronoDataProvider.GetMax(RowIndex: Integer): String;
begin
  Result := FDataset.QuickQuery(Format(FMaxStr, [FRowList[RowIndex], FActiveGroup]));
end;

function TChronoDataProvider.GetMedian(RowIndex: Integer): String;
begin
  Result := FDataset.QuickQuery(Format(FMedianStr, [FRowList[RowIndex], FActiveGroup])+
   ' order by time Limit 1 Offset '+GetCount(RowIndex)+'/2');
end;

function TChronoDataProvider.GetAvg(RowIndex: Integer): String;
begin
  Result := FDataset.QuickQuery(Format(FAverageStr, [FRowList[RowIndex], FActiveGroup]));
end;

function TChronoDataProvider.GetAvg(const ARow, AGroup: String): String;
begin
  Result := FDataset.QuickQuery(Format(FAverageStr, [ARow, AGroup]));
end;



end.

