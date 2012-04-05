unit LuiJSONLazReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_DSet, fpjson, contnrs;

type

  { TfrJSONDataset }

  TfrJSONDataset = class(TfrDataset)
  private
    FData: TJSONData;
  protected
    procedure DoCheckEOF(var IsEof: Boolean); override;
  public
    procedure GetValue(const ParName: String; var ParValue: Variant);
    property Data: TJSONData read FData write FData;
  end;

  TfrJSONDatasetClass = class of TfrJSONDataset;

  { TfrJSONCrossDataset }

  TfrJSONCrossDataset = class(TfrJSONDataset)
  private
    FMasterDataset: TfrJSONDataset;
    procedure UpdateMasterData;
  protected
    procedure DoFirst; override;
    procedure DoNext; override;
  public
    property MasterDataset: TfrJSONDataset read FMasterDataset write FMasterDataset;
  end;

  { TfrJSONDataLink }

  TfrJSONDataLink = class
  private
    FBandName: String;
    FCrossBandName: String;
    FCrossDataset: TfrJSONDataset;
    FDataset: TfrJSONDataset;
    FPropertyName: String;
  public
    constructor Create(const ABandName, APropertyName, ACrossBandName: String);
    property BandName: String read FBandName;
    property CrossBandName: String read FCrossBandName;
    property CrossDataset: TfrJSONDataset read FCrossDataset write FCrossDataset;
    property Dataset: TfrJSONDataset read FDataset write FDataset;
    property PropertyName: String read FPropertyName;
  end;

  { TfrJSONReport }

  TfrJSONReport = class(TfrReport)
  private
    FConfigProperty: String;
    FDataProperty: String;
    FReportData: TJSONObject;
    FConfigData: TJSONObject;
    FData: TJSONObject;
    FJSONObject: TJSONObject;
    FDataLinks: TFPHashObjectList;
    FNullValues: TJSONObject;
    FOwnsConfigData: Boolean;
    FOwnsReportData: Boolean;
    function CreateJSONDataset(DatasetClass: TfrJSONDatasetClass; Index: Integer): TfrJSONDataset;
    procedure FreeOwnedData;
    procedure GetValue(const ParName: String; var ParValue: Variant);
    procedure BeginDoc;
    procedure ParseConfigData;
    procedure UserFunction(const AName: String; p1, p2, p3: Variant; var Val: Variant);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData(ReportData: TJSONObject; OwnsReportData: Boolean = False);
    procedure LoadData(ReportData, ConfigData: TJSONObject; OwnsReportData: Boolean = False;
      OwnsConfigData: Boolean = False);
    procedure RegisterCrossDataLink(const BandName, CrossBandName, PropertyName: String);
    procedure RegisterDataLink(const BandName, PropertyName: String);
    property ConfigProperty: String read FConfigProperty write FConfigProperty;
    property DataProperty: String read FDataProperty write FDataProperty;
    property JSONObject: TJSONObject read FJSONObject write FJSONObject;
    property NullValues: TJSONObject read FNullValues;
  end;

implementation

uses
  LuiJSONUtils, Variants;

{ TfrJSONCrossDataset }

procedure TfrJSONCrossDataset.UpdateMasterData;
begin
  Assert(FMasterDataset <> nil, 'MasterDataset is nil');
  if (Data.JSONType = jtArray) and (RecNo < Data.Count) then
    FMasterDataset.Data := Data.Items[RecNo];
end;

procedure TfrJSONCrossDataset.DoFirst;
begin
  inherited DoFirst;
  UpdateMasterData;
end;

procedure TfrJSONCrossDataset.DoNext;
begin
  inherited DoNext;
  UpdateMasterData;
end;

{ TfrJSONDataLink }

constructor TfrJSONDataLink.Create(const ABandName, APropertyName, ACrossBandName: String);
begin
  FBandName := ABandName;
  FPropertyName := APropertyName;
  FCrossBandName := ACrossBandName;
end;

{ TfrJSONDataset }

procedure TfrJSONDataset.DoCheckEOF(var IsEof: Boolean);
begin
  case FData.JSONType of
  jtArray:
    IsEof := RecNo >= FData.Count;
  jtObject:
    IsEof := RecNo = 1;
  else
    IsEof := True;
  end;
end;

procedure TfrJSONDataset.GetValue(const ParName: String; var ParValue: Variant);
var
  ArrayItem: TJSONData;
  PropData: TJSONData;
begin
  case FData.JSONType of
    jtArray:
      begin
        if RecNo < FData.Count then
        begin
          ArrayItem := FData.Items[RecNo];
          case ArrayItem.JSONType of
          jtObject:
            begin
              PropData := GetJSONProp(TJSONObject(ArrayItem), ParName);
              if PropData <> nil then
                ParValue := PropData.Value;
            end;
          jtNumber, jtString, jtBoolean:
            begin
              ParValue := ArrayItem.Value;
            end;
          end;
        end;
      end;
    jtObject:
      begin
        // todo: add option to handle not defined fields
        PropData := GetJSONProp(TJSONObject(FData), ParName);
        if PropData <> nil then
          ParValue := PropData.Value;
      end;
  end;
end;

{ TfrJSONReport }

function TfrJSONReport.CreateJSONDataset(DatasetClass: TfrJSONDatasetClass; Index: Integer): TfrJSONDataset;
begin
  Result := DatasetClass.Create(Self.Owner);
  Result.Name := Result.Owner.Name + DatasetClass.ClassName + IntToStr(Index);
  Result.FreeNotification(Self);
end;

procedure TfrJSONReport.FreeOwnedData;
begin
  if FOwnsReportData then
    FreeAndNil(FReportData);
  if FOwnsConfigData then
    FreeAndNil(FConfigData);
end;

procedure TfrJSONReport.GetValue(const ParName: String;
  var ParValue: Variant);
var
  PropData: TJSONData;
  DataLink: TfrJSONDataLink;
  PropertyName: String;
  DotPos: Integer;
begin
  DotPos := Pos('.', ParName);
  if DotPos = 0 then
  begin
    PropData := GetJSONProp(FJSONObject, ParName);
    if (PropData <> nil) and (PropData.JSONType <> jtNull) then
      ParValue := PropData.Value;
  end
  else
  begin
    PropertyName := Copy(ParName, 1, DotPos - 1);
    DataLink := TfrJSONDataLink(FDataLinks.Find(PropertyName));
    if DataLink <> nil then
    begin
      Assert(DataLink.Dataset <> nil, 'Dataset not created for property ' + PropertyName);
      PropertyName := Copy(ParName, DotPos + 1, Length(ParName));
      DataLink.Dataset.GetValue(PropertyName, ParValue);
    end;
  end;
  if VarIsEmpty(ParValue) then
  begin
    PropData := GetJSONProp(FNullValues, ParName);
    if PropData <> nil then
      ParValue := PropData.Value
    else
    begin
      //cheat to allow call of userfunction
      if Pos('IFNULL', ParName) <> 1 then
        ParValue := Null;
    end;
  end;
end;

procedure TfrJSONReport.BeginDoc;
var
  i: Integer;
  DataLink: TfrJSONDataLink;
  Data: TJSONData;
  Band: TfrBandView;
begin
  if FJSONObject = nil then
    raise Exception.Create('TJSONObjectReport.JSONObject = nil');
  for i := 0 to FDataLinks.Count - 1 do
  begin
    DataLink := TfrJSONDataLink(FDataLinks[i]);
    Data := FJSONObject.Elements[DataLink.PropertyName];
    if DataLink.Dataset = nil then
      DataLink.Dataset := CreateJSONDataset(TfrJSONDataset, i);
    Band := FindObject(DataLink.BandName) as TfrBandView;
    if Band = nil then
      raise Exception.CreateFmt('Band "%s" not found', [DataLink.BandName]);
    Band.DataSet := DataLink.Dataset.Name;
    DataLink.Dataset.Data := Data;
    if DataLink.CrossBandName <> '' then
    begin
      Band := FindObject(DataLink.CrossBandName) as TfrBandView;
      if Band = nil then
        raise Exception.CreateFmt('CrossBand "%s" not found', [DataLink.CrossBandName]);
      if Band.BandType <> btCrossData then
        raise Exception.CreateFmt('Band "%s" type different from CrossData', [DataLink.CrossBandName]);
      if DataLink.CrossDataset = nil then
        DataLink.CrossDataset := CreateJSONDataset(TfrJSONCrossDataset, i);
      TfrJSONCrossDataset(DataLink.CrossDataset).MasterDataset := DataLink.Dataset;
      Band.DataSet := DataLink.BandName + '=' + DataLink.CrossDataset.Name + ';';
      Band.DataSet := DataLink.CrossDataset.Name;
      DataLink.CrossDataset.Data := Data;
    end;
  end;
end;

procedure TfrJSONReport.ParseConfigData;
var
  DataLinksData: TJSONData;
  NullValuesData: TJSONData;
  i: Integer;
begin
  if FConfigData = nil then
  begin
    DataLinksData := nil;
    NullValuesData := nil;
  end
  else
  begin
    i := FConfigData.IndexOfName('datalinks');
    if i <> -1 then
    begin

    end
    else
      DataLinksData := nil;
    i := FConfigData.IndexOfName('nullvalues');
    if i <> -1 then
    begin

    end
    else
      NullValuesData := nil;
  end;
  if NullValuesData <> nil then
  begin

  end
  else
    FNullValues.Clear;
  if DataLinksData <> nil then
  begin

  end
  else
    FDataLinks.Clear;
end;

procedure TfrJSONReport.UserFunction(const AName: String; p1, p2, p3: Variant;
  var Val: Variant);
var
  V1: Variant;
  S2, S3: String;
begin
  if AName = 'IFNULL' then
  begin
    V1 := frParser.Calc(P1);
    if VarIsNull(V1) then
    begin
      S2 := P2;
      if S2 <> '' then
        Val := frParser.Calc(S2)
      else
        Val := Null;
    end
    else
    begin
      S3 := P3;
      if S3 = '' then
        Val := V1
      else
        Val := frParser.Calc(S3);
    end;
  end;
end;

procedure TfrJSONReport.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
  DataLink: TfrJSONDataLink;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TfrJSONDataset) then
  begin
    for i := 0 to FDataLinks.Count - 1 do
    begin
      DataLink := TfrJSONDataLink(FDataLinks[i]);
      if DataLink.Dataset = AComponent then
        DataLink.Dataset := nil
      else if DataLink.CrossDataset = AComponent then
        DataLink.CrossDataset := nil;
    end;
  end;
end;

constructor TfrJSONReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetValue := @GetValue;
  OnBeginDoc := @BeginDoc;
  OnUserFunction := @UserFunction;
  FDataLinks := TFPHashObjectList.Create(True);
  FNullValues := TJSONObject.Create;
  FConfigProperty := 'report.config';
end;

destructor TfrJSONReport.Destroy;
var
  i: Integer;
  DataLink: TfrJSONDataLink;
begin
  for i := 0 to FDataLinks.Count - 1 do
  begin
    DataLink := TfrJSONDataLink(FDataLinks[i]);
    DataLink.Dataset.Free;
    DataLink.CrossDataset.Free;
  end;
  FreeOwnedData;
  FDataLinks.Destroy;
  FNullValues.Destroy;
  inherited Destroy;
end;

procedure TfrJSONReport.LoadData(ReportData: TJSONObject;
  OwnsReportData: Boolean);
begin
  if ReportData = nil then
    raise Exception.Create('TfrJSONReport.LoadData ReportData = nil');
  FreeOwnedData;
  FReportData := ReportData;
  FOwnsReportData := OwnsReportData;
  if FDataProperty <> '' then
    FData := ReportData.Objects[FDataProperty]
  else
    FData := ReportData;
  if FConfigProperty <> '' then
    FConfigData := ReportData.Objects[FConfigProperty]
  else
    FConfigData := nil;
  FOwnsConfigData := False;
  ParseConfigData;
end;

procedure TfrJSONReport.LoadData(ReportData, ConfigData: TJSONObject;
  OwnsReportData, OwnsConfigData: Boolean);
begin
  if ReportData = nil then
    raise Exception.Create('TfrJSONReport.LoadData ReportData = nil');
  FreeOwnedData;
  FReportData := ReportData;
  FOwnsReportData := OwnsReportData;
  if FDataProperty <> '' then
    FData := ReportData.Objects[FDataProperty]
  else
    FData := ReportData;
  FConfigData := ConfigData;
  FOwnsConfigData := OwnsConfigData;
  ParseConfigData;
end;

procedure TfrJSONReport.RegisterCrossDataLink(const BandName, CrossBandName,
  PropertyName: String);
begin
  FDataLinks.Add(PropertyName, TfrJSONDataLink.Create(BandName, PropertyName, CrossBandName));
end;

procedure TfrJSONReport.RegisterDataLink(const BandName, PropertyName: String);
begin
  FDataLinks.Add(PropertyName, TfrJSONDataLink.Create(BandName, PropertyName, ''));
end;

end.

