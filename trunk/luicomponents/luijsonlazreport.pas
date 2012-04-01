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

  { TfrJSONDataSourceDef }

  TfrJSONDataSourceDef = class
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
    FJSONObject: TJSONObject;
    FDataSourceDefs: TFPHashObjectList;
    FNullValues: TJSONObject;
    function CreateJSONDataset(DatasetClass: TfrJSONDatasetClass; Index: Integer): TfrJSONDataset;
    procedure GetValue(const ParName: String; var ParValue: Variant);
    procedure BeginDoc;
    procedure UserFunction(const AName: String; p1, p2, p3: Variant; var Val: Variant);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterCrossDataSource(const BandName, CrossBandName, PropertyName: String);
    procedure RegisterDataSource(const BandName, PropertyName: String);
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

{ TfrJSONDataSourceDef }

constructor TfrJSONDataSourceDef.Create(const ABandName, APropertyName, ACrossBandName: String);
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

procedure TfrJSONReport.GetValue(const ParName: String;
  var ParValue: Variant);
var
  PropData: TJSONData;
  DataSourceDef: TfrJSONDataSourceDef;
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
    DataSourceDef := TfrJSONDataSourceDef(FDataSourceDefs.Find(PropertyName));
    if DataSourceDef <> nil then
    begin
      Assert(DataSourceDef.Dataset <> nil, 'Dataset not created for property ' + PropertyName);
      PropertyName := Copy(ParName, DotPos + 1, Length(ParName));
      DataSourceDef.Dataset.GetValue(PropertyName, ParValue);
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
  Def: TfrJSONDataSourceDef;
  Data: TJSONData;
  Band: TfrBandView;
begin
  if FJSONObject = nil then
    raise Exception.Create('TJSONObjectReport.JSONObject = nil');
  for i := 0 to FDataSourceDefs.Count - 1 do
  begin
    Def := TfrJSONDataSourceDef(FDataSourceDefs[i]);
    Data := FJSONObject.Elements[Def.PropertyName];
    if Def.Dataset = nil then
      Def.Dataset := CreateJSONDataset(TfrJSONDataset, i);
    Band := FindObject(Def.BandName) as TfrBandView;
    if Band = nil then
      raise Exception.CreateFmt('Band "%s" not found', [Def.BandName]);
    Band.DataSet := Def.Dataset.Name;
    Def.Dataset.Data := Data;
    if Def.CrossBandName <> '' then
    begin
      Band := FindObject(Def.CrossBandName) as TfrBandView;
      if Band = nil then
        raise Exception.CreateFmt('CrossBand "%s" not found', [Def.CrossBandName]);
      if Band.BandType <> btCrossData then
        raise Exception.CreateFmt('Band "%s" type different from CrossData', [Def.CrossBandName]);
      if Def.CrossDataset = nil then
        Def.CrossDataset := CreateJSONDataset(TfrJSONCrossDataset, i);
      TfrJSONCrossDataset(Def.CrossDataset).MasterDataset := Def.Dataset;
      Band.DataSet := Def.BandName + '=' + Def.CrossDataset.Name + ';';
      Band.DataSet := Def.CrossDataset.Name;
      Def.CrossDataset.Data := Data;
    end;
  end;
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
  Def: TfrJSONDataSourceDef;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TfrJSONDataset) then
  begin
    for i := 0 to FDataSourceDefs.Count - 1 do
    begin
      Def := TfrJSONDataSourceDef(FDataSourceDefs[i]);
      if Def.Dataset = AComponent then
        Def.Dataset := nil
      else if Def.CrossDataset = AComponent then
        Def.CrossDataset := nil;
    end;
  end;
end;

constructor TfrJSONReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetValue := @GetValue;
  OnBeginDoc := @BeginDoc;
  OnUserFunction := @UserFunction;
  FDataSourceDefs := TFPHashObjectList.Create(True);
  FNullValues := TJSONObject.Create;
end;

destructor TfrJSONReport.Destroy;
var
  i: Integer;
  Def: TfrJSONDataSourceDef;
begin
  for i := 0 to FDataSourceDefs.Count - 1 do
  begin
    Def := TfrJSONDataSourceDef(FDataSourceDefs[i]);
    Def.Dataset.Free;
    Def.CrossDataset.Free;
  end;
  FDataSourceDefs.Destroy;
  FNullValues.Destroy;
  inherited Destroy;
end;

procedure TfrJSONReport.RegisterCrossDataSource(const BandName, CrossBandName,
  PropertyName: String);
begin
  FDataSourceDefs.Add(PropertyName, TfrJSONDataSourceDef.Create(BandName, PropertyName, CrossBandName));
end;

procedure TfrJSONReport.RegisterDataSource(const BandName, PropertyName: String);
begin
  FDataSourceDefs.Add(PropertyName, TfrJSONDataSourceDef.Create(BandName, PropertyName, ''));
end;

end.

