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

  { TfrJSONDataSourceDef }

  TfrJSONDataSourceDef = class
  private
    FBandName: String;
    FDataset: TfrJSONDataset;
    FPropertyName: String;
  public
    constructor Create(const ABandName, APropertyName: String);
    property BandName: String read FBandName;
    property Dataset: TfrJSONDataset read FDataset write FDataset;
    property PropertyName: String read FPropertyName;
  end;

  { TfrJSONReport }

  TfrJSONReport = class(TfrReport)
  private
    FJSONObject: TJSONObject;
    FDataSourceDefs: TFPHashObjectList;
    FNullValues: TJSONObject;
    procedure GetValue(const ParName: String; var ParValue: Variant);
    procedure BeginDoc;
    procedure UserFunction(const AName: String; p1, p2, p3: Variant; var Val: Variant);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterDataSource(const BandName, PropertyName: String);
    property JSONObject: TJSONObject read FJSONObject write FJSONObject;
    property NullValues: TJSONObject read FNullValues;
  end;

implementation

uses
  LuiJSONUtils, Variants;

{ TfrJSONDataSourceDef }

constructor TfrJSONDataSourceDef.Create(const ABandName, APropertyName: String);
begin
  FBandName := ABandName;
  FPropertyName := APropertyName;
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
        ArrayItem := FData.Items[RecNo];
        if ArrayItem.JSONType = jtObject then
        begin
          PropData := GetJSONProp(TJSONObject(ArrayItem), ParName);
          if PropData <> nil then
            ParValue := PropData.Value;
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
      ParValue := PropData.Value
    else
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
    begin
      Def.Dataset := TfrJSONDataset.Create(Self.Owner);
      Def.Dataset.Name := Self.Owner.Name + 'JSONDataset' + IntToStr(i);
      Def.Dataset.FreeNotification(Self);
    end;
    Def.Dataset.Data := Data;
    Band := FindObject(Def.BandName) as TfrBandView;
    if Band = nil then
      raise Exception.CreateFmt('Band "%s" not found', [Def.BandName]);
    Band.DataSet := Def.Dataset.Name;
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
        Def.Dataset := nil;
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
begin
  for i := 0 to FDataSourceDefs.Count - 1 do
    TfrJSONDataSourceDef(FDataSourceDefs[i]).Dataset.Free;
  FDataSourceDefs.Destroy;
  FNullValues.Destroy;
  inherited Destroy;
end;

procedure TfrJSONReport.RegisterDataSource(const BandName, PropertyName: String);
begin
  FDataSourceDefs.Add(PropertyName, TfrJSONDataSourceDef.Create(BandName, PropertyName));
end;

end.

