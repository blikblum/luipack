unit LuiConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TLuiConfigProvider }

  TLuiConfigProvider = class(TComponent)
  protected
    function ReadInteger(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Integer; virtual;
    function ReadString(const SectionTitle, ItemKey: String; out ValueExists: Boolean): String; virtual; abstract;
    function ReadBoolean(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Boolean; virtual;
    function ReadFloat(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Double; virtual;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer); virtual;
    procedure WriteString(const SectionTitle, ItemKey: String; AValue: String); virtual; abstract;
    procedure WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean); virtual;
    procedure WriteFloat(const SectionTitle, ItemKey: String; AValue: Double); virtual;
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
  end;

  TLuiConfigDataType = (ldtString, ldtInteger, ldtBoolean, ldtFloat);

  { TLuiConfigItem }

  TLuiConfigItem = class(TCollectionItem)
  private
    FDataType: TLuiConfigDataType;
    FDefaultValue: String;
    FDisplayText: String;
    FKey: String;
    procedure SetDataType(const AValue: TLuiConfigDataType);
    procedure SetDefaultValue(const AValue: String);
    procedure SetDisplayText(const AValue: String);
    procedure SetKey(const AValue: String);
  published
    property DataType: TLuiConfigDataType read FDataType write SetDataType;
    property DefaultValue: String read FDefaultValue write SetDefaultValue;
    property DisplayText: String read FDisplayText write SetDisplayText;
    property Key: String read FKey write SetKey;
  end;

  { TLuiConfigItems }

  TLuiConfigItems = class(TCollection)
  private
    FList: TFPHashList;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TLuiConfigItem;
    function Find(const Key: String): TLuiConfigItem;
    function GetDefaultString(const Key: String): String;
    function GetDefaultInteger(const Key: String): Integer;
    function GetDefaultBoolean(const Key: String): Boolean;
    function GetDefaultFloat(const Key: String): Double;
  end;

  { TLuiConfigSection }

  TLuiConfigSection = class(TCollectionItem)
  private
    FDisplayText: String;
    FTitle: String;
    procedure SetDisplayTitle(const AValue: String);
    procedure SetTitle(const AValue: String);
  public
    property DisplayText: String read FDisplayText write SetDisplayTitle;
    property Title: String read FTitle write SetTitle;
  end;

  { TLuiConfigSections }

  TLuiConfigSections = class(TCollection)
  private
    FHashList: TFPHashList;
    FValidHashList: Boolean;
    procedure BuildHashList;
  protected
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
  public
    destructor Destroy; override;
    function Add: TLuiConfigSection;
    function Find(const Title: String): TLuiConfigSection;
  end;

  TLuiConfigNotificationType = (lcnOpen, lcnClose);

  IConfigObserver = interface
    procedure ConfigNotification(NotificationType: TLuiConfigNotificationType;
      Data: PtrInt);
  end;

  { TLuiConfig }

  TLuiConfig = class(TComponent)
  private
    FActive: Boolean;
    FDataProvider: TLuiConfigProvider;
    FItemDefs: TLuiConfigItems;
    FObserverList: TFpList;
    FSectionDefs: TLuiConfigSections;
    procedure CheckObserverList;
    function HasObserver: Boolean;
    procedure InternalOpen;
    procedure InternalClose;
    procedure Notify(NotificationType: TLuiConfigNotificationType; Data: PtrInt);
    procedure SetActive(const AValue: Boolean);
    procedure SetDataProvider(const AValue: TLuiConfigProvider);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObserver(Observer: IConfigObserver);
    function GetItemText(const ItemKey: String): String;
    function GetSectionText(const SectionTitle: String): String;
    function ReadInteger(const SectionTitle, ItemKey: String): Integer;
    function ReadString(const SectionTitle, ItemKey: String): String;
    function ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
    function ReadFloat(const SectionTitle, ItemKey: String): Double;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure RemoveObserver(Observer: IConfigObserver);
    procedure WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer);
    procedure WriteString(const SectionTitle, ItemKey: String; AValue: String);
    procedure WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean);
    procedure WriteFloat(const SectionTitle, ItemKey: String; AValue: Double);
  published
    property Active: Boolean read FActive write SetActive;
    property DataProvider: TLuiConfigProvider read FDataProvider write SetDataProvider;
    property ItemDefs: TLuiConfigItems read FItemDefs;
    property SectionDefs: TLuiConfigSections read FSectionDefs;
  end;

//todo: use miscutils instead
function ReplacePathMacros(const Path: String): String;

implementation

uses
  StrUtils;

function ReplacePathMacros(const Path: String): String;
begin
  Result := AnsiReplaceText(Path, '$(APP_CONFIG_DIR)', GetAppConfigDir(False));
  Result := AnsiReplaceText(Result, '$(EXE_PATH)', ExtractFileDir(ParamStr(0)));
end;

{ TLuiConfig }

procedure TLuiConfig.SetDataProvider(const AValue: TLuiConfigProvider);
begin
  if FDataProvider = AValue then
    Exit;
  FDataProvider := AValue;
end;

procedure TLuiConfig.CheckObserverList;
begin
  if FObserverList = nil then
    FObserverList := TFPList.Create;
end;

function TLuiConfig.HasObserver: Boolean;
begin
  Result := (FObserverList <> nil) and (FObserverList.Count > 0);
end;

procedure TLuiConfig.InternalOpen;
begin
  if FDataProvider = nil then
    raise Exception.Create('DataProvider not set');
  FDataProvider.Open;
  Notify(lcnOpen, 0);
end;

procedure TLuiConfig.InternalClose;
begin
  if FDataProvider <> nil then
    FDataProvider.Close;
  Notify(lcnClose, 0);
end;

procedure TLuiConfig.Loaded;
begin
  inherited Loaded;
  if FActive then
    InternalOpen
  else
    InternalClose;
end;

procedure TLuiConfig.Notify(NotificationType: TLuiConfigNotificationType;
  Data: PtrInt);
var
  i: Integer;
begin
  if not HasObserver then
    Exit;
  for i := 0 to FObserverList.Count - 1 do
    IConfigObserver(FObserverList[i]).ConfigNotification(NotificationType, Data);
end;

procedure TLuiConfig.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if csLoading in ComponentState then
    Exit;
  if FActive then
    InternalOpen
  else
    InternalClose;
end;

constructor TLuiConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSectionDefs := TLuiConfigSections.Create(TLuiConfigSection);
  FItemDefs := TLuiConfigItems.Create(TLuiConfigItem);
end;

destructor TLuiConfig.Destroy;
begin
  FObserverList.Free;
  inherited Destroy;
end;

procedure TLuiConfig.AddObserver(Observer: IConfigObserver);
begin
  CheckObserverList;
  FObserverList.Add(Observer);
end;

function TLuiConfig.GetItemText(const ItemKey: String): String;
var
  Item: TLuiConfigItem;
begin
  Item := FItemDefs.Find(ItemKey);
  if Item <> nil then
    Result := Item.DisplayText
  else
    Result := ItemKey;
end;

function TLuiConfig.GetSectionText(const SectionTitle: String): String;
var
  Section: TLuiConfigSection;
begin
  Section := FSectionDefs.Find(SectionTitle);
  if Section <> nil then
    Result := Section.DisplayText
  else
    Result := SectionTitle;
end;

function TLuiConfig.ReadInteger(const SectionTitle, ItemKey: String): Integer;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadInteger(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultInteger(ItemKey);
end;

function TLuiConfig.ReadString(const SectionTitle, ItemKey: String): String;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadString(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultString(ItemKey);
end;

function TLuiConfig.ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadBoolean(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultBoolean(ItemKey);
end;

function TLuiConfig.ReadFloat(const SectionTitle, ItemKey: String): Double;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadFloat(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultFloat(ItemKey);
end;

procedure TLuiConfig.ReadSection(const SectionTitle: String; Strings: TStrings);
begin
  FDataProvider.ReadSection(SectionTitle, Strings);
end;

procedure TLuiConfig.ReadSections(Strings: TStrings);
begin
  FDataProvider.ReadSections(Strings);
end;

procedure TLuiConfig.RemoveObserver(Observer: IConfigObserver);
begin
  if not HasObserver then
    Exit;
  FObserverList.Remove(Observer);
end;

procedure TLuiConfig.WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer);
begin
  FDataProvider.WriteInteger(SectionTitle, ItemKey, AValue);
end;

procedure TLuiConfig.WriteString(const SectionTitle, ItemKey: String; AValue: String);
begin
  FDataProvider.WriteString(SectionTitle, ItemKey, AValue);
end;

procedure TLuiConfig.WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean);
begin
  FDataProvider.WriteBoolean(SectionTitle, ItemKey, AValue);
end;

procedure TLuiConfig.WriteFloat(const SectionTitle, ItemKey: String; AValue: Double);
begin
  FDataProvider.WriteFloat(SectionTitle, ItemKey, AValue);
end;

{ TLuiConfigItem }

procedure TLuiConfigItem.SetKey(const AValue: String);
begin
  if FKey=AValue then exit;
  FKey:=AValue;
end;

procedure TLuiConfigItem.SetDataType(const AValue: TLuiConfigDataType);
begin
  if FDataType=AValue then exit;
  FDataType:=AValue;
end;

procedure TLuiConfigItem.SetDefaultValue(const AValue: String);
begin
  if FDefaultValue=AValue then exit;
  FDefaultValue:=AValue;
end;

procedure TLuiConfigItem.SetDisplayText(const AValue: String);
begin
  if FDisplayText=AValue then exit;
  FDisplayText:=AValue;
end;

{ TLuiConfigSections }

procedure TLuiConfigSections.BuildHashList;
begin
  if FHashList = nil then
    FHashList := TFPHashList.Create
  else
    FHashList.Clear;
  //todo:
end;

procedure TLuiConfigSections.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  FValidHashList := False;
end;

destructor TLuiConfigSections.Destroy;
begin
  FHashList.Free;
  inherited Destroy;
end;

function TLuiConfigSections.Add: TLuiConfigSection;
begin
  Result := TLuiConfigSection(inherited Add);
end;

function TLuiConfigSections.Find(const Title: String): TLuiConfigSection;
begin
  if not FValidHashList then
    BuildHashList;
  Result := TLuiConfigSection(FHashList.Find(Title));
end;

{ TLuiConfigSection }

procedure TLuiConfigSection.SetDisplayTitle(const AValue: String);
begin
  if FDisplayText=AValue then exit;
  FDisplayText:=AValue;
end;

procedure TLuiConfigSection.SetTitle(const AValue: String);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

{ TLuiConfigItems }

constructor TLuiConfigItems.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FList := TFPHashList.Create;
end;

destructor TLuiConfigItems.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

function TLuiConfigItems.Add: TLuiConfigItem;
begin
  //protect the case the user try to create a item twice
  Result := TLuiConfigItem(inherited Add);
end;

function TLuiConfigItems.Find(const Key: String): TLuiConfigItem;
begin
  Result := TLuiConfigItem(FList.Find(Key));
end;

function TLuiConfigItems.GetDefaultString(const Key: String): String;
var
  Item: TLuiConfigItem;
begin
  Item := Find(Key);
  if Item <> nil then
    Result := Item.DefaultValue
  else
    Result := '';
end;

function TLuiConfigItems.GetDefaultInteger(const Key: String): Integer;
begin
  Result := StrToIntDef(GetDefaultString(Key), 0);
end;

function TLuiConfigItems.GetDefaultBoolean(const Key: String): Boolean;
begin
  Result := StrToBoolDef(GetDefaultString(Key), False);
end;

function TLuiConfigItems.GetDefaultFloat(const Key: String): Double;
begin
  Result := StrToFloatDef(GetDefaultString(Key), 0);
end;

{ TLuiConfigProvider }

function TLuiConfigProvider.ReadInteger(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Integer;
begin
  Result := StrToIntDef(ReadString(SectionTitle, ItemKey, ValueExists), 0);
end;

function TLuiConfigProvider.ReadBoolean(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadString(SectionTitle, ItemKey, ValueExists), False);
end;

function TLuiConfigProvider.ReadFloat(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Double;
begin
  Result := StrToFloatDef(ReadString(SectionTitle, ItemKey, ValueExists), 0);
end;

procedure TLuiConfigProvider.WriteInteger(const SectionTitle, ItemKey: String;
  AValue: Integer);
begin
  WriteString(SectionTitle, ItemKey, IntToStr(AValue));
end;

procedure TLuiConfigProvider.WriteBoolean(const SectionTitle, ItemKey: String;
  AValue: Boolean);
begin
  WriteString(SectionTitle, ItemKey, IfThen(AValue, '1', '0'));
end;

procedure TLuiConfigProvider.WriteFloat(const SectionTitle, ItemKey: String;
  AValue: Double);
begin
  WriteString(SectionTitle, ItemKey, FloatToStr(AValue));
end;

end.

