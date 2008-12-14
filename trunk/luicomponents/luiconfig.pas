unit LuiConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TLuiConfigProvider }

  TLuiConfigProvider = class(TComponent)
  protected
    function ReadInteger(const Section, Key: String; Default: Integer): Integer; virtual;
    function ReadString(const Section, Key, Default: String): String; virtual; abstract;
    function ReadBoolean(const Section, Key: String; Default: Boolean): Boolean; virtual;
    function ReadFloat(const Section, Key: String; Default: Double): Double; virtual;
    procedure ReadSection(const Section: String; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure WriteInteger(const Section, Key: String; AValue: Integer); virtual;
    procedure WriteString(const Section, Key: String; AValue: String); virtual; abstract;
    procedure WriteBoolean(const Section, Key: String; AValue: Boolean); virtual;
    procedure WriteFloat(const Section, Key: String; AValue: Double); virtual;
  end;

  TLuiConfigDataType = (ldtString, ldtInteger, ldtBoolean, ldtFloat);

  { TLuiConfigItem }

  TLuiConfigItem = class(TCollectionItem)
  private
    FDataType: TLuiConfigDataType;
    FDefaultValue: String;
    FDisplayText: String;
    FKey: ShortString;
    procedure SetDataType(const AValue: TLuiConfigDataType);
    procedure SetDefaultValue(const AValue: String);
    procedure SetDisplayText(const AValue: String);
    procedure SetKey(const AValue: ShortString);
  published
    property DataType: TLuiConfigDataType read FDataType write SetDataType;
    property DefaultValue: String read FDefaultValue write SetDefaultValue;
    property DisplayText: String read FDisplayText write SetDisplayText;
    property Key: ShortString read FKey write SetKey;
  end;

  { TLuiConfigItems }

  TLuiConfigItems = class(TCollection)
  private
    FList: TFPHashList;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TLuiConfigItem;
    function Find(const Key: ShortString): TLuiConfigItem;
    function GetDefaultString(const Key: ShortString): String;
    function GetDefaultInteger(const Key: ShortString): Integer;
    function GetDefaultBoolean(const Key: ShortString): Boolean;
    function GetDefaultFloat(const Key: ShortString): Double;
  end;

  { TLuiConfigSection }

  TLuiConfigSection = class(TCollectionItem)
  private
    FDisplayText: String;
    FTitle: ShortString;
    procedure SetDisplayTitle(const AValue: String);
    procedure SetTitle(const AValue: ShortString);
  public
    property DisplayText: String read FDisplayText write SetDisplayTitle;
    property Title: ShortString read FTitle write SetTitle;
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
    function Find(const Title: ShortString): TLuiConfigSection;
  end;

  { TLuiConfig }

  TLuiConfig = class(TComponent)
  private
    FActive: Boolean;
    FDataProvider: TLuiConfigProvider;
    FItemDefs: TLuiConfigItems;
    FSectionDefs: TLuiConfigSections;
    procedure SetActive(const AValue: Boolean);
    procedure SetDataProvider(const AValue: TLuiConfigProvider);
  public
    constructor Create(AOwner: TComponent); override;
    function GetItemText(const ItemKey: String): String;
    function GetSectionText(const SectionTitle: String): String;
    function ReadInteger(const SectionTitle, ItemKey: String): Integer;
    function ReadString(const SectionTitle, ItemKey: String): String;
    function ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
    function ReadFloat(const SectionTitle, ItemKey: String): Double;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
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

implementation

uses
  StrUtils;

{ TLuiConfig }

procedure TLuiConfig.SetDataProvider(const AValue: TLuiConfigProvider);
begin
  if FDataProvider = AValue then
    Exit;
  FDataProvider := AValue;
end;

procedure TLuiConfig.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
end;

constructor TLuiConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSectionDefs := TLuiConfigSections.Create(TLuiConfigSection);
  FItemDefs := TLuiConfigItems.Create(TLuiConfigItem);
end;

function TLuiConfig.GetItemText(const ItemKey: String): String;
var
  Item: TLuiConfigItem;
begin
  Item := FItemDefs.Find(ItemKey);
  if Item <> nil then
    Result := Item.DisplayName
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
begin
  Result := FDataProvider.ReadInteger(SectionTitle, ItemKey,
    FItemDefs.GetDefaultInteger(ItemKey));
end;

function TLuiConfig.ReadString(const SectionTitle, ItemKey: String): String;
begin
  Result := FDataProvider.ReadString(SectionTitle, ItemKey,
    FItemDefs.GetDefaultString(ItemKey));
end;

function TLuiConfig.ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
begin
  Result := FDataProvider.ReadBoolean(SectionTitle, ItemKey,
    FItemDefs.GetDefaultBoolean(ItemKey));
end;

function TLuiConfig.ReadFloat(const SectionTitle, ItemKey: String): Double;
begin
  Result := FDataProvider.ReadFloat(SectionTitle, ItemKey,
    FItemDefs.GetDefaultFloat(ItemKey));
end;

procedure TLuiConfig.ReadSection(const SectionTitle: String; Strings: TStrings);
begin

end;

procedure TLuiConfig.ReadSections(Strings: TStrings);
begin

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

procedure TLuiConfigItem.SetKey(const AValue: ShortString);
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

function TLuiConfigSections.Find(const Title: ShortString): TLuiConfigSection;
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

procedure TLuiConfigSection.SetTitle(const AValue: ShortString);
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

function TLuiConfigItems.Find(const Key: ShortString): TLuiConfigItem;
begin
  Result := TLuiConfigItem(FList.Find(Key));
end;

function TLuiConfigItems.GetDefaultString(const Key: ShortString): String;
var
  Item: TLuiConfigItem;
begin
  Item := Find(Key);
  if Item <> nil then
    Result := Item.DefaultValue
  else
    Result := '';
end;

function TLuiConfigItems.GetDefaultInteger(const Key: ShortString): Integer;
begin
  Result := StrToIntDef(GetDefaultString(Key), 0);
end;

function TLuiConfigItems.GetDefaultBoolean(const Key: ShortString): Boolean;
begin
  Result := StrToBoolDef(GetDefaultString(Key), False);
end;

function TLuiConfigItems.GetDefaultFloat(const Key: ShortString): Double;
begin
  Result := StrToFloatDef(GetDefaultString(Key), 0);
end;

{ TLuiConfigProvider }

function TLuiConfigProvider.ReadInteger(const Section, Key: String; Default: Integer): Integer;
begin
  Result := StrToIntDef(ReadString(Section, Key, ''), Default);
end;

function TLuiConfigProvider.ReadBoolean(const Section, Key: String; Default: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadString(Section, Key, ''), Default);
end;

function TLuiConfigProvider.ReadFloat(const Section, Key: String; Default: Double): Double;
begin
  Result := StrToFloatDef(ReadString(Section, Key, ''), Default);
end;

procedure TLuiConfigProvider.WriteInteger(const Section, Key: String;
  AValue: Integer);
begin
  WriteString(Section, Key, IntToStr(AValue));
end;

procedure TLuiConfigProvider.WriteBoolean(const Section, Key: String;
  AValue: Boolean);
begin
  WriteString(Section, Key, IfThen(AValue, '1', '0'));
end;

procedure TLuiConfigProvider.WriteFloat(const Section, Key: String;
  AValue: Double);
begin
  WriteString(Section, Key, FloatToStr(AValue));
end;

end.

