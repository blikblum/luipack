unit JSONFormMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormMediator, Controls, fpjson;

type

  TJSONFormMediator = class;

  TJSONFormElement = class;

  { TJSONGUIMediator }

  TJSONGUIMediator = class
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); virtual;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); virtual;
    class procedure Initialize(Element: TJSONFormElement); virtual;
  end;

  TJSONGUIMediatorClass = class of TJSONGUIMediator;

  { TJSONCaptionMediator }

  TJSONCaptionMediator = class(TJSONGUIMediator)
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
  end;

  { TJSONSpinEditMediator }

  TJSONSpinEditMediator = class(TJSONGUIMediator)
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
  end;

  { TJSONListMediator }

  TJSONListMediator = class(TJSONGUIMediator)
    class function GetItemIndex(Data: TJSONObject; const PropName: String; Items: TStrings; OptionsData: TJSONObject): Integer; static;
    class procedure SetItemIndex(Data: TJSONObject; const PropName: String; Items: TStrings; ItemIndex: Integer; OptionsData: TJSONObject); static;
    class procedure LoadItems(Items: TStrings; OptionsData: TJSONObject); static;
  end;

  { TJSONComboBoxMediator }

  TJSONComboBoxMediator = class(TJSONListMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class procedure Initialize(Element: TJSONFormElement); override;
  end;

  { TJSONRadioGroupMediator }

  TJSONRadioGroupMediator = class(TJSONListMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class procedure Initialize(Element: TJSONFormElement); override;
  end;

  { TJSONCheckBoxMediator }

  TJSONCheckBoxMediator = class(TJSONGUIMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
  end;

  { TJSONFormElement }

  TJSONFormElement = class(TFormElement)
  private
    FMediatorClass: TJSONGUIMediatorClass;
    FOptions: String;
    FOptionsData: TJSONObject;
    FInitialized: Boolean;
    function GetOptionsData: TJSONObject;
    procedure Initialize;
    procedure LoadData(Data: TJSONObject);
    procedure MediatorClassNeeded;
    procedure OptionsDataNeeded;
    procedure SaveData(Data: TJSONObject);
  public
    procedure Assign(Source: TPersistent); override;
    property OptionsData: TJSONObject read GetOptionsData;
  published
    property Options: String read FOptions write FOptions;
  end;

  { TJSONFormElements }

  TJSONFormElements = class(TFormElements)
  private
    function GetItem(Index: Integer): TJSONFormElement;
  public
    constructor Create(AOwner: TJSONFormMediator);
    function Add: TJSONFormElement;
    property Items[Index: Integer]: TJSONFormElement read GetItem; default;
  end;

  { TJSONFormMediator }

  TJSONFormMediator = class(TCustomFormMediator)
  private
    FData: TJSONObject;
    FElements: TJSONFormElements;
    procedure SetData(Value: TJSONObject);
    procedure SetElements(Value: TJSONFormElements);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData;
    procedure SaveData;
    property Data: TJSONObject read FData write SetData;
  published
    property Elements: TJSONFormElements read FElements write SetElements;
  end;

implementation

uses
  contnrs, LuiJSONUtils, Spin, ExtCtrls, StdCtrls;

type

  TJSONDataMapType = (jdmText, jdmIndex, jdmValue);

  { TJSONGUIMediatorStore }

  TJSONGUIMediatorStore = class
  private
    FList: TFPHashList;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(const MediatorId: String): TJSONGUIMediatorClass;
    procedure RegisterMediator(const MediatorId: String; MediatorClass: TJSONGUIMediatorClass);
  end;

var
  MediatorStore: TJSONGUIMediatorStore;

procedure RegisterJSONMediator(const MediatorId: String;
  MediatorClass: TJSONGUIMediatorClass);
begin
  MediatorStore.RegisterMediator(MediatorId, MediatorClass);
end;

procedure RegisterJSONMediator(ControlClass: TControlClass;
  MediatorClass: TJSONGUIMediatorClass);
begin
  RegisterJSONMediator(ControlClass.ClassName, MediatorClass);
end;

{ TJSONCheckBoxMediator }

class procedure TJSONCheckBoxMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  CheckBox: TCheckBox;
begin
  //todo: add checked/unchecked options
  CheckBox := Element.Control as TCheckBox;
  CheckBox.Checked := Data.Get(Element.PropertyName, False);
end;

class procedure TJSONCheckBoxMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  CheckBox: TCheckBox;
  PropName: String;
begin
  CheckBox := Element.Control as TCheckBox;
  PropName := Element.PropertyName;
  if CheckBox.Checked then
    Data.Booleans[PropName] := True
  else
    Data.Delete(PropName);
end;

{ TJSONRadioGroupMediator }

class procedure TJSONRadioGroupMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  RadioGroup: TRadioGroup;
begin
  RadioGroup := Element.Control as TRadioGroup;
  RadioGroup.ItemIndex := GetItemIndex(Data, Element.PropertyName,
    RadioGroup.Items, Element.OptionsData);
end;

class procedure TJSONRadioGroupMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  RadioGroup: TRadioGroup;
begin
  RadioGroup := Element.Control as TRadioGroup;
  SetItemIndex(Data, Element.PropertyName, RadioGroup.Items,
    RadioGroup.ItemIndex, Element.OptionsData);
end;

class procedure TJSONRadioGroupMediator.Initialize(Element: TJSONFormElement);
var
  RadioGroup: TRadioGroup;
begin
  RadioGroup := Element.Control as TRadioGroup;
  LoadItems(RadioGroup.Items, Element.OptionsData);
end;

{ TJSONComboBoxMediator }

class procedure TJSONComboBoxMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  ComboBox: TComboBox;
  PropName: String;
  OptionsData: TJSONObject;
begin
  ComboBox := Element.Control as TComboBox;
  PropName := Element.PropertyName;
  OptionsData := Element.OptionsData;
  ComboBox.ItemIndex := GetItemIndex(Data, PropName, ComboBox.Items, OptionsData);
  if (ComboBox.ItemIndex = -1) and (ComboBox.Style = csDropDown) then
  begin
    if OptionsData.Get('datamap', 'text') = 'text' then
      ComboBox.Text := Data.Get(PropName, '')
    else
      ComboBox.Text := '';
  end;
end;

class procedure TJSONComboBoxMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  ComboBox: TComboBox;
  OptionsData: TJSONObject;
  PropName: String;
begin
  ComboBox := Element.Control as TComboBox;
  PropName := Element.PropertyName;
  OptionsData := Element.OptionsData;
  if (ComboBox.ItemIndex = -1) and (ComboBox.Style = csDropDown) and (OptionsData.Get('datamap', 'text') = 'text') then
    Data.Strings[PropName] := ComboBox.Text
  else
    SetItemIndex(Data, PropName, ComboBox.Items, ComboBox.ItemIndex, OptionsData);
end;

class procedure TJSONComboBoxMediator.Initialize(Element: TJSONFormElement);
var
  ComboBox: TComboBox;
begin
  ComboBox := Element.Control as TComboBox;
  LoadItems(ComboBox.Items, Element.OptionsData);
end;

{ TJSONListMediator }

class function TJSONListMediator.GetItemIndex(Data: TJSONObject;
  const PropName: String; Items: TStrings; OptionsData: TJSONObject): Integer;
var
  PropData, MapData: TJSONData;
  SourceData: TJSONArray;
  MapType: TJSONDataMapType;
  ValuePropName: String;
begin
  Result := -1;
  PropData := Data.Find(PropName);
  if (PropData <> nil) and not (PropData.JSONType in [jtNull, jtArray, jtObject]) then
  begin
    MapType := jdmText;
    ValuePropName := 'value';
    MapData := OptionsData.Find('datamap');
    if MapData <> nil then
    begin
      case MapData.JSONType of
        jtString:
          if MapData.AsString = 'index' then
            MapType := jdmIndex
          else if MapData.AsString = 'value' then
            MapType := jdmValue;
        jtObject:
          begin
            MapType := jdmValue;
            ValuePropName := TJSONObject(MapData).Get('valueprop', 'value');
          end;
      end;
    end;
    case MapType of
      jdmText:
        Result := Items.IndexOf(PropData.AsString);
      jdmIndex:
        begin
          if PropData.JSONType = jtNumber then
            Result := PropData.AsInteger;
        end;
      jdmValue:
        begin
          if FindJSONProp(OptionsData, 'datasource', SourceData) then
            Result := GetJSONIndexOf(SourceData, [ValuePropName, PropData.Value]);
        end;
    end;
    //check for out of range result
    if (Result < 0) and (Result >= Items.Count) then
      Result := -1;
  end;
end;


class procedure TJSONListMediator.SetItemIndex(Data: TJSONObject;
  const PropName: String; Items: TStrings; ItemIndex: Integer;
  OptionsData: TJSONObject);
var
  MapType: TJSONDataMapType;
  MapData, ValueData: TJSONData;
  SourceData: TJSONArray;
  ValuePropName: String;
  ItemData: TJSONData;
begin
  if ItemIndex > -1 then
  begin
    MapType := jdmText;
    ValuePropName := 'value';
    MapData := OptionsData.Find('datamap');
    if MapData <> nil then
    begin
      case MapData.JSONType of
        jtString:
          if MapData.AsString = 'index' then
            MapType := jdmIndex
          else if MapData.AsString = 'value' then
            MapType := jdmValue;
        jtObject:
          begin
            ValuePropName := TJSONObject(MapData).Get('valueprop', 'value');
            MapType := jdmValue;
          end;
      end;
    end;
    case MapType of
      jdmText:
        Data.Strings[PropName] := Items[ItemIndex];
      jdmIndex:
        Data.Integers[PropName] := ItemIndex;
      jdmValue:
        begin
          if FindJSONProp(OptionsData, 'datasource', SourceData) then
          begin
            if ItemIndex < SourceData.Count then
            begin
              ItemData := SourceData.Items[ItemIndex];
              if ItemData.JSONType = jtObject then
              begin
                ValueData := TJSONObject(ItemData).Find(ValuePropName);
                if ValueData <> nil then
                  Data.Elements[PropName] := ValueData.Clone;
              end;
            end;
          end;
        end;
    end;
  end
  else
    Data.Delete(PropName);
end;


class procedure TJSONListMediator.LoadItems(Items: TStrings;
  OptionsData: TJSONObject);
var
  SourceData: TJSONArray;
  MapData, ItemData: TJSONData;
  TextProp: String;
  i: Integer;
begin
  if FindJSONProp(OptionsData, 'datasource', SourceData) then
  begin
    MapData := OptionsData.Find('datamap');
    if (MapData <> nil) and (MapData.JSONType = jtObject) then
      TextProp := TJSONObject(MapData).Get('textprop', 'text')
    else
      TextProp := 'text';
    Items.Clear;
    for i := 0 to SourceData.Count - 1 do
    begin
      ItemData := SourceData.Items[i];
      case ItemData.JSONType of
        jtString:
          Items.Add(ItemData.AsString);
        jtObject:
          Items.Add(TJSONObject(ItemData).Get(TextProp, ''));
      end;
    end;
  end;
end;

{ TJSONSpinEditMediator }

class procedure TJSONSpinEditMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  SpinEdit: TCustomFloatSpinEdit;
  PropData: TJSONData;
begin
  SpinEdit := Element.Control as TCustomFloatSpinEdit;
  PropData := Data.Find(Element.PropertyName);
  if (PropData = nil) or (PropData.JSONType = jtNull) then
    SpinEdit.ValueEmpty := True
  else
  begin
    SpinEdit.Value := PropData.AsFloat;
    SpinEdit.ValueEmpty := False;
  end;
end;

class procedure TJSONSpinEditMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  SpinEdit: TCustomFloatSpinEdit;
  PropName: String;
begin
  SpinEdit := Element.Control as TCustomFloatSpinEdit;
  PropName := Element.PropertyName;
  if not SpinEdit.ValueEmpty then
  begin
    if SpinEdit.DecimalPlaces = 0 then
      Data.Integers[PropName] := round(SpinEdit.Value)
    else
      Data.Floats[PropName] := SpinEdit.Value;
  end
  else
  begin
    //todo add option to configure undefined/null
    Data.Delete(PropName);
    //Data.Nulls[PropName] := True;
  end;
end;

{ TJSONCaptionMediator }

class procedure TJSONCaptionMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  FormatStr, TemplateStr, ValueStr: String;
  PropData: TJSONData;
  OptionsData: TJSONObject;
begin
  PropData := Data.Find(Element.PropertyName);
  OptionsData := Element.OptionsData;
  if PropData <> nil then
  begin
    //todo: check propdata type
    ValueStr := PropData.AsString;
    FormatStr := OptionsData.Get('format', '');
    if FormatStr = 'date' then
      ValueStr := DateToStr(PropData.AsFloat)
    else if FormatStr = 'datetime' then
      ValueStr := DateTimeToStr(PropData.AsFloat);
    TemplateStr := OptionsData.Get('template', '%s');
    Element.Control.Caption := Format(TemplateStr, [ValueStr]);
  end
  else
    Element.Control.Caption := '';
end;

class procedure TJSONCaptionMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
begin
  //
end;

constructor TJSONGUIMediatorStore.Create;
begin
  FList := TFPHashList.Create;
end;

destructor TJSONGUIMediatorStore.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

function TJSONGUIMediatorStore.Find(const MediatorId: String): TJSONGUIMediatorClass;
begin
  REsult := TJSONGUIMediatorClass(FList.Find(MediatorId));
end;

procedure TJSONGUIMediatorStore.RegisterMediator(const MediatorId: String;
  MediatorClass: TJSONGUIMediatorClass);
begin
  FList.Add(MediatorId, MediatorClass);
end;

{ TJSONGUIMediator }

type
  TControlAccess = class(TControl)

  end;

class procedure TJSONGUIMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
begin
  TControlAccess(Element.Control).Text := Data.Get(Element.PropertyName, '');
end;

class procedure TJSONGUIMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  i: Integer;
  ControlText: String;
  PropName: String;
begin
  PropName := Element.PropertyName;
  i := Data.IndexOfName(PropName);
  ControlText := TControlAccess(Element.Control).Text;
  if (i <> -1) or (ControlText <> '') then
    Data.Strings[PropName] := ControlText;
end;

class procedure TJSONGUIMediator.Initialize(Element: TJSONFormElement);
begin
  //
end;

{ TJSONFormMediator }

procedure TJSONFormMediator.SetElements(Value: TJSONFormElements);
begin
  FElements.Assign(Value);
end;

procedure TJSONFormMediator.SetData(Value: TJSONObject);
begin
  if FData = Value then Exit;
  FData := Value;
end;

constructor TJSONFormMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FElements := TJSONFormElements.Create(Self);
end;

destructor TJSONFormMediator.Destroy;
begin
  FElements.Destroy;
  inherited Destroy;
end;

procedure TJSONFormMediator.LoadData;
var
  i: Integer;
begin
  //todo: check data
  BeginLoad;
  try
    for i := 0 to FElements.Count - 1 do
      TJSONFormElement(FElements[i]).LoadData(FData);
  finally
    EndLoad;
  end;
end;

procedure TJSONFormMediator.SaveData;
var
  i: Integer;
begin
  //todo: check Data
  for i := 0 to FElements.Count - 1 do
    TJSONFormElement(FElements[i]).SaveData(FData);
end;

{ TJSONFormElements }

function TJSONFormElements.GetItem(Index: Integer): TJSONFormElement;
begin
  Result := TJSONFormElement(inherited Items[Index]);
end;

constructor TJSONFormElements.Create(AOwner: TJSONFormMediator);
begin
  inherited Create(AOwner, TJSONFormElement);
end;

function TJSONFormElements.Add: TJSONFormElement;
begin
  Result := TJSONFormElement(inherited Add);
end;

{ TJSONFormElement }

function TJSONFormElement.GetOptionsData: TJSONObject;
begin
  OptionsDataNeeded;
  Result := FOptionsData;
end;

procedure TJSONFormElement.Initialize;
begin
  if not FInitialized then
  begin
    OptionsDataNeeded;
    MediatorClassNeeded;
    if Control <> nil then
      FMediatorClass.Initialize(Self);
    FInitialized := True;
  end;
end;

procedure TJSONFormElement.MediatorClassNeeded;
begin
  if FMediatorClass = nil then
  begin
    if MediatorId <> '' then
      FMediatorClass := MediatorStore.Find(MediatorId)
    else
      FMediatorClass := MediatorStore.Find(Control.ClassName);
    if FMediatorClass = nil then
      raise Exception.CreateFmt('Could not find mediator (MediatorId: "%s" ControlClass: "%s")', [MediatorId, Control.ClassName]);
  end;
end;

procedure TJSONFormElement.OptionsDataNeeded;
begin
  if FOptionsData = nil then
  begin
    if not TryStrToJSON(FOptions, FOptionsData) then
      FOptionsData := TJSONObject.Create;
  end;
end;

procedure TJSONFormElement.LoadData(Data: TJSONObject);
begin
  if Control <> nil then
  begin
    Initialize;
    FMediatorClass.DoJSONToGUI(Data, Self);
  end;
end;

procedure TJSONFormElement.SaveData(Data: TJSONObject);
begin
  if Control <> nil then
  begin
    Initialize;
    FMediatorClass.DoGUIToJSON(Self, Data);
  end;
end;

procedure TJSONFormElement.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJSONFormElement then
  begin
    Options := TJSONFormElement(Source).Options;
  end;
end;

initialization
  MediatorStore := TJSONGUIMediatorStore.Create;
  RegisterJSONMediator(TEdit, TJSONGUIMediator);
  RegisterJSONMediator(TMemo, TJSONGUIMediator);
  RegisterJSONMediator(TComboBox, TJSONComboBoxMediator);
  RegisterJSONMediator(TLabel, TJSONCaptionMediator);
  RegisterJSONMediator(TSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TFloatSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TRadioGroup, TJSONRadioGroupMediator);
  RegisterJSONMediator(TCheckBox, TJSONCheckBoxMediator);

finalization
  MediatorStore.Destroy;

end.

