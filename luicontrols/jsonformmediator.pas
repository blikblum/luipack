unit JSONFormMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormMediator, Controls, fpjson, LuiJSONUtils;

type

  TJSONFormMediator = class;

  TJSONFormElement = class;

  TJSONStringProperty = type TJSONStringType;

  { TJSONGUIMediator }

  TJSONGUIMediator = class
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); virtual;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); virtual;
    class procedure Initialize(Element: TJSONFormElement); virtual;
    class function AllowsCaptionLabel: Boolean; virtual;
  end;

  TJSONGUIMediatorClass = class of TJSONGUIMediator;

  { TJSONCaptionMediator }

  TJSONCaptionMediator = class(TJSONGUIMediator)
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class function AllowsCaptionLabel: Boolean; override;
  end;

  { TJSONEditMediator }

  TJSONEditMediator = class(TJSONGUIMediator)
  public
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
    class function AllowsCaptionLabel: Boolean; override;
  end;

  { TJSONRadioButtonMediator }

  TJSONRadioButtonMediator = class(TJSONGUIMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class function AllowsCaptionLabel: Boolean; override;
  end;

  { TJSONCheckGroupMediator }

  TJSONCheckGroupMediator = class(TJSONListMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class procedure Initialize(Element: TJSONFormElement); override;
  end;

  { TJSONCheckBoxMediator }

  TJSONCheckBoxMediator = class(TJSONGUIMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
    class function AllowsCaptionLabel: Boolean; override;
  end;

  { TJSONMemoMediator }

  TJSONMemoMediator = class(TJSONGUIMediator)
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
  end;

  { TJSONFormElement }

  TJSONFormElement = class(TFormElement)
  private
    FMediatorClass: TJSONGUIMediatorClass;
    FOptions: TJSONObjectString;
    FOptionsData: TJSONObject;
    FInitialized: Boolean;
    function GetOptionsData: TJSONObject;
    procedure Initialize;
    procedure LoadData(Data: TJSONObject);
    procedure MediatorClassNeeded;
    procedure OptionsDataNeeded;
    procedure SaveData(Data: TJSONObject);
  protected
    function AllowsCaptionLabel: Boolean; override;
    function FormMediator: TJSONFormMediator;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset(UpdateData: Boolean);
    property OptionsData: TJSONObject read GetOptionsData;
  published
    property Options: TJSONObjectString read FOptions write FOptions;
  end;

  { TJSONFormElements }

  TJSONFormElements = class(TFormElements)
  private
    function GetItem(Index: Integer): TJSONFormElement;
  public
    constructor Create(AOwner: TJSONFormMediator);
    function Add: TJSONFormElement;
    function ElementByName(const ElementName: String): TJSONFormElement;
    function FindElement(const Name: String; ByPropertyName: Boolean = False): TJSONFormElement;
    property Items[Index: Integer]: TJSONFormElement read GetItem; default;
  end;

  { TJSONFormMediator }

  TJSONFormMediator = class(TCustomFormMediator)
  private
    FData: TJSONObject;
    FElements: TJSONFormElements;
    procedure SetData(Value: TJSONObject);
    procedure SetElements(Value: TJSONFormElements);
  protected
    function GetElements: TFormElements; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ElementByName(const ElementName: String): TJSONFormElement;
    procedure LoadData;
    procedure LoadData(const PropertySet: String);
    procedure SaveData;
    procedure SaveData(const PropertySet: String);
    property Data: TJSONObject read FData write SetData;
  published
    property Elements: TJSONFormElements read FElements write SetElements;
  end;

  procedure RegisterJSONMediator(const MediatorId: String; MediatorClass: TJSONGUIMediatorClass);

  procedure RegisterJSONMediator(ControlClass: TControlClass; MediatorClass: TJSONGUIMediatorClass);

implementation

uses
  contnrs, Spin, ExtCtrls, StdCtrls, LuiJSONHelpers;

type

  TJSONListIndexType = (jliText, jliIndex, jliValue);

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

{ TJSONEditMediator }

class procedure TJSONEditMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  Edit: TCustomEdit;
  Int64Value: Int64;
  DoubleValue: Double;
  PropName, EditText: String;
begin
  Edit := Element.Control as TCustomEdit;
  PropName := Element.PropertyName;
  EditText := Edit.Text;
  if Edit.NumbersOnly and not (Element.OptionsData.Get('type', '') = 'string') then
  begin
    //try to convert to number
    if TryStrToInt64(EditText, Int64Value) then
      SetJSONPath(Data, PropName, TJSONInt64Number.Create(Int64Value))
    else if TryStrToFloat(PropName, DoubleValue) then
      SetJSONPath(Data, PropName, TJSONFloatNumber.Create(DoubleValue))
    else
      //todo: implement default handling for empty
      SetJSONPath(Data, PropName, TJSONNull.Create);
  end
  else
  begin
    SetJSONPath(Data, PropName, TJSONString.Create(EditText));
  end;
end;

{ TJSONCheckGroupMediator }

class procedure TJSONCheckGroupMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  CheckGroup: TCheckGroup;
  ValuePath: String;
  OptionsData: TJSONObject;
  ItemsData: TJSONArray;
  PropData, ItemData, ValueData: TJSONData;
  i: Integer;
begin
  CheckGroup := Element.Control as TCheckGroup;
  PropData := Data.FindPath(Element.PropertyName);
  OptionsData := Element.OptionsData;
  //for now require PropData and items to be set
  if (PropData <> nil) and FindJSONProp(OptionsData, 'items', ItemsData) then
  begin
    if ItemsData.Count <> CheckGroup.Items.Count then
      raise Exception.Create('Mismatch item count between TCheckGroup and mediator configuration');
    ValuePath := OptionsData.Get('valuepath', 'value');
    case PropData.JSONType of
      jtObject:
        begin
          //todo
        end;
      jtArray:
        begin
          for i := 0 to ItemsData.Count -1 do
          begin
            ItemData := ItemsData.Items[i];
            if ItemData.JSONType = jtObject then
              ValueData := TJSONObject(ItemData).FindPath(ValuePath)
            else
              ValueData := ItemData;
            if ValueData.JSONType in [jtString, jtNull, jtNumber] then
              CheckGroup.Checked[i] := TJSONArray(PropData).IndexOf(ValueData.Value) <> -1
            else
              CheckGroup.Checked[i] := False;
          end;
        end;
    end;
  end
  else
  begin
    for i := 0 to CheckGroup.Items.Count - 1 do
      CheckGroup.Checked[i] := False;
  end;
end;

class procedure TJSONCheckGroupMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  CheckGroup: TCheckGroup;
  ValuePath: String;
  OptionsData: TJSONObject;
  ItemsData: TJSONArray;
  PropData, ItemData, ValueData: TJSONData;
  i: Integer;
begin
  CheckGroup := Element.Control as TCheckGroup;
  PropData := Data.FindPath(Element.PropertyName);
  OptionsData := Element.OptionsData;
  if FindJSONProp(OptionsData, 'items', ItemsData) then
  begin
    if ItemsData.Count <> CheckGroup.Items.Count then
      raise Exception.Create('Mismatch item count between TCheckGroup and mediator configuration');
    ValuePath := OptionsData.Get('valuepath', 'value');

    if (PropData = nil) or (PropData.JSONType in [jtString, jtNumber, jtNull]) then
    begin
      PropData := TJSONArray.Create;
      SetJSONPath(Data, Element.PropertyName, PropData);
    end
    else
      PropData.Clear;

    case PropData.JSONType of
      jtObject:
        begin
          //todo
        end;
      jtArray:
        begin
          for i := 0 to ItemsData.Count -1 do
          begin
            ItemData := ItemsData.Items[i];
            if ItemData.JSONType = jtObject then
              ValueData := TJSONObject(ItemData).FindPath(ValuePath)
            else
              ValueData := ItemData;
            if CheckGroup.Checked[i] and (ValueData.JSONType in [jtString, jtNull, jtNumber]) then
              TJSONArray(PropData).Add(ValueData.Clone);
          end;
        end;
    end;
  end;
end;

class procedure TJSONCheckGroupMediator.Initialize(Element: TJSONFormElement);
var
  CheckGroup: TCheckGroup;
begin
  CheckGroup := Element.Control as TCheckGroup;
  LoadItems(CheckGroup.Items, Element.OptionsData);
end;

{ TJSONRadioButtonMediator }

class procedure TJSONRadioButtonMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  RadioButton: TRadioButton;
  PropData, ValueData: TJSONData;
  Checked: Boolean;
begin
  RadioButton := Element.Control as TRadioButton;
  PropData := Data.FindPath(Element.PropertyName);
  ValueData := Element.OptionsData.Find('value');
  Checked := False;
  if (PropData <> nil) and (ValueData <> nil) then
    Checked := CompareJSONData(PropData, ValueData) = 0;
  RadioButton.Checked := Checked;
end;

class procedure TJSONRadioButtonMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  RadioButton: TRadioButton;
  ValueData: TJSONData;
begin
  RadioButton := Element.Control as TRadioButton;
  ValueData := Element.OptionsData.Find('value');
  if RadioButton.Checked and (ValueData <> nil) then
    SetJSONPath(Data, Element.PropertyName, ValueData.Clone);
end;

class function TJSONRadioButtonMediator.AllowsCaptionLabel: Boolean;
begin
  Result := False;
end;

{ TJSONMemoMediator }

class procedure TJSONMemoMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  PropData: TJSONArray;
  ItemData: TJSONData;
  i: Integer;
  MemoLines: TStrings;
begin
  if not (Element.OptionsData.Get('datatype', '') = 'array') then
    inherited DoJSONToGUI(Data, Element)
  else
  begin
    MemoLines := (Element.Control as TMemo).Lines;
    MemoLines.BeginUpdate;
    try
      MemoLines.Clear;
      if FindJSONPath(Data, Element.PropertyName, PropData) then
      begin
        for i := 0 to PropData.Count - 1 do
        begin
          ItemData := PropData.Items[i];
          if ItemData.JSONType = jtString then
            MemoLines.Add(ItemData.AsString);
        end;
      end;
    finally
      MemoLines.EndUpdate;
    end;
  end;
end;

class procedure TJSONMemoMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  MemoLines: TStrings;
  PropData: TJSONArray;
  i: Integer;
begin
  if not (Element.OptionsData.Get('datatype', '') = 'array') then
    inherited DoGUIToJSON(Element, Data)
  else
  begin
    MemoLines := (Element.Control as TMemo).Lines;
    PropData := TJSONArray.Create;
    for i := 0 to MemoLines.Count - 1 do
      PropData.Add(MemoLines[i]);
    SetJSONPath(Data, Element.PropertyName, PropData);
  end;
end;

{ TJSONCheckBoxMediator }

class procedure TJSONCheckBoxMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  CheckBox: TCheckBox;
  PropData, ValueData: TJSONData;
  Checked: Boolean;
begin
  CheckBox := Element.Control as TCheckBox;
  PropData := Data.FindPath(Element.PropertyName);
  ValueData := Element.OptionsData.Find('value');
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtObject, jtArray]) then
  begin
    Checked := False;
    if PropData <> nil then
    begin
      if Element.OptionsData.Get('grouped', False) or (PropData.JSONType in [jtObject, jtArray]) then
      begin
        case PropData.JSONType of
          jtObject:
            Checked := TJSONObject(PropData).Get(ValueData.AsString, False);
          jtArray:
            Checked := TJSONArray(PropData).IndexOf(ValueData.Value) > -1;
        end;
      end
      else
        Checked := CompareJSONData(ValueData, PropData) = 0;
    end;
  end
  else
  begin
    Checked := (PropData is TJSONBoolean) and PropData.AsBoolean;
  end;
  CheckBox.Checked := Checked;
end;

class procedure TJSONCheckBoxMediator.DoGUIToJSON(Element: TJSONFormElement;
  Data: TJSONObject);
var
  CheckBox: TCheckBox;
  ValueData, PropData: TJSONData;
  PropName: String;
  ValueIndex: Integer;
begin
  CheckBox := Element.Control as TCheckBox;
  PropName := Element.PropertyName;
  ValueData := Element.OptionsData.Find('value');
  if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtObject, jtArray]) then
  begin
    PropData := Data.Find(PropName);
    if Element.OptionsData.Get('grouped', False) or ((PropData <> nil) and (PropData.JSONType in [jtObject, jtArray])) then
    begin
      if PropData = nil then
      begin
        if CheckBox.Checked then
          Data.Arrays[PropName] := TJSONArray.Create([ValueData.Clone]);
      end
      else
      begin
        case PropData.JSONType of
          jtArray:
            begin
              ValueIndex := TJSONArray(PropData).IndexOf(ValueData.Value);
              if CheckBox.Checked then
              begin
                if ValueIndex = -1 then
                  TJSONArray(PropData).Add(ValueData.Clone);
              end
              else
              begin
                if ValueIndex <> -1 then
                  TJSONArray(PropData).Delete(ValueIndex);
              end;
            end;
          jtObject:
            begin
              ValueIndex := TJSONObject(PropData).IndexOfName(ValueData.AsString);
              if CheckBox.Checked then
              begin
                if ValueIndex = -1 then
                  TJSONObject(PropData).Booleans[ValueData.AsString] := True;
              end
              else
              begin
                if ValueIndex > -1 then
                  TJSONObject(PropData).Delete(ValueIndex);
              end;
            end;
        end;
      end;
    end
    else
    begin
      if CheckBox.Checked then
        Data.Elements[PropName] := ValueData.Clone
      else
      begin
        if Element.OptionsData.Get('removeprop', False) then
          Data.Delete(PropName)
        else
          Data.Booleans[PropName] := False;
      end;
    end;
  end
  else
  begin
    if not CheckBox.Checked and Element.OptionsData.Get('removeprop', False) and (Pos('.', PropName) = 0) then
      Data.Delete(PropName)
    else
      SetJSONPath(Data, PropName, TJSONBoolean.Create(CheckBox.Checked));
  end;
end;

class function TJSONCheckBoxMediator.AllowsCaptionLabel: Boolean;
begin
  Result := False;
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

class function TJSONRadioGroupMediator.AllowsCaptionLabel: Boolean;
begin
  Result := False;
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
    if OptionsData.Get('schema', 'text') = 'text' then
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
  if (ComboBox.ItemIndex = -1) and (ComboBox.Style = csDropDown) and (OptionsData.Get('schema', 'text') = 'text') then
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
  PropData, SchemaData: TJSONData;
  ItemsData: TJSONArray;
  IndexType: TJSONListIndexType;
  ValuePath: String;
begin
  Result := -1;
  PropData := Data.FindPath(PropName);
  if (PropData <> nil) and not (PropData.JSONType in [jtNull, jtArray, jtObject]) then
  begin
    //defaults to value if there's an items
    if FindJSONProp(OptionsData, 'items', ItemsData) then
      IndexType := jliValue
    else
      IndexType := jliText;
    ValuePath := OptionsData.Get('valuepath', 'value');
    SchemaData := OptionsData.Find('schema');
    if SchemaData <> nil then
    begin
      case SchemaData.JSONType of
        jtString:
          if SchemaData.AsString = 'index' then
            IndexType := jliIndex
          else if SchemaData.AsString = 'value' then
            IndexType := jliValue
          else if SchemaData.AsString = 'text' then
            IndexType := jliText;
        jtObject:
          begin
            IndexType := jliValue;
            //todo: remove schema.valuepath
            ValuePath := TJSONObject(SchemaData).Get('valuepath', ValuePath);
          end;
      end;
    end;
    case IndexType of
      jliText:
        Result := Items.IndexOf(PropData.AsString);
      jliIndex:
        begin
          if PropData.JSONType = jtNumber then
            Result := PropData.AsInteger;
        end;
      jliValue:
        begin
          if ItemsData <> nil then
            Result := ItemsData.IndexOf([ValuePath, PropData.Value]);
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
  IndexType: TJSONListIndexType;
  SchemaData, ValueData: TJSONData;
  ItemsData: TJSONArray;
  ValuePath: String;
  ItemData: TJSONData;
begin
  if ItemIndex > -1 then
  begin
    //defaults to value if there's an items
    if FindJSONProp(OptionsData, 'items', ItemsData) then
      IndexType := jliValue
    else
      IndexType := jliText;
    ValuePath := OptionsData.Get('valuepath', 'value');
    SchemaData := OptionsData.Find('schema');
    if SchemaData <> nil then
    begin
      case SchemaData.JSONType of
        jtString:
          if SchemaData.AsString = 'index' then
            IndexType := jliIndex
          else if SchemaData.AsString = 'value' then
            IndexType := jliValue
          else if SchemaData.AsString = 'text' then
            IndexType := jliText;
        jtObject:
          begin
            IndexType := jliValue;
            //todo: remove schema.valuepath
            ValuePath := TJSONObject(SchemaData).Get('valuepath', ValuePath);
          end;
      end;
    end;
    case IndexType of
      jliText:
        SetJSONPath(Data, PropName, TJSONString.Create(Items[ItemIndex]));
      jliIndex:
        SetJSONPath(Data, PropName, TJSONIntegerNumber.Create(ItemIndex));
      jliValue:
        begin
          if ItemsData <> nil then
          begin
            if ItemIndex < ItemsData.Count then
            begin
              ItemData := ItemsData.Items[ItemIndex];
              ValueData := ItemData.FindPath(ValuePath);
              if ValueData <> nil then
                SetJSONPath(Data, PropName, ValueData.Clone);
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
  ItemsData: TJSONArray;
  SchemaData: TJSONObject;
  ItemData, ValueData: TJSONData;
  TextPath: String;
  i: Integer;
begin
  if FindJSONProp(OptionsData, 'items', ItemsData) then
  begin
    TextPath := OptionsData.Get('textpath', 'text');
    //todo: remove schema.textpath
    if FindJSONProp(OptionsData, 'schema', SchemaData) then
      TextPath := SchemaData.Get('textpath', TextPath);

    Items.Clear;
    for i := 0 to ItemsData.Count - 1 do
    begin
      ItemData := ItemsData.Items[i];
      case ItemData.JSONType of
        jtString, jtNumber, jtBoolean:
          Items.Add(ItemData.AsString);
        jtObject:
          begin
            ValueData := ItemData.FindPath(TextPath);
            if (ValueData <> nil) and not (ValueData.JSONType in [jtNull, jtObject, jtArray]) then
              Items.Add(ValueData.AsString)
            else
              Items.Add('');
          end;
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
  DefaultValue: Double;
begin
  SpinEdit := Element.Control as TCustomFloatSpinEdit;
  PropData := Data.FindPath(Element.PropertyName);
  if (PropData = nil) or (PropData.JSONType = jtNull) then
  begin
    if FindJSONProp(Element.OptionsData, 'default', DefaultValue) then
    begin
      SpinEdit.Value := DefaultValue;
      SpinEdit.ValueEmpty := False;
    end
    else
      SpinEdit.ValueEmpty := True;
  end
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
  PropData: TJSONData;
  PropName: String;
begin
  SpinEdit := Element.Control as TCustomFloatSpinEdit;
  PropName := Element.PropertyName;
  if not SpinEdit.ValueEmpty then
  begin
    if SpinEdit.DecimalPlaces = 0 then
      PropData := TJSONInt64Number.Create(Round(SpinEdit.Value))
    else
      PropData := TJSONFloatNumber.Create(SpinEdit.Value);
    SetJSONPath(Data, PropName, PropData);
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
  PropData := Data.FindPath(Element.PropertyName);
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

class function TJSONCaptionMediator.AllowsCaptionLabel: Boolean;
begin
  Result := False;
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
var
  PropData: TJSONData;
  Text: String;
begin
  if FindJSONPath(Data, Element.PropertyName, PropData) and (PropData.JSONType in [jtString, jtNumber, jtBoolean]) then
    Text := PropData.AsString
  else
    Text := '';
  TControlAccess(Element.Control).Text := Text;
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
    SetJSONPath(Data, PropName, TJSONString.Create(ControlText));
end;

class procedure TJSONGUIMediator.Initialize(Element: TJSONFormElement);
begin
  //
end;

class function TJSONGUIMediator.AllowsCaptionLabel: Boolean;
begin
  Result := True;
end;

{ TJSONFormMediator }

procedure TJSONFormMediator.SetElements(Value: TJSONFormElements);
begin
  FElements.Assign(Value);
end;

function TJSONFormMediator.GetElements: TFormElements;
begin
  Result := FElements;
end;

procedure TJSONFormMediator.Loaded;
var
  i: Integer;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    for i := 0 to Elements.Count - 1 do
      InitializeCaptionDisplay(Elements[i]);
  end;
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

function TJSONFormMediator.ElementByName(const ElementName: String): TJSONFormElement;
begin
  Result := FElements.ElementByName(ElementName);
end;

procedure TJSONFormMediator.LoadData;
var
  i: Integer;
begin
  if FData = nil then
    Exit;
  BeginLoad;
  try
    for i := 0 to FElements.Count - 1 do
      TJSONFormElement(FElements[i]).LoadData(FData);
  finally
    EndLoad;
  end;
end;

procedure TJSONFormMediator.LoadData(const PropertySet: String);
var
  Element: TJSONFormElement;
begin
  //todo: implement property Set
  //for now load a element only
  Element := FElements.FindElement(PropertySet, True);
  if Element = nil then
    raise Exception.CreateFmt('Could not find a element with property "%s"', [PropertySet]);
  Element.LoadData(FData);
end;

procedure TJSONFormMediator.SaveData;
var
  i: Integer;
begin
  if FData = nil then
    Exit;
  for i := 0 to FElements.Count - 1 do
    TJSONFormElement(FElements[i]).SaveData(FData);
end;

procedure TJSONFormMediator.SaveData(const PropertySet: String);
var
  Element: TJSONFormElement;
begin
  //todo: implement property Set
  //for now save a element only
  Element := FElements.FindElement(PropertySet, True);
  if Element = nil then
    raise Exception.CreateFmt('Could not find a element with property "%s"', [PropertySet]);
  Element.SaveData(FData);
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

function TJSONFormElements.ElementByName(const ElementName: String): TJSONFormElement;
begin
  Result := FindElement(ElementName);
  if Result = nil then
    raise Exception.CreateFmt('Element "%s" not found', [ElementName]);
end;

function TJSONFormElements.FindElement(const Name: String;
  ByPropertyName: Boolean): TJSONFormElement;
var
  i: Integer;
begin
  if ByPropertyName then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i];
      if SameText(Result.PropertyName, Name) then
        Exit;
    end;
  end
  else
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i];
      if SameText(Result.Name, Name) then
        Exit;
    end;
  end;
  Result := nil;
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
    if Control <> nil then
    begin
      MediatorClassNeeded;
      FMediatorClass.Initialize(Self);
    end;
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

function TJSONFormElement.AllowsCaptionLabel: Boolean;
begin
  if Control <> nil then
  begin
    MediatorClassNeeded;
    Result := FMediatorClass.AllowsCaptionLabel;
  end
  else
    Result := True;
end;

function TJSONFormElement.FormMediator: TJSONFormMediator;
begin
  Result := (Collection.Owner as TJSONFormMediator);
end;

destructor TJSONFormElement.Destroy;
begin
  FOptionsData.Free;
  inherited Destroy;
end;

procedure TJSONFormElement.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJSONFormElement then
  begin
    Options := TJSONFormElement(Source).Options;
  end;
end;

procedure TJSONFormElement.Reset(UpdateData: Boolean);
var
  Data: TJSONObject;
begin
  FInitialized := False;
  if UpdateData then
  begin
    Data := FormMediator.Data;
    if Data <> nil then
    begin
      SaveData(Data);
      LoadData(Data);
    end;
  end;
end;

initialization
  MediatorStore := TJSONGUIMediatorStore.Create;
  RegisterJSONMediator(TEdit, TJSONEditMediator);
  RegisterJSONMediator(TLabeledEdit, TJSONEditMediator);
  RegisterJSONMediator(TMemo, TJSONMemoMediator);
  RegisterJSONMediator(TComboBox, TJSONComboBoxMediator);
  RegisterJSONMediator(TLabel, TJSONCaptionMediator);
  RegisterJSONMediator(TSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TFloatSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TRadioButton, TJSONRadioButtonMediator);
  RegisterJSONMediator(TRadioGroup, TJSONRadioGroupMediator);
  RegisterJSONMediator(TCheckBox, TJSONCheckBoxMediator);
  RegisterJSONMediator(TCheckGroup, TJSONCheckGroupMediator);

finalization
  MediatorStore.Destroy;

end.

