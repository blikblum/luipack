unit LuiJSONLCLViews;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Controls, StdCtrls, ExtCtrls;

type

  TJSONObjectViewManager = class;

  TJSONMediatorState = set of (jmsLoading);

  { TCustomJSONGUIMediator }

  TCustomJSONGUIMediator = class
  public
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String; Control: TControl; Options: TJSONData); virtual;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject; const PropName: String; Options: TJSONData); virtual;
  end;

  TCustomJSONGUIMediatorClass = class of TCustomJSONGUIMediator;

  { TJSONGenericMediator }

  TJSONGenericMediator = class(TCustomJSONGUIMediator)
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
      Control: TControl; Options: TJSONData); override;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
      const PropName: String; Options: TJSONData); override;
  end;

  { TJSONCaptionMediator }

  TJSONCaptionMediator = class(TCustomJSONGUIMediator)
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
      Control: TControl; Options: TJSONData); override;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
      const PropName: String; Options: TJSONData); override;
  end;

  { TJSONSpinEditMediator }

  TJSONSpinEditMediator = class(TCustomJSONGUIMediator)
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
      Control: TControl; Options: TJSONData); override;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
      const PropName: String; Options: TJSONData); override;
  end;

  { TJSONRadioGroupMediator }

  TJSONRadioGroupMediator = class(TCustomJSONGUIMediator)
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
      Control: TControl; Options: TJSONData); override;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
      const PropName: String; Options: TJSONData); override;
  end;

  { TJSONCheckBoxMediator }

  TJSONCheckBoxMediator = class(TCustomJSONGUIMediator)
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
      Control: TControl; Options: TJSONData); override;
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
      const PropName: String; Options: TJSONData); override;
  end;

  { TJSONObjectPropertyView }

  TJSONObjectPropertyView = class(TCollectionItem)
  private
    FControl: TControl;
    FMediatorClass: TCustomJSONGUIMediatorClass;
    FMediatorId: String;
    FOptions: String;
    FOptionsData: TJSONData;
    FPropertyName: String;
    procedure MediatorClassNeeded;
    procedure OptionsDataNeeded;
    procedure SetControl(Value: TControl);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    procedure Load(JSONObject: TJSONObject);
    procedure Save(JSONObject: TJSONObject);
  published
    property Control: TControl read FControl write SetControl;
    property MediatorId: String read FMediatorId write FMediatorId;
    property Options: String read FOptions write FOptions;
    property PropertyName: String read FPropertyName write FPropertyName;
  end;

  { TJSONObjectPropertyViews }

  TJSONObjectPropertyViews = class(TCollection)
  private
    FOwner: TJSONObjectViewManager;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TJSONObjectViewManager);
  end;

  { TJSONObjectViewManager }

  TJSONObjectViewManager = class(TComponent)
  private
    FJSONObject: TJSONObject;
    FPropertyViews: TJSONObjectPropertyViews;
    FState: TJSONMediatorState;
    procedure SetJSONObject(const Value: TJSONObject);
    procedure SetPropertyViews(const Value: TJSONObjectPropertyViews);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Load(const Properties: array of String);
    procedure Save;
    procedure Save(const Properties: array of String);
    property JSONObject: TJSONObject read FJSONObject write SetJSONObject;
    property State: TJSONMediatorState read FState;
  published
    property PropertyViews: TJSONObjectPropertyViews read FPropertyViews write SetPropertyViews;
  end;

  procedure RegisterJSONMediator(const MediatorId: String; MediatorClass: TCustomJSONGUIMediatorClass);
  procedure RegisterJSONMediator(ControlClass: TControlClass; MediatorClass: TCustomJSONGUIMediatorClass);

implementation

uses
  contnrs, LuiJSONUtils, strutils, spin, typinfo;

type

  { TJSONGUIMediatorManager }

  TJSONGUIMediatorManager = class
  private
    FList: TFPHashList;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(const MediatorId: String): TCustomJSONGUIMediatorClass;
    procedure RegisterMediator(const MediatorId: String; MediatorClass: TCustomJSONGUIMediatorClass);
  end;

var
  MediatorManager: TJSONGUIMediatorManager;

procedure RegisterJSONMediator(const MediatorId: String;
  MediatorClass: TCustomJSONGUIMediatorClass);
begin
  MediatorManager.RegisterMediator(MediatorId, MediatorClass);
end;

procedure RegisterJSONMediator(ControlClass: TControlClass;
  MediatorClass: TCustomJSONGUIMediatorClass);
begin
  RegisterJSONMediator(ControlClass.ClassName, MediatorClass);
end;

{ TJSONCheckBoxMediator }

class procedure TJSONCheckBoxMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
var
  CheckBox: TCheckBox;
  PropData: TJSONData;
begin
  CheckBox := Control as TCheckBox;
  PropData := GetJSONProp(JSONObject, PropName);
  CheckBox.Checked := (PropData <> nil) and (PropData.JSONType = jtBoolean) and PropData.AsBoolean;
end;

class procedure TJSONCheckBoxMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
var
  CheckBox: TCheckBox;
begin
  CheckBox := Control as TCheckBox;
  if CheckBox.Checked then
    JSONObject.Booleans[PropName] := True
  else
    RemoveJSONProp(JSONObject, PropName);
end;

{ TJSONRadioGroupMediator }

class procedure TJSONRadioGroupMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
var
  RadioGroup: TRadioGroup;
  PropData: TJSONData;
  NewIndex: Integer;
begin
  RadioGroup := Control as TRadioGroup;
  PropData := GetJSONProp(JSONObject, PropName);
  if (PropData <> nil) and (PropData.JSONType <> jtNull) then
  begin
    //todo make options TJSONObject
    if (Options is TJSONObject) and TJSONObject(Options).Get('useindex', False) then
    begin
      if PropData.JSONType = jtNumber then
      begin
        NewIndex := PropData.AsInteger;
        if (NewIndex >= 0) and (NewIndex < RadioGroup.Items.Count) then
          RadioGroup.ItemIndex := NewIndex
        else
          RadioGroup.ItemIndex := -1;
      end;
    end
    else
    begin
      if PropData.JSONType = jtString then
        RadioGroup.ItemIndex := RadioGroup.Items.IndexOf(PropData.AsString);
    end;
  end
  else
    RadioGroup.ItemIndex := -1;
end;

class procedure TJSONRadioGroupMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
var
  RadioGroup: TRadioGroup;
  PropData: TJSONData;
begin
  RadioGroup := Control as TRadioGroup;
  if RadioGroup.ItemIndex <> -1 then
  begin
    if (Options is TJSONObject) and TJSONObject(Options).Get('useindex', False) then
      JSONObject.Integers[PropName] := RadioGroup.ItemIndex
    else
      JSONObject.Strings[PropName] := RadioGroup.Items[RadioGroup.ItemIndex]
  end
  else
    RemoveJSONProp(JSONObject, PropName);
end;

{ TJSONSpinEditMediator }

class procedure TJSONSpinEditMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
var
  PropData: TJSONData;
  SpinEdit: TCustomFloatSpinEdit;
begin
  SpinEdit := Control as TCustomFloatSpinEdit;
  PropData := GetJSONProp(JSONObject, PropName);
  if (PropData = nil) or (PropData.JSONType = jtNull) then
    SpinEdit.ValueEmpty := True
  else
  begin
    SpinEdit.Value := PropData.AsFloat;
    SpinEdit.ValueEmpty := False;
  end;
end;

class procedure TJSONSpinEditMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
var
  SpinEdit: TCustomFloatSpinEdit;
begin
  SpinEdit := Control as TCustomFloatSpinEdit;
  if not SpinEdit.ValueEmpty then
  begin
    if SpinEdit.DecimalPlaces = 0 then
      JSONObject.Integers[PropName] := round(SpinEdit.Value)
    else
      JSONObject.Floats[PropName] := SpinEdit.Value;
  end
  else
  begin
    //todo add option to configure undefined/null
    RemoveJSONProp(JSONObject, PropName);
    //JSONObject.Nulls[PropName] := True;
  end;
end;

{ TJSONGUIMediatorStore }

constructor TJSONGUIMediatorManager.Create;
begin
  FList := TFPHashList.Create;
end;

destructor TJSONGUIMediatorManager.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

function TJSONGUIMediatorManager.Find(const MediatorId: String): TCustomJSONGUIMediatorClass;
begin
  Result := TCustomJSONGUIMediatorClass(FList.Find(MediatorId));
end;

procedure TJSONGUIMediatorManager.RegisterMediator(const MediatorId: String;
  MediatorClass: TCustomJSONGUIMediatorClass);
begin
  FList.Add(MediatorId, MediatorClass);
end;

{ TJSONObjectViewManager }

procedure TJSONObjectViewManager.SetPropertyViews(const Value: TJSONObjectPropertyViews);
begin
  FPropertyViews.Assign(Value);
end;

procedure TJSONObjectViewManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
  View: TJSONObjectPropertyView;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    for i := 0 to FPropertyViews.Count -1 do
    begin
      View := TJSONObjectPropertyView(FPropertyViews.Items[i]);
      if AComponent = View.Control then
        View.Control := nil;
    end;
  end;
end;

procedure TJSONObjectViewManager.SetJSONObject(const Value: TJSONObject);
begin
  if FJSONObject = Value then exit;
  FJSONObject := Value;
end;

constructor TJSONObjectViewManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropertyViews := TJSONObjectPropertyViews.Create(Self);
end;

destructor TJSONObjectViewManager.Destroy;
begin
  FPropertyViews.Destroy;
  inherited Destroy;
end;

procedure TJSONObjectViewManager.Load;
var
  i: Integer;
  View: TJSONObjectPropertyView;
begin
  Include(FState, jmsLoading);
  try
    for i := 0 to FPropertyViews.Count - 1 do
    begin
      View := TJSONObjectPropertyView(FPropertyViews.Items[i]);
      View.Load(FJSONObject);
    end;
  finally
    Exclude(FState, jmsLoading);
  end;
end;

procedure TJSONObjectViewManager.Load(const Properties: array of String);
var
  i: Integer;
  View: TJSONObjectPropertyView;
begin
  Include(FState, jmsLoading);
  try
    for i := 0 to FPropertyViews.Count - 1 do
    begin
      View := TJSONObjectPropertyView(FPropertyViews.Items[i]);
      if AnsiMatchText(View.PropertyName, Properties) then
        View.Load(FJSONObject);
    end;
  finally
    Exclude(FState, jmsLoading);
  end;
end;

procedure TJSONObjectViewManager.Save;
var
  i: Integer;
  View: TJSONObjectPropertyView;
begin
  for i := 0 to FPropertyViews.Count -1 do
  begin
    View := TJSONObjectPropertyView(FPropertyViews.Items[i]);
    View.Save(FJSONObject);
  end;
end;

procedure TJSONObjectViewManager.Save(const Properties: array of String);
var
  i: Integer;
  View: TJSONObjectPropertyView;
begin
  for i := 0 to FPropertyViews.Count -1 do
  begin
    View := TJSONObjectPropertyView(FPropertyViews.Items[i]);
    if AnsiMatchText(View.PropertyName, Properties) then
      View.Save(FJSONObject);
  end;
end;

{ TJSONObjectPropertyViews }

function TJSONObjectPropertyViews.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TJSONObjectPropertyViews.Create(AOwner: TJSONObjectViewManager);
begin
  inherited Create(TJSONObjectPropertyView);
  FOwner := AOwner;
end;

{ TJSONObjectPropertyView }

procedure TJSONObjectPropertyView.MediatorClassNeeded;
begin
  if FMediatorClass = nil then
  begin
    if FMediatorId <> '' then
      FMediatorClass := MediatorManager.Find(FMediatorId)
    else
      FMediatorClass := MediatorManager.Find(Control.ClassName);
    if FMediatorClass = nil then
      raise Exception.CreateFmt('Could not find mediator (MediatorId: "%s" ControlClass: "%s")', [FMediatorId, Control.ClassName]);
  end;
end;

procedure TJSONObjectPropertyView.OptionsDataNeeded;
begin
  if (FOptions <> '') and (FOptionsData = nil) then
    FOptionsData := StringToJSONData(FOptions);
end;

procedure TJSONObjectPropertyView.SetControl(Value: TControl);
var
  TheOwner: TComponent;
begin
  if FControl = Value then Exit;
  TheOwner := Collection.Owner as TComponent;
  if (TheOwner <> nil) then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotification(TheOwner);
    if Value <> nil then
      Value.FreeNotification(TheOwner);
  end;
  FControl := Value;
end;

destructor TJSONObjectPropertyView.Destroy;
begin
  FOptionsData.Free;
  inherited Destroy;
end;

procedure TJSONObjectPropertyView.Assign(Source: TPersistent);
begin
  if Source is TJSONObjectPropertyView then
  begin
    PropertyName := TJSONObjectPropertyView(Source).PropertyName;
    Control := TJSONObjectPropertyView(Source).Control;
    Options := TJSONObjectPropertyView(Source).Options;
    MediatorId := TJSONObjectPropertyView(Source).MediatorId;
  end
  else
    inherited Assign(Source);
end;

function TJSONObjectPropertyView.GetDisplayName: string;
begin
  Result := FPropertyName;
  if Result = '' then
    Result := ClassName;
end;

procedure TJSONObjectPropertyView.Load(JSONObject: TJSONObject);
begin
  if FControl <> nil then
  begin
    //todo handle mediator and options loading once
    MediatorClassNeeded;
    OptionsDataNeeded;
    FMediatorClass.DoJSONToGUI(JSONObject, FPropertyName, FControl, FOptionsData);
  end;
end;

procedure TJSONObjectPropertyView.Save(JSONObject: TJSONObject);
begin
  if FControl <> nil then
  begin
    //todo handle mediator and options loading once
    MediatorClassNeeded;
    OptionsDataNeeded;
    FMediatorClass.DoGUIToJSON(FControl, JSONObject, FPropertyName, FOptionsData);
  end;
end;

{ TCustomJSONGUIMediator }

class procedure TCustomJSONGUIMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
begin
  //
end;

class procedure TCustomJSONGUIMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
begin
  //
end;

{ TJSONGenericMediator }

type
  TControlAccess = class(TControl)

  end;

class procedure TJSONGenericMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
begin
  TControlAccess(Control).Text := GetJSONProp(JSONObject, PropName, '');
end;

class procedure TJSONGenericMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
var
  i: Integer;
  ControlText: String;
begin
  i := JSONObject.IndexOfName(PropName);
  ControlText := TControlAccess(Control).Text;
  if (i <> -1) or (ControlText <> '') then
    JSONObject.Strings[PropName] := ControlText;
end;

{ TJSONCaptionMediator }

class procedure TJSONCaptionMediator.DoJSONToGUI(JSONObject: TJSONObject;
  const PropName: String; Control: TControl; Options: TJSONData);
var
  FormatStr, TemplateStr, ValueStr: String;
  PropData: TJSONData;
begin
  PropData := GetJSONProp(JSONObject, PropName);
  if PropData <> nil then
    ValueStr := PropData.AsString
  else
    ValueStr := '';
  if Options <> nil then
  begin
    case Options.JSONType of
      jtObject:
        begin
          if PropData <> nil then
          begin
            FormatStr := GetJSONProp(TJSONObject(Options), 'format', '');
            if FormatStr = 'date' then
              ValueStr := DateToStr(PropData.AsFloat)
            else if FormatStr = 'datetime' then
              ValueStr := DateTimeToStr(PropData.AsFloat);
          end;
          TemplateStr := GetJSONProp(TJSONObject(Options), 'template', '%s');
        end;
      jtString: //template
        TemplateStr := Options.AsString;
    else
    begin
      TemplateStr := '%s';
    end;
    end;
    Control.Caption := Format(TemplateStr, [ValueStr]);
  end
  else
    Control.Caption := ValueStr;
end;

class procedure TJSONCaptionMediator.DoGUIToJSON(Control: TControl;
  JSONObject: TJSONObject; const PropName: String; Options: TJSONData);
begin
  //
end;

initialization
  MediatorManager := TJSONGUIMediatorManager.Create;
  RegisterJSONMediator(TEdit, TJSONGenericMediator);
  RegisterJSONMediator(TMemo, TJSONGenericMediator);
  RegisterJSONMediator(TComboBox, TJSONGenericMediator);
  RegisterJSONMediator(TLabel, TJSONCaptionMediator);
  RegisterJSONMediator(TSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TFloatSpinEdit, TJSONSpinEditMediator);
  RegisterJSONMediator(TRadioGroup, TJSONRadioGroupMediator);
  RegisterJSONMediator(TCheckBox, TJSONCheckBoxMediator);

finalization
  MediatorManager.Destroy;

end.

