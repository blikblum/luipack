unit JSONBooleanGroupView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Controls;

type

  TBooleanValue = (bvTrue, bvFalse, bvUndefined, bvNone);

  { TJSONBooleanProperty }

  TJSONBooleanProperty = class(TCollectionItem)
  private
    FCaption: String;
    FName: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write FCaption;
    property Name: String read FName write FName;
  end;

  TJSONBooleanProperties = class(TOwnedCollection)
  public
  end;

  { TJSONBooleanGroupManager }

  TJSONBooleanGroupManager = class(TComponent)
  private
    FControl: TWinControl;
    FFalseText: String;
    FProperties: TJSONBooleanProperties;
    FTrueText: String;
    FUndefinedText: String;
    FViews: TFPList;
    FDefaultValue: TBooleanValue;
    procedure CreateViews;
    procedure SetControl(AValue: TWinControl);
    procedure SetProperties(AValue: TJSONBooleanProperties);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckValues: Boolean;
  published
    property Control: TWinControl read FControl write SetControl;
    property DefaultValue: TBooleanValue read FDefaultValue write FDefaultValue;
    property FalseText: String read FFalseText write FFalseText;
    property Properties: TJSONBooleanProperties read FProperties write SetProperties;
    property TrueText: String read FTrueText write FTrueText;
    property UndefinedText: String read FUndefinedText write FUndefinedText;
  end;

implementation

uses
  JSONBooleanRadioButtonView;

{ TJSONBooleanProperty }

procedure TJSONBooleanProperty.Assign(Source: TPersistent);
begin
  if Source is TJSONBooleanProperty then
  begin
    FCaption := TJSONBooleanProperty(Source).FCaption;
    FName := TJSONBooleanProperty(Source).FName;
  end
  else
    inherited Assign(Source);
end;

{ TJSONBooleanGroupManager }

procedure TJSONBooleanGroupManager.CreateViews;
var
  View: TJSONBooleanRadioButtonViewFrame;
  JSONProperty: TJSONBooleanProperty;
  i: Integer;
begin
  if FControl = nil then
    Exit;
  FViews.Clear;
  for i := 0 to FProperties.Count - 1 do
  begin
    JSONProperty := TJSONBooleanProperty(FProperties.Items[i]);
    //todo: use self as owner
    View := TJSONBooleanRadioButtonViewFrame.Create(FControl);
    View.PropertyName := JSONProperty.Name;
    View.PropertyCaption := JSONProperty.Caption;
    View.TrueText := FTrueText;
    View.FalseText := FFalseText;
    View.UndefinedText := FUndefinedText;
    //todo
    View.InitialValue := FDefaultValue;
    View.Parent := FControl;
    View.Visible := True;
    FViews.Add(View);
  end;
end;

procedure TJSONBooleanGroupManager.SetControl(AValue: TWinControl);
begin
  if FControl = AValue then Exit;
  FControl := AValue;
end;

procedure TJSONBooleanGroupManager.SetProperties(AValue: TJSONBooleanProperties);
begin
  FProperties.Assign(AValue);
end;

constructor TJSONBooleanGroupManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TJSONBooleanProperties.Create(Self, TJSONBooleanProperty);
  FViews := TFPList.Create;
  FFalseText := 'No';
  FTrueText := 'Yes';
end;

destructor TJSONBooleanGroupManager.Destroy;
begin
  FProperties.Destroy;
  FViews.Destroy;
  inherited Destroy;
end;

function TJSONBooleanGroupManager.CheckValues: Boolean;
begin
  //todo
end;

end.

