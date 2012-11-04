unit JSONBooleanGroupView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Controls, Forms;

type

  TBooleanValue = (bvTrue, bvFalse, bvIndeterminate, bvNone);

  { TJSONBooleanProperty }

  TJSONBooleanProperty = class(TCollectionItem)
  private
    FCaption: String;
    FName: String;
    FView: TCustomFrame; //todo: add IJSONBooleanView intf
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write FCaption;
    property Name: String read FName write FName;
  end;

  TJSONBooleanProperties = class(TOwnedCollection)
  public
  end;

  { TJSONBooleanMediator }

  TJSONBooleanMediator = class(TComponent)
  private
    FControl: TWinControl;
    FFalseCaption: String;
    FNullValue: TBooleanValue;
    FProperties: TJSONBooleanProperties;
    FTrueCaption: String;
    FIndeterminateCaption: String;
    FUndefinedValue: TBooleanValue;
    procedure CreateViews(Data: TJSONObject);
    procedure SetControl(AValue: TWinControl);
    procedure SetProperties(AValue: TJSONBooleanProperties);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckValues: Boolean;
    procedure LoadData(Data: TJSONObject);
  published
    property Control: TWinControl read FControl write SetControl;
    property FalseCaption: String read FFalseCaption write FFalseCaption;
    property IndeterminateCaption: String read FIndeterminateCaption write FIndeterminateCaption;
    property NullValue: TBooleanValue read FNullValue write FNullValue default bvIndeterminate;
    property Properties: TJSONBooleanProperties read FProperties write SetProperties;
    property TrueCaption: String read FTrueCaption write FTrueCaption;
    property UndefinedValue: TBooleanValue read FUndefinedValue write FUndefinedValue default bvNone;
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

{ TJSONBooleanMediator }

procedure TJSONBooleanMediator.CreateViews(Data: TJSONObject);
var
  View: TJSONBooleanRadioButtonViewFrame;
  JSONProperty: TJSONBooleanProperty;
  i, PropIndex: Integer;
  PropData: TJSONData;
  InitialValue: TBooleanValue;
begin
  if (FControl = nil) or (Data = nil) then
    Exit;
  for i := 0 to FProperties.Count - 1 do
  begin
    JSONProperty := TJSONBooleanProperty(FProperties.Items[i]);
    //todo: use self as owner
    View := TJSONBooleanRadioButtonViewFrame.Create(FControl);
    View.PropertyCaption := JSONProperty.Caption;
    View.TrueCaption := FTrueCaption;
    View.FalseCaption := FFalseCaption;
    View.IndeterminateCaption := FIndeterminateCaption;
    PropIndex := Data.IndexOfName(JSONProperty.Name);
    if PropIndex = -1 then
      InitialValue := FUndefinedValue
    else
    begin
      PropData := Data.Items[PropIndex];
      case PropData.JSONType of
        jtBoolean:
        begin
          if PropData.AsBoolean then
            InitialValue := bvTrue
          else
            InitialValue := bvFalse;
        end;
        jtNull:
          InitialValue := FNullValue;
        else
        begin
          InitialValue := bvIndeterminate;  //??
        end;
      end;
    end;
    View.InitialValue := InitialValue;
    View.Parent := FControl;
    View.Visible := True;
    JSONProperty.FView := View;
  end;
end;

procedure TJSONBooleanMediator.SetControl(AValue: TWinControl);
begin
  if FControl = AValue then Exit;
  FControl := AValue;
end;

procedure TJSONBooleanMediator.SetProperties(AValue: TJSONBooleanProperties);
begin
  FProperties.Assign(AValue);
end;

constructor TJSONBooleanMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TJSONBooleanProperties.Create(Self, TJSONBooleanProperty);
  FFalseCaption := 'No';
  FTrueCaption := 'Yes';
  FIndeterminateCaption := 'Indeterminate';
  FNullValue := bvIndeterminate;
  FUndefinedValue := bvNone;
end;

destructor TJSONBooleanMediator.Destroy;
begin
  FProperties.Destroy;
  inherited Destroy;
end;

function TJSONBooleanMediator.CheckValues: Boolean;
begin
  //todo
end;

procedure TJSONBooleanMediator.LoadData(Data: TJSONObject);
var
  i: Integer;
begin
 // for i := 0 to FViews.Count - 1;
end;

end.

