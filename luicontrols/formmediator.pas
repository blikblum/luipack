unit FormMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  TCustomFormMediator = class;

  TFormMediatorState = set of (fmsLoading);

  { TFormElement }

  TFormElement = class(TCollectionItem)
  private
    FControl: TControl;
    FMediatorId: String;
    FName: String;
    FPropertyName: String;
    function GetName: String;
    procedure SetControl(Value: TControl);
  public
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
  published
    property Control: TControl read FControl write SetControl;
    property MediatorId: String read FMediatorId write FMediatorId;
    property Name: String read GetName write FName;
    property PropertyName: String read FPropertyName write FPropertyName;
  end;

  TFormElementClass = class of TFormElement;

  { TFormElements }

  TFormElements = class(TOwnedCollection)
  private
  protected
  public
  end;

  TFormElementsClass = class of TFormElements;

  { TCustomFormMediator }

  TCustomFormMediator = class(TComponent)
  private
    FState: TFormMediatorState;
  protected
    procedure BeginLoad;
    procedure EndLoad;
  public
    property State: TFormMediatorState read FState;
  published
  end;


implementation

{ TCustomFormMediator }

procedure TCustomFormMediator.BeginLoad;
begin
  Include(FState, fmsLoading);
end;

procedure TCustomFormMediator.EndLoad;
begin
  Exclude(FState, fmsLoading);
end;


procedure TFormElement.SetControl(Value: TControl);
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

function TFormElement.GetName: String;
begin
  if FName <> '' then
    Result := FName
  else
    Result := FPropertyName;
end;

procedure TFormElement.Assign(Source: TPersistent);
begin
  if Source is TFormElement then
  begin
    PropertyName := TFormElement(Source).PropertyName;
    Control := TFormElement(Source).Control;
    Name := TFormElement(Source).Name;
    MediatorId := TFormElement(Source).MediatorId;
  end
  else
    inherited Assign(Source);
end;

function TFormElement.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := ClassName;
  if FControl <> nil then
    Result := Format('%s (%s)', [Result, FControl.Name]);
end;


end.

