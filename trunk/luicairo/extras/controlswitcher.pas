unit ControlSwitcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiBar, Controls;

type

  TControlSwitcher = class;

  TControlCreateEvent = procedure(Sender: TControlSwitcher; NewControl: TControl) of object;

  { TControlInfo }

  TControlInfo = class(TCollectionItem)
  private
    FCaption: String;
    FControl: TControl;
    FControlClass: TControlClass;
    procedure SetControl(Value: TControl);
  protected
    function GetDisplayName: String; override;
  public
    property ControlClass: TControlClass read FControlClass write FControlClass;
  published
    property Caption: String read FCaption write FCaption;
    property Control: TControl read FControl write SetControl;
  end;

  { TControlList }

  TControlList = class(TCollection)
  private
    FOwner: TControlSwitcher;
    FValidList: Boolean;
    function GetItems(Index: Integer): TControlInfo;
  protected
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TControlSwitcher; AItemClass: TCollectionItemClass);
    property Items[Index: Integer]: TControlInfo read GetItems; default;
  end;

  { TControlSwitcher }

  TControlSwitcher = class(TLuiBar)
  private
    FControlList: TControlList;
    FOnControlCreate: TControlCreateEvent;
    procedure SetControlList(const AValue: TControlList);
    procedure UpdateCells;
  protected
    procedure CreateWnd; override;
    procedure DoChange; override;
    procedure DoControlCreate(NewControl: TControl); virtual;
    function DoSelecting(OldCell, NewCell: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachControl(const Title: String; Control: TControl);
    procedure AttachControlClass(const Title: String; ControlClass: TControlClass);
  published
    property ControlList: TControlList read FControlList write SetControlList;
    property OnControlCreate: TControlCreateEvent read FOnControlCreate write FOnControlCreate;

    property CellAlign;
    property CellHeight;
    property CellRoundRadius;
    property CellWidth;
    property Colors;
    property Constraints;
    property ImagePadding;
    property ImagePosition;
    property Images;
    property InitialSpace;
    property Options;
    property OuterOffset;
    property OutLineWidth;
    property Position;
    property SelectedIndex;
    property Spacing;
    property TextAlign;
    property TextPadding;

    property OnAfterDraw;
    property OnDrawBackground;
    property OnDrawCell;
    property OnDrawCellPath;
    property OnDrawCellText;
    property OnDrawing;
    property OnGetImageInfo;
    property OnCreatePattern;
    property OnGetCellPattern;
    property OnSelect;
    property OnSelecting;
  end;

implementation

uses
  LuiMiscUtils;

{ TControlSwitcher }

procedure TControlSwitcher.SetControlList(const AValue: TControlList);
begin
  FControlList.Assign(AValue);
end;

procedure TControlSwitcher.UpdateCells;
var
  i: Integer;
begin
  BeginUpdate;
  if SelectedIndex >= FControlList.Count then
    SelectedIndex := -1;
  Cells.Clear;
  for i := 0 to FControlList.Count - 1 do
    Cells.Add(FControlList[i].Caption);
  FControlList.FValidList := True;
  EndUpdate;
end;

procedure TControlSwitcher.CreateWnd;
begin
  inherited CreateWnd;
  UpdateCells;
  DoSelecting(-1, SelectedIndex);
end;

procedure TControlSwitcher.DoChange;
begin
  if not FControlList.FValidList then
    UpdateCells;
end;

procedure TControlSwitcher.DoControlCreate(NewControl: TControl);
begin
  if Assigned(FOnControlCreate) then
    FOnControlCreate(Self, NewControl);
end;

function TControlSwitcher.DoSelecting(OldCell, NewCell: Integer): Boolean;
var
  Control: TControl;
  ControlClass: TControlClass;
begin
  Result := inherited DoSelecting(OldCell, NewCell);
  if Result then
  begin
   if (NewCell <> -1) then
   begin
     Control := FControlList[NewCell].Control;
     if (Control = nil) then
     begin
       ControlClass := FControlList[NewCell].ControlClass;
       if (ControlClass <> nil) and (Parent <> nil) then
       begin
          Control := ControlClass.Create(Parent);
          Control.Visible := False;
          //todo customize the size/location (maybe use the dock sites?)
          Control.Align := alClient;
          Control.Parent := Parent;
          FControlList[NewCell].FControl := Control;
          DoControlCreate(Control);
          CallMethod(Control, 'InitControl');
       end;
     end;
     if Control <> nil then
       Control.Visible := True;
   end;
   if (OldCell <> -1) then
   begin
     Control := FControlList[OldCell].Control;
     if Control <> nil then
       Control.Visible := False;
   end;
  end;
end;

constructor TControlSwitcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlList := TControlList.Create(Self, TControlInfo);
end;

destructor TControlSwitcher.Destroy;
begin
  FControlList.Destroy;
  inherited Destroy;
end;

procedure TControlSwitcher.AttachControl(const Title: String; Control: TControl);
var
  NewControlInfo: TControlInfo;
begin
  NewControlInfo := TControlInfo(FControlList.Add);
  NewControlInfo.Control := Control;
  NewControlInfo.Caption := Title;
  Cells.Add(Title);
end;

procedure TControlSwitcher.AttachControlClass(const Title: String;
  ControlClass: TControlClass);
var
  NewControlInfo: TControlInfo;
begin
  NewControlInfo := TControlInfo(FControlList.Add);
  NewControlInfo.ControlClass := ControlClass;
  NewControlInfo.Caption := Title;
  Cells.Add(Title);
end;

{ TControlInfo }

procedure TControlInfo.SetControl(Value: TControl);
begin
  if Value <> nil then
    Value.Visible := False;
  FControl := Value;
end;

function TControlInfo.GetDisplayName: String;
begin
  Result := FCaption;
end;

{ TControlList }

function TControlList.GetItems(Index: Integer): TControlInfo;
begin
  Result := TControlInfo(GetItem(Index));
end;

procedure TControlList.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  FValidList := False;
end;

constructor TControlList.Create(AOwner: TControlSwitcher;
  AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FOwner := AOwner;
end;

end.

