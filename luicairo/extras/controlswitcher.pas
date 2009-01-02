unit ControlSwitcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiBar, Controls;

type
  TControlSwitcher = class;

  { TControlInfo }

  TControlInfo = class(TCollectionItem)
  private
    FCaption: String;
    FControl: TControl;
    procedure SetCaption(const AValue: String);
    procedure SetControl(const AValue: TControl);
  protected
    function GetDisplayName: String; override;
  published
    property Caption: String read FCaption write SetCaption;
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
    procedure SetControlList(const AValue: TControlList);
    procedure UpdateCells;
  protected
    procedure DoChange; override;
    function DoSelecting(OldCell, NewCell: Integer): Boolean; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachControl(const Title: String; Control: TControl);
  published
    property ControlList: TControlList read FControlList write SetControlList;

    property CellAlign;
    property CellHeight;
    property CellRoundRadius;
    property CellWidth;
    property Colors;
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

{ TControlSwitcher }

procedure TControlSwitcher.SetControlList(const AValue: TControlList);
begin
  FControlList.Assign(AValue);
end;

procedure TControlSwitcher.UpdateCells;
var
  i: Integer;
begin
  //todo: constrain SelectedIndex range
  Cells.Clear;
  for i := 0 to FControlList.Count - 1 do
    Cells.Add(FControlList[i].Caption);
  FControlList.FValidList := True;
end;

procedure TControlSwitcher.DoChange;
begin
  if not FControlList.FValidList then
    UpdateCells;
end;

function TControlSwitcher.DoSelecting(OldCell, NewCell: Integer): Boolean;
var
  Control: TControl;
begin
  Result := inherited DoSelecting(OldCell, NewCell);
  if Result then
  begin
   if (NewCell <> -1) then
   begin
     Control := FControlList[NewCell].Control;
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

procedure TControlSwitcher.Loaded;
begin
  inherited Loaded;
  if SelectedIndex >= FControlList.Count then
    SelectedIndex := -1;
  UpdateCells;
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

{ TControlInfo }

procedure TControlInfo.SetControl(const AValue: TControl);
begin
  if FControl=AValue then exit;
  FControl:=AValue;
end;

function TControlInfo.GetDisplayName: String;
begin
  Result := FCaption;
end;

procedure TControlInfo.SetCaption(const AValue: String);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
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

