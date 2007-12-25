unit LuiBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, types, Controls, Cairo14;

type


  TLuiBar = class;
  
  TLuiBarOption = (lboEmulateTab);
  
  TLuiBarOptions = set of TLuiBarOption;
  
  TLuiBarColors = record
    Normal: TCairoColor;
    Hover: TCairoColor;
    Selected: TCairoColor;
    BackGround: TCairoColor;
    Text: TCairoColor;
    OutLine: TCairoColor;
  end;
  
  { TCellInfo }

  TCellInfo = class
  private
    FBounds: TRect;
    FIndex: Integer;
    FPattern: TCairoPattern;
    FTitle: String;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetBounds(const AValue: TRect);
    procedure SetIndex(const AValue: Integer);
    procedure SetPattern(const AValue: TCairoPattern);
    procedure SetTitle(const AValue: String);
  public
    destructor Destroy; override;
    property Height: Integer read GetHeight;
    property Index: Integer read FIndex write SetIndex;
    property Pattern: TCairoPattern read FPattern write SetPattern;
    property Title: String read FTitle write SetTitle;
    property Bounds: TRect read FBounds write SetBounds;
    property Width: Integer read GetWidth;
  end;

  { TCellInfoList }

  TCellInfoList = class
  private
    FInnerRadius: Double;
    FList: TFpList;
    FOuterRadius: Double;
    FOwner: TLuiBar;
    procedure InitBounds(Cell: TCellInfo);
    function GetCount: Integer;
    function GetItems(Index: Integer): TCellInfo; inline;
    procedure SetInnerRadius(const AValue: Double);
    procedure SetOuterRadius(const AValue: Double);
  public
    constructor Create(Owner: TLuiBar);
    destructor Destroy; override;
    function Add(const Title: String): TCellInfo;
    procedure Clear;
    property Count: Integer read GetCount;

    property Items[Index: Integer]: TCellInfo read GetItems; default;

  end;
  { TLuiBar }

  TLuiBar = class(TCairoControl)
  private
    FCells: TCellInfoList;
    FCellWidth: Integer;
    FColors: TLuiBarColors;
    FInnerRadius: Double;
    FOffset: TRect;
    FOptions: TLuiBarOptions;
    FOuterRadius: Double;
    FOutLineWidth: Integer;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;
    FSpacing: Integer;
    function CellInPoint(const P: TPoint): Integer;
    procedure RectanglePath(AWidth, AHeight: Integer);
  protected
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawCell(Cell: TCellInfo); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const CellTitle: String);
    property Cells: TCellInfoList read FCells;
    property CellWidth: Integer read FCellWidth write FCellWidth;
    property Colors: TLuiBarColors read FColors write FColors;
    property InnerRadius: Double read FInnerRadius write FInnerRadius;
    property Offset: TRect read FOffset write FOffset;
    property Options: TLuiBarOptions read FOptions write FOptions;
    property OuterRadius: Double read FOuterRadius write FOuterRadius;
    property OutLineWidth: Integer read FOutLineWidth write FOutLineWidth;
    property Spacing: Integer read FSpacing write FSpacing;
  published

  end;

implementation

uses
  sharedlogger;

{ TCellInfo }

{ TCellInfoList }

function TCellInfoList.GetItems(Index: Integer): TCellInfo;
begin
  Result := TCellInfo(FList[Index]);
end;

procedure TCellInfoList.SetInnerRadius(const AValue: Double);
begin
  if FInnerRadius=AValue then exit;
  FInnerRadius:=AValue;
end;

procedure TCellInfoList.SetOuterRadius(const AValue: Double);
begin
  if FOuterRadius=AValue then exit;
  FOuterRadius:=AValue;
end;

constructor TCellInfoList.Create(Owner: TLuiBar);
begin
  FList := TFPList.Create;
  FOwner := Owner;
end;

procedure TCellInfoList.InitBounds(Cell: TCellInfo);
begin
  Cell.Bounds.Top := FOwner.Offset.Top;
  Cell.Bounds.Bottom := FOwner.Height - FOwner.Offset.Bottom;

  if FList.Count > 0 then
    Cell.Bounds.Left := Items[FList.Count - 1].Bounds.Right + FOwner.Spacing
  else
    Cell.Bounds.Left := FOwner.Offset.Left;
  Cell.Bounds.Right := Cell.Bounds.Left + FOwner.CellWidth;
  Logger.Send('InitBounds of ' + Cell.Title, Cell.Bounds);
end;

function TCellInfoList.GetCount: Integer;
begin
  Result := FList.Count;
end;


destructor TCellInfoList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

function TCellInfoList.Add(const Title: String): TCellInfo;
begin
  Result := TCellInfo.Create;
  Result.Title := Title;
  Result.Index := FList.Count;
  InitBounds(Result);
  FList.Add(Result);
end;

procedure TCellInfoList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TCellInfo(FList[i]).Destroy;
  FList.Clear;
end;

{ TCellInfo }

procedure TCellInfo.SetPattern(const AValue: TCairoPattern);
begin
  FPattern := AValue;
end;

procedure TCellInfo.SetBounds(const AValue: TRect);
begin
  FBounds := AValue;
end;

function TCellInfo.GetWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

function TCellInfo.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

procedure TCellInfo.SetIndex(const AValue: Integer);
begin
  if FIndex=AValue then exit;
  FIndex:=AValue;
end;

procedure TCellInfo.SetTitle(const AValue: String);
begin
  FTitle := AValue;
end;

destructor TCellInfo.Destroy;
begin
  FPattern.Free;
end;

{ TLuiBar }

function TLuiBar.CellInPoint(const P: TPoint): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCells.Count - 1 do
    if PtInRect(FCells[i].Bounds, P) then
      Exit(i);
end;

procedure TLuiBar.RectanglePath(AWidth, AHeight: Integer);
begin
  with Context do
  begin
    MoveTo(FInnerRadius, AHeight);
    LineTo(FInnerRadius, AHeight);
    CurveTo(FInnerRadius, AHeight,
      0, AHeight,
      0, AHeight - FInnerRadius);
    LineTo(0, FOuterRadius);
    CurveTo(0, FOuterRadius,
      0, 0,
      FOuterRadius, 0);
    LineTo(AWidth - FOuterRadius, 0);
    CurveTo(AWidth - FOuterRadius,
      0, AWidth, 0,
      AWidth, FOuterRadius);
    LineTo(AWidth, AHeight - FInnerRadius);
    CurveTo(AWidth, AHeight - FInnerRadius,
      AWidth, AHeight,
      AWidth - FInnerRadius, AHeight);
    if not (lboEmulateTab in FOptions) then
      ClosePath;
  end;
end;

procedure TLuiBar.DoDraw;
var
  i: Integer;
begin
  DoDrawBackground;
  for i := 0 to FCells.Count - 1 do
    DoDrawCell(FCells[i]);
end;

procedure TLuiBar.DoDrawCell(Cell: TCellInfo);
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    Save;
    Translate(Cell.Bounds.Left, Cell.Bounds.Top);
    RectanglePath(Cell.Width, Cell.Height);
    ClipPreserve;
    if Cell.Index = FSelectedIndex then
      Color := FColors.Selected
    else
      if Cell.Index = FHoverIndex then
        Color := FColors.Hover
      else
        Color := FColors.Normal;
    FillPreserve;
    SetLineWidth(FOutLineWidth);
    Color := FColors.OutLine;
    Stroke;
    //draw text
    Color := FColors.Text;
    TextExtents(Cell.Title, @Extents);
    MoveTo((Cell.Width  - Extents.width) / 2 , (Cell.Height + Extents.height) / 2);
    ShowText(Cell.Title);
    Restore;
  end;
end;

procedure TLuiBar.DoDrawBackground;
var
  SelectedCell: TCellInfo;
begin
  with Context do
  begin
    Save;
    Color := FColors.Background;
    Rectangle(0, 0, Width, Height);
    Fill;
    if lboEmulateTab in FOptions then
    begin
      Rectangle(0, Height - FOffset.Bottom, Width, FOffset.Bottom);
      Color := FColors.Selected;
      Fill;
      //Draw outline
      SetLineWidth(FOutLineWidth);
      Color := FColors.OutLine;
      MoveTo(0, Height - FOffset.Bottom);
      if FSelectedIndex <> -1 then
      begin
        SelectedCell := FCells[FSelectedIndex];
        LineTo(SelectedCell.Bounds.Left, Height - FOffset.Bottom);
        Stroke;
        MoveTo(SelectedCell.Bounds.Right, Height - FOffset.Bottom);
      end;
      LineTo(Width, Height - FOffset.Bottom);
      Stroke;
    end;
    Restore;
  end;
end;

procedure TLuiBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClickedCell: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedCell := CellInPoint(Point(X,Y));
    if (ClickedCell <> -1) and (ClickedCell <> FSelectedIndex) then
    begin
      FSelectedIndex := ClickedCell;
      Redraw;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TLuiBar.MouseLeave;
begin
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Redraw;
  end;
  inherited MouseLeave;
end;

procedure TLuiBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHoverIndex: Integer;
begin
  NewHoverIndex := CellInPoint(Point(X, Y));
  if NewHoverIndex <> FHoverIndex then
  begin
    FHoverIndex := NewHoverIndex;
    Redraw;
  end;
  inherited MouseMove(Shift, X, Y);
end;

constructor TLuiBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCells := TCellInfoList.Create(Self);
  //init colors
  with FColors do
  begin
    Normal := CairoColor(1, 0, 0, 0.4);
    Selected := CairoColor(1, 0, 0, 1);
    Hover := CairoColor(1, 0, 0, 0.7);
    BackGround := CairoColor(1, 1, 1, 1);
    Text := CairoColor(1, 1, 1, 1);
    OutLine := CairoColor(0, 0, 0, 1);
  end;
  FHoverIndex := -1;
  FCellWidth := 100;
  FSelectedIndex := -1;
end;

destructor TLuiBar.Destroy;
begin
  FCells.Destroy;
  inherited Destroy;
end;

procedure TLuiBar.Add(const CellTitle: String);
var
  NewCell: TCellInfo;
begin
  NewCell := FCells.Add(CellTitle);
end;

end.

