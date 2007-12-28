unit LuiBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, types, Controls, Cairo14;

type


  TLuiBar = class;
  
  TCellInfo = class;
  
  TLuiBarOption = (lboEmulateTab);
  
  TLuiBarOptions = set of TLuiBarOption;
  
  TLuiBarNotify = procedure (Sender: TLuiBar) of object;
  
  TPatternType = (ptSelected, ptNormal, ptHover, ptText, ptSelectedText, ptBackground, ptOutLine);
  
  TGetDefaultPattern = procedure (Sender: TLuiBar; PatternType: TPatternType;
    var Pattern: TCairoPattern; var Color: TCairoColor) of object;
  
  TGetCellPattern = procedure (Cell: TCellInfo; PatternType: TPatternType;
    var Pattern: TCairoPattern; var Color: TCairoColor) of object;
  
  TLuiBarColors = record
    Normal: TCairoColor;
    Hover: TCairoColor;
    Selected: TCairoColor;
    BackGround: TCairoColor;
    Text: TCairoColor;
    OutLine: TCairoColor;
  end;
  
  { TLuiBarPatterns }

  TLuiBarPatterns = class
  private
    FBackGround: TCairoPattern;
    FHover: TCairoPattern;
    FInvalid: Boolean;
    FFixedValues: Boolean;
    FNormal: TCairoPattern;
    FOutLine: TCairoPattern;
    FSelected: TCairoPattern;
    FSelectedText: TCairoPattern;
    FText: TCairoPattern;
    function GetRequiresUpdate: Boolean;
    procedure SetBackGround(const AValue: TCairoPattern);
    procedure SetHover(const AValue: TCairoPattern);
    procedure SetNormal(const AValue: TCairoPattern);
    procedure SetOutLine(const AValue: TCairoPattern);
    procedure SetSelected(const AValue: TCairoPattern);
    procedure SetSelectedText(const AValue: TCairoPattern);
    procedure SetText(const AValue: TCairoPattern);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    procedure Updated;
    property Normal: TCairoPattern read FNormal write SetNormal;
    property Hover: TCairoPattern read FHover write SetHover;
    property Selected: TCairoPattern read FSelected write SetSelected;
    property BackGround: TCairoPattern read FBackGround write SetBackGround;
    property Text: TCairoPattern read FText write SetText;
    property SelectedText: TCairoPattern read FSelectedText write SetSelectedText;
    property OutLine: TCairoPattern read FOutLine write SetOutLine;
    property RequiresUpdate: Boolean read GetRequiresUpdate;
    property FixedValues: Boolean read FFixedValues write FFixedValues;
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
    //FColors: TLuiBarColors;
    FPatterns: TLuiBarPatterns;
    FInnerRadius: Double;
    FOffset: TRect;
    FOnGetDefaultPatterns: TGetDefaultPattern;
    FOnSelect: TLuiBarNotify;
    FOptions: TLuiBarOptions;
    FOuterRadius: Double;
    FOutLineWidth: Integer;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;
    FSpacing: Integer;
    function CellInPoint(const P: TPoint): Integer;
    function GetCellHeight: Integer;
    procedure SetOnGetDefaultPatterns(const AValue: TGetDefaultPattern);
    procedure SetSelectedIndex(const AValue: Integer);
  protected
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawCell(Cell: TCellInfo); virtual;
    procedure DoDrawCellPath(AWidth, AHeight: Integer); virtual;
    procedure DoSelect; virtual;
    procedure DoUpdatePatterns; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const CellTitle: String);
    property Cells: TCellInfoList read FCells;
    property CellHeight: Integer read GetCellHeight;
    property CellWidth: Integer read FCellWidth write FCellWidth;
    //property Colors: TLuiBarColors read FColors write FColors;
    property InnerRadius: Double read FInnerRadius write FInnerRadius;
    property Offset: TRect read FOffset write FOffset;
    property Options: TLuiBarOptions read FOptions write FOptions;
    property OuterRadius: Double read FOuterRadius write FOuterRadius;
    property OutLineWidth: Integer read FOutLineWidth write FOutLineWidth;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Spacing: Integer read FSpacing write FSpacing;
    //events
    property OnGetDefaultPatterns: TGetDefaultPattern read FOnGetDefaultPatterns write SetOnGetDefaultPatterns;
    property OnSelect: TLuiBarNotify read FOnSelect write FOnSelect;
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

function TLuiBar.GetCellHeight: Integer;
begin
  Result := Height - (FOffset.Top + FOffset.Bottom);
end;

procedure TLuiBar.SetOnGetDefaultPatterns(const AValue: TGetDefaultPattern);
begin
  FOnGetDefaultPatterns := AValue;
  FPatterns.FixedValues := AValue = nil;
end;

procedure TLuiBar.DoUpdatePatterns;
var
  TempColor: TCairoColor;
  TempPattern: TCairoPattern;
  
  function DoGetPattern(AType: TPatternType; DefaultColor: TCairoColor): TCairoPattern;
  begin
    Result := nil;
    if Assigned(FOnGetDefaultPatterns) then
      FOnGetDefaultPatterns(Self, AType, Result, DefaultColor);
    if Result = nil then
      Result := TCairoSolidPattern.Create(DefaultColor);
  end;
  
begin
  with FPatterns do
  begin
    Normal := DoGetPattern(ptNormal, CairoColor(1, 0, 0, 0.4));
    Selected := DoGetPattern(ptSelected, CairoColor(1, 0, 0, 1));
    Hover := DoGetPattern(ptHover, CairoColor(1, 0, 0, 0.7));
    BackGround := DoGetPattern(ptBackground, CairoColor(1, 1, 1, 1));
    Text := DoGetPattern(ptText, CairoColor(1, 1, 1, 1));
    OutLine := DoGetPattern(ptOutLine, CairoColor(0, 0, 0, 1));
    SelectedText := DoGetPattern(ptSelectedText, CairoColor(1, 1, 1, 1));
    Updated;
  end;
end;

procedure TLuiBar.DoDrawCellPath(AWidth, AHeight: Integer);
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

procedure TLuiBar.SetSelectedIndex(const AValue: Integer);
begin
  if AValue >= FCells.Count then
    raise Exception.Create('Cell index out of bounds');
  if AValue >= 0 then
    FSelectedIndex := AValue
  else
    FSelectedIndex := -1;
  Redraw;
end;

procedure TLuiBar.DoDraw;
var
  i: Integer;
begin
  if FPatterns.RequiresUpdate then
    DoUpdatePatterns;
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
    DoDrawCellPath(Cell.Width, Cell.Height);
    ClipPreserve;
    if Cell.Index = FSelectedIndex then
      Source := FPatterns.Selected
    else
      if Cell.Index = FHoverIndex then
        Source := FPatterns.Hover
      else
        Source := FPatterns.Normal;
    FillPreserve;
    LineWidth := FOutLineWidth;
    Source := FPatterns.OutLine;
    Stroke;
    //draw text
    if Cell.Index = FSelectedIndex then
      Source := FPatterns.SelectedText
    else
      Source := FPatterns.Text;
    TextExtents(Cell.Title, @Extents);
    MoveTo((Cell.Width  - Extents.width) / 2 , (Cell.Height + Extents.height) / 2);
    ShowText(Cell.Title);
    Restore;
  end;
end;

procedure TLuiBar.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TLuiBar.DoDrawBackground;
var
  SelectedCell: TCellInfo;
begin
  with Context do
  begin
    Save;
    Source := FPatterns.Background;
    Rectangle(0, 0, Width, Height);
    Fill;
    if lboEmulateTab in FOptions then
    begin
      Rectangle(0, Height - FOffset.Bottom, Width, FOffset.Bottom);
      Source := FPatterns.Selected;
      Fill;
      //Draw outline
      LineWidth := FOutLineWidth;
      Source := FPatterns.OutLine;
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
      DoSelect;
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
  FPatterns := TLuiBarPatterns.Create;
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

{ TLuiBarPatterns }

procedure TLuiBarPatterns.SetBackGround(const AValue: TCairoPattern);
begin
  //if FBackGround=AValue then exit;
  FBackGround.Free;
  FBackGround := AValue;
end;

function TLuiBarPatterns.GetRequiresUpdate: Boolean;
begin
  Result := FInvalid;
end;

procedure TLuiBarPatterns.SetHover(const AValue: TCairoPattern);
begin
  //if FHover=AValue then exit;
  FHover.Free;
  FHover := AValue;
end;

procedure TLuiBarPatterns.SetNormal(const AValue: TCairoPattern);
begin
  //if FNormal=AValue then exit;
  FNormal.Free;
  FNormal := AValue;
end;

procedure TLuiBarPatterns.SetOutLine(const AValue: TCairoPattern);
begin
  //if FOutLine=AValue then exit;
  FOutLine.Free;
  FOutLine := AValue;
end;

procedure TLuiBarPatterns.SetSelected(const AValue: TCairoPattern);
begin
  //if FSelected=AValue then exit;
  FSelected.Free;
  FSelected := AValue;
end;

procedure TLuiBarPatterns.SetSelectedText(const AValue: TCairoPattern);
begin
  //if FSelectedText=AValue then exit;
  FSelectedText.Free;
  FSelectedText := AValue;
end;

procedure TLuiBarPatterns.SetText(const AValue: TCairoPattern);
begin
  //if FText=AValue then exit;
  FText.Free;
  FText := AValue;
end;

constructor TLuiBarPatterns.Create;
begin
  FInvalid := True;
  FFixedValues := True;
end;

destructor TLuiBarPatterns.Destroy;
begin
  FNormal.Free;
  FSelected.Free;
  FHover.Free;
  FText.Free;
  FOutLine.Free;
  FBackGround.Free;
end;

procedure TLuiBarPatterns.Invalidate;
begin
  if not FFixedValues then
    FInvalid := True
end;

procedure TLuiBarPatterns.Updated;
begin
  FInvalid := False;
end;

end.

