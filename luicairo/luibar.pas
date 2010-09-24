unit LuiBar;

{$mode objfpc}{$H+}
{.$define DEBUG_LUIBAR}

interface

uses
  Classes, SysUtils, CairoClasses, LuiCairoControls, CairoLCL, Types, Controls, Cairo,
  Math, Graphics, GraphType, Maps;

type

  TCustomLuiBar = class;
  
  TLuiBarCell = class;
  
  TLuiBarOption = (
    lboEmulateTab,
    lboHoverAsSelected,
    lboVariableCellWidth,
    lboHotTrack,
    lboOutLineClientArea,
    lboOmitBaseLine,
    lboCenterImage,
    lboTransitorySelect
    );
  
  TLuiBarDrawType = (dtCell, dtCellPath, dtCellText, dtBackground);

  TLuiBarOptions = set of TLuiBarOption;
  
  TLuiBarPosition = (poTop, poLeft, poBottom, poRight);
  
  TLuiBarImagePosition = (ipTop, ipLeft, ipBottom, ipRight);
  
  TLuiBarCellAlign = (caDefault, caInvert, caCenter);
  
  TLuiBarTextAlign = (taCenter, taLeft, taRight);
  
  TLuiBarEvent = procedure (Sender: TCustomLuiBar) of object;

  TLuiBarSelectingEvent = procedure (Sender: TCustomLuiBar; OldCell, NewCell: Integer;
    var Allowed: Boolean) of object;
  
  TLuiBarDrawCellEvent = procedure (Sender: TCustomLuiBar; Cell: TLuiBarCell) of object;

  TLuiBarDrawingEvent = procedure (Sender: TCustomLuiBar; Cell: TLuiBarCell;
    DrawType: TLuiBarDrawType; var Allowed: Boolean) of object;
  
  TLuiBarPatternType = (ptSelected, ptNormal, ptHover, ptText, ptSelectedText,
    ptBackground, ptOutLine, ptClientArea);
  
  TLuiBarCreatePattern = procedure (Sender: TCustomLuiBar; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern) of object;
  
  TLuiBarGetCellPattern = procedure (Sender: TCustomLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern) of object;
  
  TLuiBarImageInfo = record
    Index: Integer;
    Effect: TGraphicsDrawEffect;
  end;

  TLuiBarGetImageInfo = procedure (Sender: TCustomLuiBar; Cell: TLuiBarCell;
    var ImageInfo: TLuiBarImageInfo) of object;

  { TLuiBarColors }

  TLuiBarColors = class(TPersistent)
  private
    FOwner: TCustomLuiBar;
    FColors: array[0..7] of TColor;
    function GetColor(const Index: Integer): TColor; inline;
    procedure SetColor(const Index: Integer; const Value: TColor);
  public
    constructor Create(AOwner: TCustomLuiBar);
    procedure Assign(Source: TPersistent); override;
  published
    property Normal: TColor index 0 read GetColor write SetColor default clSkyBlue;
    property Selected: TColor index 1 read GetColor write SetColor default clBlue;
    property Hover: TColor index 2 read GetColor write SetColor default clBlue;
    property Text: TColor index 3 read GetColor write SetColor default clBlack;
    property SelectedText: TColor index 4 read GetColor write SetColor default clWhite;
    property OutLine: TColor index 5 read GetColor write SetColor default clWhite;
    property Background: TColor index 6 read GetColor write SetColor default clWhite;
    property ClientArea: TColor index 7 read GetColor write SetColor default clBlue;
  end;
  
  { TLuiBarPatterns }

  TLuiBarPatterns = class
  private
    FBackGround: TCairoPattern;
    FClientArea: TCairoPattern;
    FHover: TCairoPattern;
    FNormal: TCairoPattern;
    FOutLine: TCairoPattern;
    FSelected: TCairoPattern;
    FSelectedText: TCairoPattern;
    FText: TCairoPattern;
    FPatternMap: TMap;
    FInvalid: Boolean;
    FFixedValues: Boolean;
    procedure FreePatternMap;
    function GetIndexed(Index: Integer): TCairoPattern;
    function GetRequiresUpdate: Boolean;
    procedure PatternMapNeeded;
    procedure SetBackGround(const AValue: TCairoPattern);
    procedure SetClientArea(const AValue: TCairoPattern);
    procedure SetHover(const AValue: TCairoPattern);
    procedure SetIndexed(Index: Integer; const AValue: TCairoPattern);
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
    property RequiresUpdate: Boolean read GetRequiresUpdate;
    property FixedValues: Boolean read FFixedValues write FFixedValues;
    //patterns
    property BackGround: TCairoPattern read FBackGround write SetBackGround;
    property ClientArea: TCairoPattern read FClientArea write SetClientArea;
    property Hover: TCairoPattern read FHover write SetHover;
    property Normal: TCairoPattern read FNormal write SetNormal;
    property OutLine: TCairoPattern read FOutLine write SetOutLine;
    property Text: TCairoPattern read FText write SetText;
    property Selected: TCairoPattern read FSelected write SetSelected;
    property SelectedText: TCairoPattern read FSelectedText write SetSelectedText;
    //User defined
    property Indexed[Index: Integer]: TCairoPattern read GetIndexed write SetIndexed; default;
  end;
  
  { TLuiBarCell }

  TLuiBarCell = class
  private
    FBounds: TRect;
    FIndex: Integer;
    FPatterns: TLuiBarPatterns;
    FTitle: String;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    UserData: Pointer;
    constructor Create;
    destructor Destroy; override;
    property Height: Integer read GetHeight;
    property Index: Integer read FIndex write FIndex;
    property Patterns: TLuiBarPatterns read FPatterns write FPatterns;
    property Title: String read FTitle write FTitle;
    property Bounds: TRect read FBounds write FBounds;
    property Width: Integer read GetWidth;
  end;

  { TLuiBarCellList }

  TLuiBarCellList = class
  private
    FList: TFpList;
    FOwner: TCustomLuiBar;
    FRequiresUpdate: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TLuiBarCell; inline;
  protected
    property RequiresUpdate: Boolean read FRequiresUpdate write FRequiresUpdate;
  public
    constructor Create(Owner: TCustomLuiBar);
    destructor Destroy; override;
    function Add(const Title: String): TLuiBarCell;
    procedure Clear;
    procedure UpdateCellBounds;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLuiBarCell read GetItems; default;
  end;
  
  { TCustomLuiBar }

  TCustomLuiBar = class(TLuiContainer)
  private
    FCellAlign: TLuiBarCellAlign;
    FCellHeight: Integer;
    FCells: TLuiBarCellList;
    FCellWidth: Integer;
    FClientBounds: TRect;
    FColors: TLuiBarColors;
    FImagePadding: Integer;
    FImagePosition: TLuiBarImagePosition;
    FImages: TImageList;
    FInitialSpace: Integer;
    FOnAfterDraw: TLuiBarEvent;
    FOnDrawBackground: TLuiBarEvent;
    FOnDrawCell: TLuiBarDrawCellEvent;
    FOnDrawCellPath: TLuiBarDrawCellEvent;
    FOnDrawCellText: TLuiBarDrawCellEvent;
    FOnDrawing: TLuiBarDrawingEvent;
    FOnGetCellPattern: TLuiBarGetCellPattern;
    FOnGetImageInfo: TLuiBarGetImageInfo;
    FOnSelecting: TLuiBarSelectingEvent;
    FOuterOffset: Integer;
    FPatterns: TLuiBarPatterns;
    FOnCreatePattern: TLuiBarCreatePattern;
    FOnSelect: TLuiBarEvent;
    FOptions: TLuiBarOptions;
    FCellRoundRadius: Integer;
    FOutLineWidth: Integer;
    FPosition: TLuiBarPosition;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;
    FSpacing: Integer;
    FTextAlign: TLuiBarTextAlign;
    FTextPadding: Integer;
    function CellInPos(X, Y: Integer): Integer;
    function GetAlignOffset(CellSize, ControlSize: Integer): Integer;
    function GetRealCellWidth: Integer;
    function GetTextWidth(const AText: String): Double;
    function IndexToPatternType(Index: Integer): TLuiBarPatternType;
    procedure SetCellAlign(const AValue: TLuiBarCellAlign);
    procedure SetCellHeight(const AValue: Integer);
    procedure SetCellRoundRadius(const AValue: Integer);
    procedure SetCellWidth(const AValue: Integer);
    procedure SetColors(const AValue: TLuiBarColors);
    procedure SetImagePadding(const AValue: Integer);
    procedure SetImagePosition(const AValue: TLuiBarImagePosition);
    procedure SetImages(const AValue: TImageList);
    procedure SetInitialSpace(const AValue: Integer);
    procedure SetOptions(const AValue: TLuiBarOptions);
    procedure SetOuterOffset(const AValue: Integer);
    procedure SetOutLineWidth(const AValue: Integer);
    procedure SetPosition(const AValue: TLuiBarPosition);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetSpacing(const AValue: Integer);
    procedure SetTextAlign(const AValue: TLuiBarTextAlign);
    procedure SetTextPadding(const AValue: Integer);
  protected
    procedure DefaultDrawCell(Cell: TLuiBarCell);
    procedure DefaultDrawCellPath(Cell: TLuiBarCell);
    procedure DefaultDrawCellText(Cell: TLuiBarCell);
    procedure DoAfterDraw; virtual;
    function DoCalculateCellWidth(Cell: TLuiBarCell): Integer;
    procedure DoDraw; override;
    function DoDrawing(Cell: TLuiBarCell; DrawType: TLuiBarDrawType): Boolean; virtual;
    procedure DoDrawBackground; virtual;
    procedure DoDrawCell(Cell: TLuiBarCell); virtual;
    procedure DoDrawCellPath(Cell: TLuiBarCell); virtual;
    procedure DoDrawCellText(Cell: TLuiBarCell); virtual;
    procedure DoDrawClientArea;
    function DoGetCellPattern(Cell: TLuiBarCell; PatternType: TLuiBarPatternType
      ): TCairoPattern; virtual;
    function DoGetImageInfo(Cell: TLuiBarCell): TLuiBarImageInfo; virtual;
    procedure DoOnResize; override;
    procedure DoSelect; virtual;
    function DoSelecting(OldCell, NewCell: Integer): Boolean; virtual;
    procedure DoUpdatePatterns; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cells: TLuiBarCellList read FCells;
    property CellAlign: TLuiBarCellAlign read FCellAlign write SetCellAlign;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 20;
    property CellRoundRadius: Integer read FCellRoundRadius write SetCellRoundRadius;
    property CellWidth: Integer read FCellWidth write SetCellWidth;
    property ClientBounds: TRect read FClientBounds;
    property Colors: TLuiBarColors read FColors write SetColors;
    property Context;
    property HoverIndex: Integer read FHoverIndex;
    property ImagePadding: Integer read FImagePadding write SetImagePadding default 3;
    property ImagePosition: TLuiBarImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property Images: TImageList read FImages write SetImages;
    property InitialSpace: Integer read FInitialSpace write SetInitialSpace;
    property Options: TLuiBarOptions read FOptions write SetOptions;
    property OuterOffset: Integer read FOuterOffset write SetOuterOffset;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Patterns: TLuiBarPatterns read FPatterns;
    property Position: TLuiBarPosition read FPosition write SetPosition;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default -1;
    property Spacing: Integer read FSpacing write SetSpacing;
    property TextAlign: TLuiBarTextAlign read FTextAlign write SetTextAlign default taCenter;
    property TextPadding: Integer read FTextPadding write SetTextPadding default 8;
    //events
    property OnAfterDraw: TLuiBarEvent read FOnAfterDraw write FOnAfterDraw;
    property OnDrawBackground: TLuiBarEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawCell: TLuiBarDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawCellPath: TLuiBarDrawCellEvent read FOnDrawCellPath write FOnDrawCellPath;
    property OnDrawCellText: TLuiBarDrawCellEvent read FOnDrawCellText write FOnDrawCellText;
    property OnDrawing: TLuiBarDrawingEvent read FOnDrawing write FOnDrawing;
    property OnGetImageInfo: TLuiBarGetImageInfo read FOnGetImageInfo write FOnGetImageInfo;
    property OnCreatePattern: TLuiBarCreatePattern read FOnCreatePattern write FOnCreatePattern;
    property OnGetCellPattern: TLuiBarGetCellPattern read FOnGetCellPattern write FOnGetCellPattern;
    property OnSelect: TLuiBarEvent read FOnSelect write FOnSelect;
    property OnSelecting: TLuiBarSelectingEvent read FOnSelecting write FOnSelecting;
  end;

  TLuiBar = class (TCustomLuiBar)
  published
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
    property Patterns;
    property Position;
    property SelectedIndex;
    property Spacing;
    property TextAlign;
    property TextPadding;
    //events
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
    property Align;
    property BorderSpacing;
    property OnCreateContext;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation

uses
  {$ifdef DEBUG_LUIBAR}sharedlogger,{$endif}
  CairoUtils;

{ TLuiBarCellList }

function TLuiBarCellList.GetItems(Index: Integer): TLuiBarCell;
begin
  Result := TLuiBarCell(FList[Index]);
end;

constructor TLuiBarCellList.Create(Owner: TCustomLuiBar);
begin
  FList := TFPList.Create;
  FOwner := Owner;
end;

function TLuiBarCellList.GetCount: Integer;
begin
  Result := FList.Count;
end;

destructor TLuiBarCellList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

function TLuiBarCellList.Add(const Title: String): TLuiBarCell;
begin
  Result := TLuiBarCell.Create;
  Result.Title := Title;
  Result.Index := FList.Count;
  FList.Add(Result);
  FRequiresUpdate := True;
end;

procedure TLuiBarCellList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TLuiBarCell(FList[i]).Destroy;
  FList.Clear;
end;

procedure TLuiBarCellList.UpdateCellBounds;
var
  i, NextLeft, NextTop, AlignOffset: Integer;
  Cell: TLuiBarCell;
begin
  //todo: move to TLuiBar
  if FList.Count = 0 then
  begin
    FOwner.FClientBounds := FOwner.ClientRect;
    Exit;
  end;
  case FOwner.Position of
    poTop:
      begin
        NextLeft := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.FBounds.Left := NextLeft;
          Cell.FBounds.Top := FOwner.OuterOffset;
          Cell.FBounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.FBounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Right, FOwner.Width);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].FBounds, AlignOffset, 0);
        end;
        FOwner.FClientBounds.Top := FOwner.OuterOffset + FOwner.CellHeight;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
    poLeft:
      begin
        NextTop := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.FBounds.Top := NextTop;
          Cell.FBounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.FBounds.Left := FOwner.OuterOffset;
          Cell.FBounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Bottom, FOwner.Height);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].FBounds, 0, AlignOffset);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := Cell.Bounds.Right; //hack
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
    poRight:
      begin
        NextTop := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.FBounds.Top := NextTop;
          Cell.FBounds.Right := FOwner.Width - FOwner.OuterOffset;
          Cell.FBounds.Left := Cell.Bounds.Right - FOwner.DoCalculateCellWidth(Cell);
          Cell.FBounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Bottom, FOwner.Height);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].FBounds, 0, AlignOffset);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width - (FOwner.OuterOffset +
          FOwner.DoCalculateCellWidth(Cell));
      end;
    poBottom:
      begin
        NextLeft := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.FBounds.Left := NextLeft;
          Cell.FBounds.Bottom := FOwner.Height - FOwner.OuterOffset;
          Cell.FBounds.Top := Cell.Bounds.Bottom - FOwner.CellHeight;
          Cell.FBounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Right, FOwner.Width);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].FBounds, AlignOffset, 0);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height - (FOwner.CellHeight + FOwner.OuterOffset);
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
  end;
  {$ifdef DEBUG_LUIBAR}Logger.Send('LuiBar - ClientArea', FOwner.ClientBounds);{$endif}
  FRequiresUpdate := False;
end;

{ TLuiBarCell }

function TLuiBarCell.GetWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

constructor TLuiBarCell.Create;
begin
  FPatterns := TLuiBarPatterns.Create;
end;

function TLuiBarCell.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

destructor TLuiBarCell.Destroy;
begin
  FPatterns.Destroy;
end;

{ TCustomLuiBar }

function TCustomLuiBar.CellInPos(X, Y: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCells.Count - 1 do
    if PosInRect(FCells[i].Bounds, X, Y) then
      Exit(i);
end;

function TCustomLuiBar.GetAlignOffset(CellSize, ControlSize: Integer): Integer;
begin
  case FCellAlign of
    caDefault:
      Result := FInitialSpace;
    caInvert:
      Result := ControlSize - (CellSize + FInitialSpace);
    caCenter:
      Result := (ControlSize - CellSize) div 2;
  end;
end;

function TCustomLuiBar.DoDrawing(Cell: TLuiBarCell; DrawType: TLuiBarDrawType
  ): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawing) then
    FOnDrawing(Self, Cell, DrawType, Result);
end;

function TCustomLuiBar.GetRealCellWidth: Integer;
begin
  if FCellWidth > 0 then
    Result := FCellWidth
  else
  begin
    if FPosition in [poRight, poLeft] then
      Result := Width - FOuterOffset
    else
      Result := (Width - FInitialSpace - (FSpacing * (FCells.Count - 1) )) div FCells.Count;
  end;
end;

function TCustomLuiBar.GetTextWidth(const AText: String): Double;
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    TextExtents(AText, @Extents);
    Result := Extents.width;
  end;
end;

function TCustomLuiBar.IndexToPatternType(Index: Integer): TLuiBarPatternType;
begin
  if Index = FSelectedIndex then
    Result := ptSelected
  else
    if Index = FHoverIndex then
      Result := ptHover
    else
      Result := ptNormal;
end;

procedure TCustomLuiBar.SetCellAlign(const AValue: TLuiBarCellAlign);
begin
  if FCellAlign = AValue then
    Exit;
  FCellAlign := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetCellHeight(const AValue: Integer);
begin
  if FCellHeight = AValue then
    Exit;
  FCellHeight := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetCellRoundRadius(const AValue: Integer);
begin
  if FCellRoundRadius = AValue then
    Exit;
  FCellRoundRadius := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetCellWidth(const AValue: Integer);
begin
  if FCellWidth = AValue then
    Exit;
  FCellWidth := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetColors(const AValue: TLuiBarColors);
begin
  FColors.Assign(AValue);
end;

procedure TCustomLuiBar.SetImagePadding(const AValue: Integer);
begin
  if FImagePadding = AValue then
    Exit;
  FImagePadding := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetImagePosition(const AValue: TLuiBarImagePosition);
begin
  if FImagePosition = AValue then
    Exit;
  FImagePosition := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetImages(const AValue: TImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
end;

procedure TCustomLuiBar.SetInitialSpace(const AValue: Integer);
begin
  if FInitialSpace = AValue then
    Exit;
  FInitialSpace := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetOptions(const AValue: TLuiBarOptions);
begin
  if FOptions = AValue then
    Exit;
  FOptions := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetOuterOffset(const AValue: Integer);
begin
  if FOuterOffset = AValue then
    Exit;
  FOuterOffset := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetOutLineWidth(const AValue: Integer);
begin
  if FOutLineWidth = AValue then
    Exit;
  FOutLineWidth := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetPosition(const AValue: TLuiBarPosition);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Changed;
end;

procedure TCustomLuiBar.DoUpdatePatterns;

  function DoGetPattern(AType: TLuiBarPatternType; DefaultColor: TColor): TCairoPattern;
  begin
    Result := nil;
    if Assigned(FOnCreatePattern) then
      FOnCreatePattern(Self, AType, Result);
    if Result = nil then
      Result := TCairoSolidPattern.Create(ColorToCairoColor(DefaultColor));
  end;
  
begin
  with FPatterns do
  begin
    //todo: find a way to set default similar patterns (ClientArea = Selected)
    Normal := DoGetPattern(ptNormal, FColors.Normal);
    Selected := DoGetPattern(ptSelected, FColors.Selected);
    ClientArea := DoGetPattern(ptClientArea, FColors.ClientArea);
    Hover := DoGetPattern(ptHover, FColors.Hover);
    BackGround := DoGetPattern(ptBackground, FColors.Background);
    Text := DoGetPattern(ptText, FColors.Text);
    OutLine := DoGetPattern(ptOutLine, FColors.OutLine);
    SelectedText := DoGetPattern(ptSelectedText, FColors.SelectedText);
    Updated;
  end;
end;

procedure TCustomLuiBar.DoDrawCellPath(Cell: TLuiBarCell);
begin
  if DoDrawing(Cell, dtCellPath) then
    DefaultDrawCellPath(Cell);
  if Assigned(FOnDrawCellPath) then
    FOnDrawCellPath(Self, Cell);
end;

procedure TCustomLuiBar.DoDrawCellText(Cell: TLuiBarCell);
begin
  if DoDrawing(Cell, dtCellText) then
    DefaultDrawCellText(Cell);
  if Assigned(FOnDrawCellText) then
    FOnDrawCellText(Self, Cell);
end;

procedure TCustomLuiBar.DoDrawClientArea;
var
  LineOffset: Double;

  procedure DrawBaseLineSkipCell(Cell: TLuiBarCell);
  begin
    with Context do
    begin
      case FPosition of
        poTop:
          begin
            LineTo(Cell.Bounds.Left, FClientBounds.Top + LineOffset);
            Stroke;
            MoveTo(Cell.Bounds.Right, FClientBounds.Top + LineOffset);
          end;
        poLeft:
          begin
            LineTo(FClientBounds.Left + LineOffset, Cell.Bounds.Top);
            Stroke;
            MoveTo(FClientBounds.Left + LineOffset, Cell.Bounds.Bottom);
          end;
        poRight:
          begin
            LineTo(FClientBounds.Right - LineOffset, Cell.Bounds.Top);
            Stroke;
            MoveTo(FClientBounds.Right - LineOffset, Cell.Bounds.Bottom);
          end;
        poBottom:
          begin
            LineTo(Cell.Bounds.Left, FClientBounds.Bottom - LineOffset);
            Stroke;
            MoveTo(Cell.Bounds.Right, FClientBounds.Bottom - LineOffset);
          end;
      end;
    end;
  end;


var
  SkipCells: array[0..1] of Integer = (-1, -1);

begin
  with Context do
  begin
    //fill the client area avoiding to overlap the cells
    //todo: do this before the cell draw ?
    //      or draw the base line at cell area instead of client area as is today?
    Save;
    Translate(FClientBounds.Left, FClientBounds.Top);
    case FPosition of
      poTop:
        Rectangle(0, FOutLineWidth, FClientBounds.Right - FClientBounds.Left,
          FClientBounds.Bottom - FClientBounds.Top - FOutLineWidth);
      poLeft:
        Rectangle(FOutLineWidth, 0, FClientBounds.Right - FClientBounds.Left - FOutLineWidth,
          FClientBounds.Bottom - FClientBounds.Top);
      poRight:
        Rectangle(0, 0, FClientBounds.Right - FClientBounds.Left - FOutLineWidth,
          FClientBounds.Bottom - FClientBounds.Top);
      poBottom:
        Rectangle(0, 0, FClientBounds.Right - FClientBounds.Left,
          FClientBounds.Bottom - FClientBounds.Top - FOutLineWidth);
    end;
    Source := FPatterns.ClientArea;
    Fill;
    Restore;

    //Draw base outline
    Save;
    if not (lboOmitBaseLine in FOptions) then
    begin
      //todo: handle when there's no client area
      //todo: make lineoffset a field
      LineOffset := GetSharpLineOffset(FOutLineWidth);

      LineWidth := FOutLineWidth;
      Source := FPatterns.OutLine;

      case FPosition of
        poTop:
          MoveTo(0, FClientBounds.Top + LineOffset);
        poLeft:
          MoveTo(FClientBounds.Left + LineOffset, 0);
        poRight:
          MoveTo(FClientBounds.Right - LineOffset, 0);
        poBottom:
          MoveTo(0, FClientBounds.Bottom - LineOffset);
      end;

      if (lboHoverAsSelected in FOptions) and
        (FHoverIndex <> -1) and (FSelectedIndex <> FHoverIndex) then
      begin
        SkipCells[0] := Min(FSelectedIndex, FHoverIndex);
        SkipCells[1] := Max(FSelectedIndex, FHoverIndex);
      end
      else
        SkipCells[0] := FSelectedIndex;
      if SkipCells[0] <> -1 then
        DrawBaseLineSkipCell(FCells[SkipCells[0]]);

      if SkipCells[1] <> -1 then
        DrawBaseLineSkipCell(FCells[SkipCells[1]]);

    end;
    //todo: move this to DoDrawClientArea
    case FPosition of
      poTop:
        begin
          if lboOutLineClientArea in FOptions then
          begin
            LineTo(Width - LineOffset, FClientBounds.Top + LineOffset);
            LineTo(Width - LineOffset, Height - LineOffset);
            LineTo(LineOffset, Height - LineOffset);
            LineTo(LineOffset, FClientBounds.Top + LineOffset);
          end
          else
            LineTo(Width, FClientBounds.Top + LineOffset);
        end;
      poLeft:
        begin
          if lboOutLineClientArea in FOptions then
          begin
            LineTo(FClientBounds.Left + LineOffset, Height - LineOffset);
            LineTo(Width - LineOffset, Height - LineOffset);
            LineTo(Width - LineOffset, LineOffset);
            LineTo(FClientBounds.Left + LineOffset, LineOffset);
          end
          else
            LineTo(FClientBounds.Left + LineOffset, Height);
        end;
      poRight:
        begin
          if lboOutLineClientArea in FOptions then
          begin
            LineTo(FClientBounds.Right - LineOffset, Height - LineOffset);
            LineTo(LineOffset, Height - LineOffset);
            LineTo(LineOffset, LineOffset);
            LineTo(FClientBounds.Right - LineOffset, LineOffset);
          end
          else
            LineTo(FClientBounds.Right - LineOffset, Height);
        end;
      poBottom:
        begin
          if lboOutLineClientArea in FOptions then
          begin
            LineTo(Width - LineOffset, FClientBounds.Bottom - LineOffset);
            LineTo(Width - LineOffset, LineOffset);
            LineTo(LineOffset, LineOffset);
            LineTo(LineOffset, FClientBounds.Bottom - LineOffset);
          end
          else
            LineTo(Width, FClientBounds.Bottom - LineOffset);
        end;
    end;
    Stroke;
    Restore;
  end;
end;

function TCustomLuiBar.DoGetCellPattern(Cell: TLuiBarCell; PatternType: TLuiBarPatternType): TCairoPattern;
begin
  //todo: see if is necessary this mechanism
  Result := nil;
  if Assigned(FOnGetCellPattern) then
    FOnGetCellPattern(Self, Cell, PatternType, Result);
  if Result = nil then
  begin
    case PatternType of
      ptNormal:
      begin
        if Cell.Patterns.Normal = nil then
          Result := FPatterns.Normal
        else
          Result := Cell.Patterns.Normal;
      end;
      ptHover:
      begin
        if lboHoverAsSelected in FOptions then
          Result := DoGetCellPattern(Cell, ptSelected)
        else
        begin
          if Cell.Patterns.Hover = nil then
            Result := FPatterns.Hover
          else
            Result := Cell.Patterns.Hover;
        end;
      end;
      ptSelected:
      begin
        if Cell.Patterns.Selected = nil then
          Result := FPatterns.Selected
        else
          Result := Cell.Patterns.Selected;
      end;
      ptOutLine:
      begin
        if Cell.Patterns.OutLine = nil then
          Result := FPatterns.OutLine
        else
          Result := Cell.Patterns.OutLine;
      end;
      ptText:
      begin
        if Cell.Patterns.Text = nil then
          Result := FPatterns.Text
        else
          Result := Cell.Patterns.Text;
      end;
      ptSelectedText:
      begin
        if Cell.Patterns.SelectedText = nil then
          Result := FPatterns.SelectedText
        else
          Result := Cell.Patterns.SelectedText;
      end;
    end;
  end;
end;

function TCustomLuiBar.DoGetImageInfo(Cell: TLuiBarCell): TLuiBarImageInfo;
begin
  Result.Index := -1;
  Result.Effect := gdeNormal;
  if Assigned(FOnGetImageInfo) then
    FOnGetImageInfo(Self, Cell, Result);
end;

procedure TCustomLuiBar.DoOnResize;
begin
  inherited DoOnResize;
  if FCellAlign in [caInvert, caCenter] then
    FCells.RequiresUpdate := True;
end;

procedure TCustomLuiBar.SetSelectedIndex(const AValue: Integer);
begin
  if FSelectedIndex = AValue then
    Exit;
  if (AValue >= FCells.Count) and not (csLoading in ComponentState) then
    raise Exception.Create('Cell index out of bounds');
  FSelectedIndex := Max(-1, AValue);
  Changed;
end;

procedure TCustomLuiBar.SetSpacing(const AValue: Integer);
begin
  if FSpacing = AValue then
    Exit;
  FSpacing := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetTextAlign(const AValue: TLuiBarTextAlign);
begin
  if FTextAlign = AValue then
    Exit;
  FTextAlign := AValue;
  Changed;
end;

procedure TCustomLuiBar.SetTextPadding(const AValue: Integer);
begin
  if FTextPadding = AValue then
    Exit;
  FTextPadding := AValue;
  Changed;
end;

procedure TCustomLuiBar.DoAfterDraw;
begin
  if Assigned(FOnAfterDraw) then
    FOnAfterDraw(Self);
end;

function TCustomLuiBar.DoCalculateCellWidth(Cell: TLuiBarCell): Integer;
begin
  if FPosition in [poTop, poBottom] then
  begin
    if lboVariableCellWidth in FOptions then
    begin
      Result := Round(GetTextWidth(Cell.Title));
      if (FImages <> nil) and (DoGetImageInfo(Cell).Index >= 0) then
      begin
        //ignore FTextPadding if the Title is ''
        if Result <> 0 then
          Inc(Result, FTextPadding);
        Inc(Result, FImages.Width + 2 * FImagePadding);
      end
      else
        Inc(Result, 2 * FTextPadding)
    end
    else
    begin
      Result := GetRealCellWidth;
      //align the last cell
      if (FCellWidth = 0) and (Cell.Index = FCells.Count - 1) then
        Inc(Result, (Width - FInitialSpace - (FSpacing * (FCells.Count - 1) )) mod FCells.Count);
    end;
  end
  else
  begin
    Result := GetRealCellWidth;
  end;
end;

procedure TCustomLuiBar.DoDraw;
var
  i: Integer;
  Cell: TLuiBarCell;
begin
  if FPatterns.RequiresUpdate then
    DoUpdatePatterns;
  if FCells.RequiresUpdate then
    FCells.UpdateCellBounds;
  DoDrawBackground;
  for i := 0 to FCells.Count - 1 do
  with Context do
  begin
    Cell := FCells[i];
    Save;
    Translate(Cell.Bounds.Left, Cell.Bounds.Top);
    DoDrawCell(Cell);
    DoDrawCellText(Cell);
    Restore;
  end;
  if lboEmulateTab in FOptions then
    DoDrawClientArea;
  inherited DoDraw;
  DoAfterDraw;
end;

procedure TCustomLuiBar.DoDrawCell(Cell: TLuiBarCell);
begin
  if DoDrawing(Cell, dtCell) then
    DefaultDrawCell(Cell);
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, Cell);
end;

procedure TCustomLuiBar.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

function TCustomLuiBar.DoSelecting(OldCell, NewCell: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnSelecting) then
    FOnSelecting(Self, OldCell, NewCell, Result);
end;

procedure TCustomLuiBar.DoDrawBackground;
begin
  with Context do
  begin
    Save;
    if DoDrawing(nil, dtBackground) then
    begin
      Source := FPatterns.Background;
      Rectangle(0, 0, Width, Height);
      Fill;
    end;
    if Assigned(FOnDrawBackground) then
      FOnDrawBackground(Self);
    Restore;
  end;
end;
procedure TCustomLuiBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClickedCell: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedCell := CellInPos(X,Y);
    if (ClickedCell <> -1) and (ClickedCell <> FSelectedIndex) then
    begin
      if DoSelecting(FSelectedIndex, ClickedCell) then
      begin
        FSelectedIndex := ClickedCell;
        DoSelect;
        if lboTransitorySelect in FOptions then
          FSelectedIndex := -1
        else
          Redraw;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomLuiBar.MouseLeave;
begin
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Redraw;
  end;
  inherited MouseLeave;
end;

procedure TCustomLuiBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHoverIndex: Integer;
begin
  if lboHotTrack in FOptions then
  begin
    NewHoverIndex := CellInPos(X, Y);
    if NewHoverIndex <> FHoverIndex then
    begin
      FHoverIndex := NewHoverIndex;
      Redraw;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

constructor TCustomLuiBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCells := TLuiBarCellList.Create(Self);
  FPatterns := TLuiBarPatterns.Create;
  FCellHeight := 20;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  FTextPadding := 8;
  FTextAlign := taCenter;
  FImagePadding := 3;
  FImagePosition := ipLeft;
  FColors := TLuiBarColors.Create(Self);
  SetInitialBounds(0, 0, 100, 30);
end;

procedure TCustomLuiBar.DefaultDrawCell(Cell: TLuiBarCell);
begin
  with Context do
  begin
    Source := DoGetCellPattern(Cell, IndexToPatternType(Cell.Index));
    {
    I've found a cleaner, smaller, faster way to resolve this
    
    //due to cairo singularity regarding linewidth and stroke, the current approach
    //to get a sharp outline leads to a gap between the the selected cell and the client area
    //this may look a hack but is the smallest posiible hack until now. Believe me.
    if (lboEmulateTab in FOptions) and
      ((Cell.Index = FSelectedIndex) or (lboOmitBaseLine in FOptions) or
      ((lboHoverAsSelected in FOptions) and (Cell.Index = FHoverIndex))) then
      FillCellClientGap(Cell);
    }
    DoDrawCellPath(Cell);
    FillPreserve;
    LineWidth := FOutLineWidth;
    Source := DoGetCellPattern(Cell, ptOutLine);
    Stroke;
  end;
end;

procedure TCustomLuiBar.DefaultDrawCellPath(Cell: TLuiBarCell);
var
  InnerRadius: Integer;
  R: TDoubleRect;
  LineOffset: Double;
begin
  if lboEmulateTab in FOptions then
    InnerRadius := 0
  else
    InnerRadius := FCellRoundRadius;
  CalculateSharpBounds(0, 0, Cell.Width, Cell.Height, FOutLineWidth, R);
  LineOffset := GetSharpLineOffset(FOutLineWidth);
  //todo: use matrix manipulation to consolidate the procedures??
  case FPosition of
    poTop:
      with Context do
      begin
        //this is necessary to avoid a gap between cell and client area
        if lboEmulateTab in FOptions then
          R.Bottom := R.Bottom + LineOffset + FOutLineWidth;
        MoveTo(R.Left + InnerRadius, R.Bottom);
        CurveTo(R.Left + InnerRadius, R.Bottom,
          R.Left, R.Bottom,
          R.Left, R.Bottom - InnerRadius);
        LineTo(R.Left, R.Top + FCellRoundRadius);
        CurveTo(R.Left, R.Top + FCellRoundRadius,
          R.Left, R.Top,
          R.Left + FCellRoundRadius, R.Top);
        LineTo(R.Right - FCellRoundRadius, R.Top);
        CurveTo(R.Right - FCellRoundRadius, R.Top,
          R.Right, R.Top,
          R.Right, R.Top + FCellRoundRadius);
        LineTo(R.Right, R.Bottom - InnerRadius);
        CurveTo(R.Right, R.Bottom - InnerRadius,
          R.Right, R.Bottom,
          R.Right - InnerRadius, R.Bottom);
      end;
    poBottom:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Top := R.Top - LineOffset - FOutLineWidth;
        MoveTo(R.Left + InnerRadius, R.Top);
        CurveTo(R.Left + InnerRadius, R.Top,
          R.Left, R.Top,
          R.Left, R.Top + InnerRadius);
        LineTo(R.Left, R.Bottom - FCellRoundRadius);
        CurveTo(R.Left, R.Bottom - FCellRoundRadius,
          R.Left, R.Bottom,
          R.Left + FCellRoundRadius, R.Bottom);
        LineTo(R.Right - FCellRoundRadius, R.Bottom);
        CurveTo(R.Right - FCellRoundRadius, R.Bottom,
          R.Right, R.Bottom,
          R.Right, R.Bottom - FCellRoundRadius);
        LineTo(R.Right, R.Top + InnerRadius);
        CurveTo(R.Right, R.Top + InnerRadius,
          R.Right, R.Top,
          R.Right - InnerRadius, R.Top);
      end;
    poLeft:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Right := R.Right + FOutLineWidth + LineOffset;
        MoveTo(R.Right, R.Top + InnerRadius);
        CurveTo(R.Right, R.Top + InnerRadius,
          R.Right, R.Top,
          R.Right - InnerRadius, R.Top);
        LineTo(R.Left + FCellRoundRadius, R.Top);
        CurveTo(R.Left + FCellRoundRadius, R.Top,
          R.Left, R.Top,
          R.Left, R.Top + FCellRoundRadius);
        LineTo(R.Left, R.Bottom - FCellRoundRadius);
        CurveTo(R.Left, R.Bottom - FCellRoundRadius,
          R.Left, R.Bottom,
          R.Left + FCellRoundRadius, R.Bottom);
        LineTo(R.Right - InnerRadius, R.Bottom);
        CurveTo(R.Right - InnerRadius, R.Bottom,
          R.Right, R.Bottom,
          R.Right, R.Bottom - InnerRadius);
      end;
    poRight:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Left := R.Left - FOutLineWidth - LineOffset;
        MoveTo(R.Left, R.Top + InnerRadius);
        CurveTo(R.Left, R.Top + InnerRadius,
          R.Left, R.Top,
          R.Left + InnerRadius, R.Top);
        LineTo(R.Right - FCellRoundRadius, R.Top);
        CurveTo(R.Right - FCellRoundRadius, R.Top,
          R.Right, R.Top,
          R.Right, R.Top + FCellRoundRadius);
        LineTo(R.Right, R.Bottom - FCellRoundRadius);
        CurveTo(R.Right, R.Bottom - FCellRoundRadius,
          R.Right, R.Bottom,
          R.Right - FCellRoundRadius, R.Bottom);
        LineTo(R.Left + InnerRadius, R.Bottom);
        CurveTo(R.Left + InnerRadius, R.Bottom,
          R.Left, R.Bottom,
          R.Left, R.Bottom - InnerRadius);
      end;
  end;
  if not (lboEmulateTab in FOptions) then
    Context.ClosePath;
end;

procedure TCustomLuiBar.DefaultDrawCellText(Cell: TLuiBarCell);
const
  TextPatternMap: array[Boolean] of TLuiBarPatternType = (ptText, ptSelectedText);
var
  Extents: cairo_text_extents_t;
  TextWidth: Integer;
  TextHeight: Integer;
  TextBox: TRect;
  EffectiveTextAlign: TLuiBarTextAlign;
  ImagePos: TPoint;
  ImageInfo: TLuiBarImageInfo;
begin
  with Context do
  begin
    Source := DoGetCellPattern(Cell, TextPatternMap[Cell.Index = FSelectedIndex]);
    EffectiveTextAlign := FTextAlign;
    TextExtents(Cell.Title, @Extents);
    TextWidth := Round(Extents.Width);
    TextHeight := Round(Extents.Height);
    ImageInfo := DoGetImageInfo(Cell);
    if (FImages <> nil) and (ImageInfo.Index >= 0) then
    begin
      //todo: compute linewidth
      //todo: refactor into sub routines ?
      case FImagePosition of
      ipLeft:
        begin
          ImagePos.y := (Cell.Height - FImages.Height) div 2;
          if (lboCenterImage in FOptions) and (FTextAlign = taCenter) then
          begin
            ImagePos.x := Cell.Width - FImages.Width;
            //ignore TextWidth if Title = ''
            if TextWidth <> 0 then
              Dec(ImagePos.x, TextWidth + FImagePadding);
            ImagePos.x := ImagePos.x div 2;
            EffectiveTextAlign := taLeft;
          end
          else
          begin
            ImagePos.x := FImagePadding;
          end;
          TextBox := Rect(ImagePos.x + FImages.Width + FImagePadding, 0,
            Cell.Width - FTextPadding, Cell.Height);
        end;
      ipTop:
        begin
          ImagePos.x := (Cell.Width - FImages.Width) div 2;
          if lboCenterImage in FOptions then
          begin
            ImagePos.y := Cell.Height - FImages.Height;
            //ignore TextHeight if Title = ''
            if TextHeight <> 0 then
              Dec(ImagePos.y, TextHeight + FImagePadding);
            ImagePos.y := ImagePos.y div 2;
          end
          else
          begin
            ImagePos.y := FImagePadding;
          end;
          TextBox := Rect(FTextPadding, ImagePos.y + FImages.Height + FImagePadding,
            Cell.Width - FTextPadding, Cell.Height - FTextPadding);
        end;
      ipBottom:
        begin
          ImagePos.x := (Cell.Width - FImages.Width) div 2;
          if lboCenterImage in FOptions then
          begin
            ImagePos.y := Cell.Height - FImages.Height;
            if TextHeight <> 0 then
              Dec(ImagePos.y, TextHeight + FImagePadding);
            ImagePos.y := ImagePos.y div 2;
            Inc(ImagePos.y, TextHeight + FImagePadding)
          end
          else
          begin
            ImagePos.y := Cell.Height - FImages.Height - FImagePadding;
          end;
          TextBox := Rect(FTextPadding, FTextPadding,
            Cell.Width - FTextPadding, ImagePos.y - FImagePadding);
        end;
      ipRight:
        begin
          ImagePos.y := (Cell.Height - FImages.Height) div 2;
          if (lboCenterImage in FOptions) and (FTextAlign = taCenter) then
          begin
            ImagePos.x := Cell.Width - FImages.Width;
            if TextWidth <> 0 then
              Dec(ImagePos.x, TextWidth + FImagePadding);
            ImagePos.x := ImagePos.x div 2;
            Inc(ImagePos.x, TextWidth + FImagePadding);
            EffectiveTextAlign := taRight;
          end
          else
          begin
            ImagePos.x := Cell.Width - FImages.Width - FImagePadding;
          end;
          TextBox := Rect(FTextPadding, 0,
            ImagePos.x - FImagePadding, Cell.Height);
        end;
      end;

      FImages.Draw(Bitmap.Canvas, Cell.Bounds.Left + ImagePos.x,
        Cell.Bounds.Top + ImagePos.y, ImageInfo.Index, ImageInfo.Effect);
    end
    else
    begin
      TextBox := Rect(FTextPadding, 0, Cell.Width - FTextPadding, Cell.Height);
    end;
    //Draw text in the given rectangle
    case EffectiveTextAlign of
      taCenter:
      begin
        with TextBox do
          MoveTo((Right - Left - TextWidth) div 2 + Left,
            (Bottom - Top + TextHeight) div 2 + Top);
      end;
      taLeft:
      begin
        with TextBox do
          MoveTo(Left, (Bottom - Top + TextHeight) div 2 + Top);
      end;
      taRight:
      begin
        with TextBox do
          MoveTo(Right - TextWidth, (Bottom - Top + TextHeight) div 2 + Top);
      end;
    end;

    ShowText(Cell.Title);
  end;
end;

destructor TCustomLuiBar.Destroy;
begin
  FCells.Destroy;
  FColors.Destroy;
  FPatterns.Destroy;
  inherited Destroy;
end;

{ TLuiBarPatterns }

procedure TLuiBarPatterns.SetBackGround(const AValue: TCairoPattern);
begin
  if FBackGround = AValue then
    Exit;
  FBackGround.Free;
  FBackGround := AValue;
end;

procedure TLuiBarPatterns.SetClientArea(const AValue: TCairoPattern);
begin
  if FClientArea = AValue then
    Exit;
  FClientArea.Free;
  FClientArea := AValue;
end;

function TLuiBarPatterns.GetRequiresUpdate: Boolean;
begin
  Result := FInvalid;
end;

procedure TLuiBarPatterns.SetHover(const AValue: TCairoPattern);
begin
  if FHover = AValue then
    Exit;
  FHover.Free;
  FHover := AValue;
end;

procedure TLuiBarPatterns.SetIndexed(Index: Integer; const AValue: TCairoPattern);
var
  OldPattern: TCairoPattern;
begin
  PatternMapNeeded;
  if FPatternMap.GetData(Index, OldPattern) then
  begin
    OldPattern.Free;
    FPatternMap.SetData(Index, AValue);
  end
  else
    FPatternMap.Add(Index, AValue);
end;

procedure TLuiBarPatterns.SetNormal(const AValue: TCairoPattern);
begin
  if FNormal = AValue then
    Exit;
  FNormal.Free;
  FNormal := AValue;
end;

procedure TLuiBarPatterns.SetOutLine(const AValue: TCairoPattern);
begin
  if FOutLine = AValue then
    Exit;
  FOutLine.Free;
  FOutLine := AValue;
end;

procedure TLuiBarPatterns.SetSelected(const AValue: TCairoPattern);
begin
  if FSelected = AValue then
    Exit;
  FSelected.Free;
  FSelected := AValue;
end;

procedure TLuiBarPatterns.SetSelectedText(const AValue: TCairoPattern);
begin
  if FSelectedText = AValue then
    Exit;
  FSelectedText.Free;
  FSelectedText := AValue;
end;

procedure TLuiBarPatterns.SetText(const AValue: TCairoPattern);
begin
  if FText = AValue then
    Exit;
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
  FClientArea.Free;
  FNormal.Free;
  FSelected.Free;
  FHover.Free;
  FText.Free;
  FSelectedText.Free;
  FOutLine.Free;
  FBackGround.Free;
  
  FreePatternMap;
end;

procedure TLuiBarPatterns.FreePatternMap;
var
  Iterator: TMapIterator;
  Pattern: TCairoPattern;
begin
  if FPatternMap = nil then
    Exit;
  Iterator := TMapIterator.Create(FPatternMap);
  with Iterator do
  begin
    while not EOM do
    begin
      GetData(Pattern);
      Pattern.Free;
      Next;
    end;
    Destroy;
  end;
  FPatternMap.Destroy;
end;

function TLuiBarPatterns.GetIndexed(Index: Integer): TCairoPattern;
begin
  if FPatternMap = nil then
    raise Exception.Create('Pattern Map Not Initialized');
  if not FPatternMap.GetData(Index, Result) then
    raise Exception.Create('Pattern with index ' + IntToStr(Index) + ' not found');
end;

procedure TLuiBarPatterns.Invalidate;
begin
  if not FFixedValues then
    FInvalid := True
end;

procedure TLuiBarPatterns.PatternMapNeeded;
begin
  if FPatternMap = nil then
    FPatternMap := TMap.Create(its4, SizeOf(TCairoPattern));
end;

procedure TLuiBarPatterns.Updated;
begin
  FInvalid := False;
end;

{ TLuiBarColors }

function TLuiBarColors.GetColor(const Index: Integer): TColor;
begin
  Result := FColors[Index];
end;

procedure TLuiBarColors.SetColor(const Index: Integer; const Value: TColor);
begin
  if FColors[Index] = Value then
    Exit;
  FColors[Index] := Value;
  FOwner.Changed;
end;

constructor TLuiBarColors.Create(AOwner: TCustomLuiBar);
begin
  FOwner := AOwner;
  //todo: find better initial collors
  FColors[0] := clSkyBlue; //Normal
  FColors[1] := clBlue; //Selected
  FColors[2] := clBlue; //Hover
  FColors[3] := clBlack; //Text
  FColors[4] := clWhite; //SelectedText
  FColors[5] := clWhite; //OutLine
  FColors[6] := clWhite;  //Background
  FColors[7] := clBlue; //ClientArea
end;

procedure TLuiBarColors.Assign(Source: TPersistent);
begin
  if Source is TLuiBarColors then
  begin
    FColors := TLuiBarColors(Source).FColors;
    FOwner.Changed;
  end
  else
    inherited Assign(Source);
end;

end.

