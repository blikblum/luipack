unit LuiBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, types, Controls, Cairo14, math, Graphics;

type

  TLuiBar = class;
  
  TLuiBarCell = class;
  
  TLuiBarOption = (
    lboEmulateTab,
    lboHoverAsSelected,
    lboVariableCellWidth,
    lboHotTrack,
    lboOutLineClientArea,
    lboOmitBaseLine
    );
  
  TLuiBarOptions = set of TLuiBarOption;
  
  TLuiBarPosition = (lbpTop, lbpLeft, lbpBottom, lbpRight);
  
  TLuiBarCellAlign = (lbaDefault, lbaInvert, lbaCenter);
  
  TLuiBarEvent = procedure (Sender: TLuiBar) of object;
  
  TLuiBarDrawCellEvent = procedure (Sender: TLuiBar; Cell: TLuiBarCell) of object;
  
  TLuiBarPatternType = (ptSelected, ptNormal, ptHover, ptText, ptSelectedText,
    ptBackground, ptOutLine, ptClientArea);
  
  TLuiBarGetPattern = procedure (Sender: TLuiBar; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern) of object;
  
  TLuiBarGetCellPattern = procedure (Sender: TLuiBar; Cell: TLuiBarCell; PatternType: TLuiBarPatternType;
    var Pattern: TCairoPattern) of object;
  
  TLuiBarColors = record
    Normal: TColor;
    Selected: TColor;
    Hover: TColor;
    Text: TColor;
    SelectedText: TColor;
    OutLine: TColor;
    Background: TColor;
    ClientArea: TColor;
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
    FInvalid: Boolean;
    FFixedValues: Boolean;
    function GetRequiresUpdate: Boolean;
    procedure SetBackGround(const AValue: TCairoPattern);
    procedure SetClientArea(const AValue: TCairoPattern);
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
  end;
  
  { TLuiBarCell }

  TLuiBarCell = class
  private
    FBounds: TRect;
    FImageIndex: Integer;
    FIndex: Integer;
    FPatterns: TLuiBarPatterns;
    FTitle: String;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Height: Integer read GetHeight;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
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
    FOwner: TLuiBar;
    FRequiresUpdate: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TLuiBarCell; inline;
  public
    constructor Create(Owner: TLuiBar);
    destructor Destroy; override;
    function Add(const Title: String; ImageIndex: Integer = -1): TLuiBarCell;
    procedure Clear;
    procedure UpdateCellBounds;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLuiBarCell read GetItems; default;
    property RequiresUpdate: Boolean read FRequiresUpdate;
  end;
  
  { TLuiBar }

  TLuiBar = class(TCustomCairoControl)
  private
    FCellAlign: TLuiBarCellAlign;
    FCellHeight: Integer;
    FCells: TLuiBarCellList;
    FCellWidth: Integer;
    FClientBounds: TRect;
    FColors: TLuiBarColors;
    FImages: TImageList;
    FInitialSpace: Integer;
    FOnDrawBackground: TLuiBarEvent;
    FOnDrawCell: TLuiBarDrawCellEvent;
    FOnDrawCellPath: TLuiBarDrawCellEvent;
    FOnDrawCellText: TLuiBarDrawCellEvent;
    FOnGetCellPattern: TLuiBarGetCellPattern;
    FOuterOffset: Integer;
    FPatterns: TLuiBarPatterns;
    FOnGetPattern: TLuiBarGetPattern;
    FOnSelect: TLuiBarEvent;
    FOptions: TLuiBarOptions;
    FCellRoundRadius: Integer;
    FOutLineWidth: Integer;
    FPosition: TLuiBarPosition;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;
    FSpacing: Integer;
    function CellInPoint(const P: TPoint): Integer;
//    procedure FillCellClientGap(Cell: TLuiBarCell);
    function GetAlignOffset(CellSize, ControlSize: Integer): Integer;
    function GetRealCellWidth: Integer;
    function GetTextWidth(const AText: String): Double;
    function IndexToPatternType(Index: Integer): TLuiBarPatternType;
    procedure SetCellAlign(const AValue: TLuiBarCellAlign);
    procedure SetImages(const AValue: TImageList);
    procedure SetInitialSpace(const AValue: Integer);
    procedure SetOnGetPattern(const AValue: TLuiBarGetPattern);
    procedure SetOuterOffset(const AValue: Integer);
    procedure SetPosition(const AValue: TLuiBarPosition);
    procedure SetSelectedIndex(const AValue: Integer);
  protected
    function DoCalculateCellWidth(Cell: TLuiBarCell): Integer;
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawCell(Cell: TLuiBarCell); virtual;
    procedure DoDrawCellPath(Cell: TLuiBarCell); virtual;
    procedure DoDrawCellText(Cell: TLuiBarCell); virtual;
    procedure DoDrawClientArea;
    function DoGetCellPattern(Cell: TLuiBarCell; PatternType: TLuiBarPatternType
      ): TCairoPattern; virtual;
    procedure DoSelect; virtual;
    procedure DoUpdatePatterns; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultDrawCell(Cell: TLuiBarCell);
    procedure DefaultDrawCellPath(const Cell: TLuiBarCell);
    procedure DefaultDrawCellText(Cell: TLuiBarCell);
    destructor Destroy; override;
    property Cells: TLuiBarCellList read FCells;
    property CellAlign: TLuiBarCellAlign read FCellAlign write SetCellAlign;
    property CellHeight: Integer read FCellHeight write FCellHeight;
    property CellRoundRadius: Integer read FCellRoundRadius write FCellRoundRadius;
    property CellWidth: Integer read FCellWidth write FCellWidth;
    property ClientBounds: TRect read FClientBounds;
    property Colors: TLuiBarColors read FColors write FColors;
    property Context;
    property HoverIndex: Integer read FHoverIndex;
    property Images: TImageList read FImages write SetImages;
    property InitialSpace: Integer read FInitialSpace write SetInitialSpace;
    property Options: TLuiBarOptions read FOptions write FOptions;
    property OuterOffset: Integer read FOuterOffset write SetOuterOffset;
    property OutLineWidth: Integer read FOutLineWidth write FOutLineWidth;
    property Patterns: TLuiBarPatterns read FPatterns;
    property Position: TLuiBarPosition read FPosition write SetPosition;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Spacing: Integer read FSpacing write FSpacing;
    //events
    property OnDrawBackground: TLuiBarEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawCell: TLuiBarDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawCellPath: TLuiBarDrawCellEvent read FOnDrawCellPath write FOnDrawCellPath;
    property OnDrawCellText: TLuiBarDrawCellEvent read FOnDrawCellText write FOnDrawCellText;
    property OnGetPattern: TLuiBarGetPattern read FOnGetPattern write SetOnGetPattern;
    property OnGetCellPattern: TLuiBarGetCellPattern read FOnGetCellPattern write FOnGetCellPattern;
    property OnSelect: TLuiBarEvent read FOnSelect write FOnSelect;
  published
    property Align;
    property BorderSpacing;
  end;

implementation

uses
  sharedlogger, CairoUtils;

const
  CellTitlePadding = 8;
  
{ TLuiBarCellList }

function TLuiBarCellList.GetItems(Index: Integer): TLuiBarCell;
begin
  Result := TLuiBarCell(FList[Index]);
end;


constructor TLuiBarCellList.Create(Owner: TLuiBar);
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

function TLuiBarCellList.Add(const Title: String; ImageIndex: Integer = -1): TLuiBarCell;
begin
  Result := TLuiBarCell.Create;
  Result.Title := Title;
  Result.Index := FList.Count;
  Result.ImageIndex := ImageIndex;
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
  i, NextLeft, NextTop, NewWidth, AlignOffset: Integer;
  Cell: TLuiBarCell;
begin
  //todo: move to TLuiBar
  case FOwner.Position of
    lbpTop:
      begin
        NextLeft := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Left := NextLeft;
          Cell.Bounds.Top := FOwner.OuterOffset;
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Right, FOwner.Width);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].Bounds, AlignOffset, 0);
        end;
        FOwner.FClientBounds.Top := FOwner.OuterOffset + FOwner.CellHeight;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
    lbpLeft:
      begin
        NextTop := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Top := NextTop;
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.Bounds.Left := FOwner.OuterOffset;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Bottom, FOwner.Height);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].Bounds, 0, AlignOffset);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := Cell.Bounds.Right; //hack
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
    lbpRight:
      begin
        NextTop := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Top := NextTop;
          Cell.Bounds.Right := FOwner.Width - FOwner.OuterOffset;
          Cell.Bounds.Left := Cell.Bounds.Right - FOwner.DoCalculateCellWidth(Cell);
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Bottom, FOwner.Height);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].Bounds, 0, AlignOffset);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height;
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width - (FOwner.OuterOffset +
          FOwner.DoCalculateCellWidth(Cell));
      end;
    lbpBottom:
      begin
        NextLeft := 0;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Left := NextLeft;
          Cell.Bounds.Bottom := FOwner.Height - FOwner.OuterOffset;
          Cell.Bounds.Top := Cell.Bounds.Bottom - FOwner.CellHeight;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
        AlignOffset := FOwner.GetAlignOffset(Items[FList.Count - 1].Bounds.Right, FOwner.Width);
        if AlignOffset > 0 then
        begin
          for i := 0 to FList.Count - 1 do
            OffsetRect(Items[i].Bounds, AlignOffset, 0);
        end;
        FOwner.FClientBounds.Top := 0;
        FOwner.FClientBounds.Bottom := FOwner.Height - (FOwner.CellHeight + FOwner.OuterOffset);
        FOwner.FClientBounds.Left := 0;
        FOwner.FClientBounds.Right := FOwner.Width;
      end;
  end;
  Logger.Send('ClientArea', FOwner.ClientBounds);
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

function TLuiBar.GetAlignOffset(CellSize, ControlSize: Integer): Integer;
begin
  case FCellAlign of
    lbaDefault:
      Result := FInitialSpace;
    lbaInvert:
      Result := ControlSize - (CellSize + FInitialSpace);
    lbaCenter:
      Result := (ControlSize - CellSize) div 2;
  end;
end;

function TLuiBar.GetRealCellWidth: Integer;
begin
  if FCellWidth > 0 then
    Result := FCellWidth
  else
  begin
    if FPosition in [lbpRight, lbpLeft] then
      Result := Width - FOuterOffset
    else
      Result := (Width - FInitialSpace - (FSpacing * (FCells.Count - 1) )) div FCells.Count;
  end;
end;

function TLuiBar.GetTextWidth(const AText: String): Double;
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    TextExtents(AText, @Extents);
    Result := Extents.width;
  end;
end;

function TLuiBar.IndexToPatternType(Index: Integer): TLuiBarPatternType;
begin
  if Index = FSelectedIndex then
    Result := ptSelected
  else
    if Index = FHoverIndex then
      Result := ptHover
    else
      Result := ptNormal;
end;

procedure TLuiBar.SetCellAlign(const AValue: TLuiBarCellAlign);
begin
  if FCellAlign = AValue then exit;
  FCellAlign := AValue;
end;

procedure TLuiBar.SetImages(const AValue: TImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
end;

procedure TLuiBar.SetInitialSpace(const AValue: Integer);
begin
  if FInitialSpace=AValue then exit;
  FInitialSpace:=AValue;
end;

procedure TLuiBar.SetOnGetPattern(const AValue: TLuiBarGetPattern);
begin
  FOnGetPattern := AValue;
  FPatterns.FixedValues := AValue = nil;
end;

procedure TLuiBar.SetOuterOffset(const AValue: Integer);
begin
  if FOuterOffset = AValue then exit;
  FOuterOffset := AValue;
end;

procedure TLuiBar.SetPosition(const AValue: TLuiBarPosition);
begin
  if FPosition = AValue then exit;
  FPosition := AValue;
end;

procedure TLuiBar.DoUpdatePatterns;

  function DoGetPattern(AType: TLuiBarPatternType; DefaultColor: TColor): TCairoPattern;
  begin
    Result := nil;
    if Assigned(FOnGetPattern) then
      FOnGetPattern(Self, AType, Result);
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

{
procedure TLuiBar.FillCellClientGap(Cell: TLuiBarCell);
begin
  with Context do
  begin
    case FPosition of
    lbpTop:
      begin
        Rectangle(0, Cell.Height - FOutLineWidth, Cell.Width, FOutLineWidth);
      end;
    lbpLeft:
      begin
        Rectangle(Cell.Width - FOutLineWidth, 0, FOutLineWidth, Cell.Height);
      end;
    lbpRight:
      begin
        Rectangle(0, 0, FOutLineWidth, Cell.Height);
      end;
    lbpBottom:
      begin
        Rectangle(0, 0, Cell.Width, FOutLineWidth);
      end;
    end;
    Fill;
  end;
end;
}

procedure TLuiBar.DoDrawCellPath(Cell: TLuiBarCell);
begin
  if Assigned(FOnDrawCellPath) then
    FOnDrawCellPath(Self, Cell)
  else
    DefaultDrawCellPath(Cell);
end;

procedure TLuiBar.DoDrawCellText(Cell: TLuiBarCell);
begin
  if Assigned(FOnDrawCellText) then
    FOnDrawCellText(Self, Cell)
  else
    DefaultDrawCellText(Cell);
end;

procedure TLuiBar.DoDrawClientArea;
var
  LineOffset: Double;

  procedure DrawBaseLineSkipCell(Cell: TLuiBarCell);
  begin
    with Context do
    begin
      case FPosition of
        lbpTop:
          begin
            LineTo(Cell.Bounds.Left, FClientBounds.Top + LineOffset);
            Stroke;
            MoveTo(Cell.Bounds.Right, FClientBounds.Top + LineOffset);
          end;
        lbpLeft:
          begin
            LineTo(FClientBounds.Left + LineOffset, Cell.Bounds.Top);
            Stroke;
            MoveTo(FClientBounds.Left + LineOffset, Cell.Bounds.Bottom);
          end;
        lbpRight:
          begin
            LineTo(FClientBounds.Right - LineOffset, Cell.Bounds.Top);
            Stroke;
            LineTo(FClientBounds.Right - LineOffset, Cell.Bounds.Bottom);
          end;
        lbpBottom:
          begin
            LineTo(Cell.Bounds.Left, FClientBounds.Bottom - LineOffset);
            Stroke;
            LineTo(Cell.Bounds.Right, FClientBounds.Bottom - LineOffset);
          end;
      end;
    end;
  end;


var
  SkipCells: array[0..1] of Integer = (-1, -1);

begin
  with Context do
  begin
    //fill the client area
    Save;
    Translate(FClientBounds.Left, FClientBounds.Top);
    Rectangle(0, 0, FClientBounds.Right - FClientBounds.Left,
      FClientBounds.Bottom - FClientBounds.Top);
    Source := FPatterns.ClientArea;
    Fill;
    Restore;

    //Draw base outline
    if not (lboOmitBaseLine in FOptions) then
    begin
      //todo: handle when there's no client area
      //todo: make lineoffset a field
      LineOffset := GetSharpLineOffset(FOutLineWidth);

      LineWidth := FOutLineWidth;
      Source := FPatterns.OutLine;

      case FPosition of
        lbpTop:
          MoveTo(0, FClientBounds.Top + LineOffset);
        lbpLeft:
          MoveTo(FClientBounds.Left + LineOffset, 0);
        lbpRight:
          MoveTo(FClientBounds.Right - LineOffset, 0);
        lbpBottom:
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

      //todo remove all these case
      case FPosition of
        lbpTop:
          LineTo(Width - LineOffset, FClientBounds.Top + LineOffset);
        lbpLeft:
          LineTo(FClientBounds.Left + LineOffset, Height - LineOffset);
        lbpRight:
          LineTo(FClientBounds.Right - LineOffset, Height - LineOffset);
        lbpBottom:
          LineTo(Width, FClientBounds.Bottom - LineOffset);
      end;
    end;
    //todo: move this to DoDrawClientArea
    if lboOutLineClientArea in FOptions then
    begin
      case FPosition of
        lbpTop:
          begin
            LineTo(Width - LineOffset, Height - LineOffset);
            LineTo(LineOffset, Height - LineOffset);
            LineTo(LineOffset, FClientBounds.Top + LineOffset);
          end;
        lbpLeft:
          begin
            LineTo(Width - LineOffset, Height - LineOffset);
            LineTo(Width - LineOffset, LineOffset);
            LineTo(FClientBounds.Left + LineOffset, LineOffset);
          end;
        lbpRight:
          begin
            LineTo(LineOffset, Height - LineOffset);
            LineTo(LineOffset, LineOffset);
            LineTo(FClientBounds.Right - LineOffset, LineOffset);
          end;
        lbpBottom:
          begin
            LineTo(Width - LineOffset, LineOffset);
            LineTo(LineOffset, LineOffset);
            LineTo(LineOffset, FClientBounds.Bottom - LineOffset);
          end;
      end;
    end;
    Stroke;
    Restore;
  end;
end;

function TLuiBar.DoGetCellPattern(Cell: TLuiBarCell; PatternType: TLuiBarPatternType): TCairoPattern;
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
        if Cell.Patterns.Hover = nil then
          Result := FPatterns.Hover
        else
          Result := Cell.Patterns.Hover;
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

function TLuiBar.DoCalculateCellWidth(Cell: TLuiBarCell): Integer;
begin
  if FPosition in [lbpTop, lbpBottom] then
  begin
    if lboVariableCellWidth in FOptions then
    begin
      Result := Round(GetTextWidth(Cell.Title)) + CellTitlePadding * 2;
      if (Cell.ImageIndex >=0) and (FImages <> nil) then
        Inc(Result, FImages.Width + CellTitlePadding div 4);
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

procedure TLuiBar.DoDraw;
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
end;

procedure TLuiBar.DoDrawCell(Cell: TLuiBarCell);
begin
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, Cell)
  else
    DefaultDrawCell(Cell);
end;

procedure TLuiBar.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TLuiBar.DoDrawBackground;
begin
  with Context do
  begin
    Save;
    if Assigned(FOnDrawBackground) then
      FOnDrawBackground(Self)
    else
    begin
      Source := FPatterns.Background;
      Rectangle(0, 0, Width, Height);
      Fill;
    end;
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
      DoSelect;
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
  if lboHotTrack in FOptions then
  begin
    NewHoverIndex := CellInPoint(Point(X, Y));
    if NewHoverIndex <> FHoverIndex then
    begin
      FHoverIndex := NewHoverIndex;
      Redraw;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

constructor TLuiBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCells := TLuiBarCellList.Create(Self);
  FPatterns := TLuiBarPatterns.Create;
  FCellHeight := 20;
  FHoverIndex := -1;
  FSelectedIndex := -1;
  //todo: find more sensible colors
  with FColors do
  begin
    Normal := clSkyBlue;
    Selected := clBlue;
    Hover := clBlue;
    Text := clBlack;
    SelectedText := clWhite;
    ClientArea := Selected;
    Background := clWhite;
    OutLine := clWhite;
  end;
  SetInitialBounds(0, 0, 100, 30);
end;

procedure TLuiBar.DefaultDrawCell(Cell: TLuiBarCell);
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

procedure TLuiBar.DefaultDrawCellPath(const Cell: TLuiBarCell);
var
  InnerRadius: Integer;
  R: TDoubleRect;
begin
  if lboEmulateTab in FOptions then
    InnerRadius := 0
  else
    InnerRadius := FCellRoundRadius;
  CalculateSharpBounds(0, 0, Cell.Width, Cell.Height, FOutLineWidth, R);
  //todo: use matrix manipulation to consolidate the procedures??
  case FPosition of
    lbpTop:
      with Context do
      begin
        //this is necessary to avoid a gap between cell and client area
        if lboEmulateTab in FOptions then
          R.Bottom := R.Bottom + FOutLineWidth;
        MoveTo(InnerRadius, R.Bottom);
        CurveTo(InnerRadius, R.Bottom,
          R.Left, R.Bottom,
          R.Left, R.Bottom - InnerRadius);
        LineTo(R.Left, FCellRoundRadius);
        CurveTo(R.Left, FCellRoundRadius,
          R.Left, R.Top,
          FCellRoundRadius, R.Top);
        LineTo(R.Right - FCellRoundRadius, R.Top);
        CurveTo(R.Right - FCellRoundRadius, R.Top,
          R.Right, R.Top,
          R.Right, FCellRoundRadius);
        LineTo(R.Right, R.Bottom - InnerRadius);
        CurveTo(R.Right, R.Bottom - InnerRadius,
          R.Right, R.Bottom,
          R.Right - InnerRadius, R.Bottom);
      end;
    lbpBottom:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Top := R.Top - FOutLineWidth;
        MoveTo(InnerRadius, R.Top);
        CurveTo(InnerRadius, R.Top,
          R.Left, R.Top,
          R.Left, InnerRadius);
        LineTo(R.Left, R.Bottom - FCellRoundRadius);
        CurveTo(R.Left, R.Bottom - FCellRoundRadius,
          R.Left, R.Bottom,
          FCellRoundRadius, R.Bottom);
        LineTo(R.Right - FCellRoundRadius, R.Bottom);
        CurveTo(R.Right - FCellRoundRadius, R.Bottom,
          R.Right, R.Bottom,
          R.Right, R.Bottom - FCellRoundRadius);
        LineTo(R.Right, InnerRadius);
        CurveTo(R.Right, InnerRadius,
          R.Right, R.Top,
          R.Right - InnerRadius, R.Top);
      end;
    lbpLeft:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Right := R.Right + FOutLineWidth;
        MoveTo(R.Right, InnerRadius);
        CurveTo(R.Right, InnerRadius,
          R.Right, R.Top,
          R.Right - InnerRadius, R.Top);
        LineTo(FCellRoundRadius, R.Top);
        CurveTo(FCellRoundRadius, R.Top,
          R.Left, R.Top,
          R.Left, FCellRoundRadius);
        LineTo(R.Left, R.Bottom - FCellRoundRadius);
        CurveTo(R.Left, R.Bottom - FCellRoundRadius,
          R.Left, R.Bottom,
          FCellRoundRadius, R.Bottom);
        LineTo(R.Right - InnerRadius, R.Bottom);
        CurveTo(R.Right - InnerRadius, R.Bottom,
          R.Right, R.Bottom,
          R.Right, R.Bottom - InnerRadius);
      end;
    lbpRight:
      with Context do
      begin
        if lboEmulateTab in FOptions then
          R.Left := R.Left - FOutLineWidth;
        MoveTo(R.Left, InnerRadius);
        CurveTo(R.Left, InnerRadius,
          R.Left, R.Top,
          InnerRadius, R.Top);
        LineTo(R.Right - FCellRoundRadius, R.Top);
        CurveTo(R.Right - FCellRoundRadius, R.Top,
          R.Right, R.Top,
          R.Right, FCellRoundRadius);
        LineTo(R.Right, R.Bottom - FCellRoundRadius);
        CurveTo(R.Right, R.Bottom - FCellRoundRadius,
          R.Right, R.Bottom,
          R.Right - FCellRoundRadius, R.Bottom);
        LineTo(InnerRadius, R.Bottom);
        CurveTo(InnerRadius, R.Bottom,
          R.Left, R.Bottom,
          R.Left, R.Bottom - InnerRadius);
      end;
  end;
  if not (lboEmulateTab in FOptions) then
    Context.ClosePath;
end;

procedure TLuiBar.DefaultDrawCellText(Cell: TLuiBarCell);
const
  TextPatternMap: array[Boolean] of TLuiBarPatternType = (ptText, ptSelectedText);
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    Source := DoGetCellPattern(Cell, TextPatternMap[Cell.Index =
      FSelectedIndex]);
    TextExtents(Cell.Title, @Extents);
    if (Cell.ImageIndex >=0) and (FImages <> nil) then
    begin
      Extents.width := Extents.width - (FImages.Width + CellTitlePadding div 4
        );
      FImages.Draw(Bitmap.Canvas, Cell.Bounds.Left + CellTitlePadding,
        (Cell.Bounds.Top + ((Cell.Height - FImages.Height) div 2)),
         Cell.ImageIndex, (Cell.Index = FSelectedIndex) or (Cell.Index =
           FHoverIndex));
    end;
    MoveTo((Cell.Width  - Extents.width) / 2 , (Cell.Height + Extents.height)
      / 2);
    ShowText(Cell.Title);
  end;
end;

destructor TLuiBar.Destroy;
begin
  FCells.Destroy;
  FPatterns.Destroy;
  inherited Destroy;
end;

{ TLuiBarPatterns }

procedure TLuiBarPatterns.SetBackGround(const AValue: TCairoPattern);
begin
  FBackGround.Free;
  FBackGround := AValue;
end;

procedure TLuiBarPatterns.SetClientArea(const AValue: TCairoPattern);
begin
  FClientArea.Free;
  FClientArea := AValue;
end;

function TLuiBarPatterns.GetRequiresUpdate: Boolean;
begin
  Result := FInvalid;
end;

procedure TLuiBarPatterns.SetHover(const AValue: TCairoPattern);
begin
  FHover.Free;
  FHover := AValue;
end;

procedure TLuiBarPatterns.SetNormal(const AValue: TCairoPattern);
begin
  FNormal.Free;
  FNormal := AValue;
end;

procedure TLuiBarPatterns.SetOutLine(const AValue: TCairoPattern);
begin
  FOutLine.Free;
  FOutLine := AValue;
end;

procedure TLuiBarPatterns.SetSelected(const AValue: TCairoPattern);
begin
  FSelected.Free;
  FSelected := AValue;
end;

procedure TLuiBarPatterns.SetSelectedText(const AValue: TCairoPattern);
begin
  FSelectedText.Free;
  FSelectedText := AValue;
end;

procedure TLuiBarPatterns.SetText(const AValue: TCairoPattern);
begin
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

