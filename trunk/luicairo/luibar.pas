unit LuiBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, types, Controls, Cairo14, math;

type


  TLuiBar = class;
  
  TCellInfo = class;
  
  TLuiBarOption = (lboEmulateTab, lboHoverAsSelected, lboOutLineClientArea, lboVariableCellWidth);
  
  TLuiBarOptions = set of TLuiBarOption;
  
  TLuiBarPosition = (lbpTop, lbpLeft, lbpBottom, lbpRight);
  
  TLuiBarNotify = procedure (Sender: TLuiBar) of object;
  
  TPatternType = (ptSelected, ptNormal, ptHover, ptText, ptSelectedText, ptBackground, ptOutLine);
  
  TLuiBarGetPattern = procedure (Sender: TLuiBar; PatternType: TPatternType;
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
    FImageIndex: Integer;
    FIndex: Integer;
    FPattern: TCairoPattern;
    FTitle: String;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    destructor Destroy; override;
    property Height: Integer read GetHeight;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Index: Integer read FIndex write FIndex;
    property Pattern: TCairoPattern read FPattern write FPattern;
    property Title: String read FTitle write FTitle;
    property Bounds: TRect read FBounds write FBounds;
    property Width: Integer read GetWidth;
  end;

  { TCellInfoList }

  TCellInfoList = class
  private
    FList: TFpList;
    FOwner: TLuiBar;
    FRequiresUpdate: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCellInfo; inline;
  public
    constructor Create(Owner: TLuiBar);
    destructor Destroy; override;
    function Add(const Title: String; ImageIndex: Integer): TCellInfo;
    procedure Clear;
    procedure UpdateCellBounds;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCellInfo read GetItems; default;
    property RequiresUpdate: Boolean read FRequiresUpdate;
  end;
  { TLuiBar }

  TLuiBar = class(TCairoControl)
  private
    FCellHeight: Integer;
    FCells: TCellInfoList;
    FCellWidth: Integer;
    FImages: TImageList;
    FInitialSpace: Integer;
    FOuterOffset: Integer;
    FPatterns: TLuiBarPatterns;
    FInnerRadius: Double;
    FOnGetDefaultPatterns: TLuiBarGetPattern;
    FOnSelect: TLuiBarNotify;
    FOptions: TLuiBarOptions;
    FOuterRadius: Double;
    FOutLineWidth: Integer;
    FPosition: TLuiBarPosition;
    FSelectedIndex: Integer;
    FHoverIndex: Integer;
    FSpacing: Integer;
    function CellInPoint(const P: TPoint): Integer;
    procedure DrawRoundedRect(XStart, YStart, XOuter1, YOuter1, XOuter2, YOuter2, XEnd, YEnd: Integer);
    function GetRealCellWidth: Integer;
    function GetTextWidth(const AText: String): Double;
    procedure SetImages(const AValue: TImageList);
    procedure SetInitialSpace(const AValue: Integer);
    procedure SetOnGetPatterns(const AValue: TLuiBarGetPattern);
    procedure SetOuterOffset(const AValue: Integer);
    procedure SetPosition(const AValue: TLuiBarPosition);
    procedure SetSelectedIndex(const AValue: Integer);
  protected
    function DoCalculateCellWidth(Cell: TCellInfo): Integer;
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawCell(Cell: TCellInfo); virtual;
    procedure DoDrawCellPath(Cell: TCellInfo); virtual;
    procedure DoDrawCellTitle(Cell: TCellInfo); virtual;
    procedure DoSelect; virtual;
    procedure DoUpdatePatterns; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                       Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const CellTitle: String; ImageIndex: Integer = -1);
    property Cells: TCellInfoList read FCells;
    property CellHeight: Integer read FCellHeight write FCellHeight;
    property CellWidth: Integer read FCellWidth write FCellWidth;
    property Images: TImageList read FImages write SetImages;
    property InitialSpace: Integer read FInitialSpace write SetInitialSpace;
    property InnerRadius: Double read FInnerRadius write FInnerRadius;
    property Options: TLuiBarOptions read FOptions write FOptions;
    property OuterOffset: Integer read FOuterOffset write SetOuterOffset;
    property OuterRadius: Double read FOuterRadius write FOuterRadius;
    property OutLineWidth: Integer read FOutLineWidth write FOutLineWidth;
    property Position: TLuiBarPosition read FPosition write SetPosition;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Spacing: Integer read FSpacing write FSpacing;
    //events
    property OnGetPatterns: TLuiBarGetPattern read FOnGetDefaultPatterns write SetOnGetPatterns;
    property OnSelect: TLuiBarNotify read FOnSelect write FOnSelect;
  published

  end;

implementation

uses
  sharedlogger;

const
  CellTitlePadding = 8;
  
{ TCellInfoList }

function TCellInfoList.GetItems(Index: Integer): TCellInfo;
begin
  Result := TCellInfo(FList[Index]);
end;


constructor TCellInfoList.Create(Owner: TLuiBar);
begin
  FList := TFPList.Create;
  FOwner := Owner;
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

function TCellInfoList.Add(const Title: String; ImageIndex: Integer): TCellInfo;
begin
  Result := TCellInfo.Create;
  Result.Title := Title;
  Result.Index := FList.Count;
  Result.ImageIndex := ImageIndex;
  FList.Add(Result);
  FRequiresUpdate := True;
end;

procedure TCellInfoList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TCellInfo(FList[i]).Destroy;
  FList.Clear;
end;

procedure TCellInfoList.UpdateCellBounds;
var
  i, NextLeft, NextTop, NewWidth: Integer;
  Cell: TCellInfo;
begin
  case FOwner.Position of
    lbpTop:
      begin
        NextLeft := FOwner.InitialSpace;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Left := NextLeft;
          Cell.Bounds.Top := FOwner.OuterOffset;
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
      end;
    lbpLeft:
      begin
        NextTop := FOwner.InitialSpace;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Top := NextTop;
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          Cell.Bounds.Left := FOwner.OuterOffset;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
      end;
    lbpRight:
      begin
        NextTop := FOwner.InitialSpace;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Top := NextTop;
          Cell.Bounds.Right := FOwner.Width - FOwner.OuterOffset;
          Cell.Bounds.Left := Cell.Bounds.Right - FOwner.DoCalculateCellWidth(Cell);
          Cell.Bounds.Bottom := Cell.Bounds.Top + FOwner.CellHeight;
          NextTop := Cell.Bounds.Bottom + FOwner.Spacing;
        end;
      end;
    lbpBottom:
      begin
        NextLeft := FOwner.InitialSpace;
        for i := 0 to FList.Count - 1 do
        begin
          Cell := Items[i];
          Cell.Bounds.Left := NextLeft;
          Cell.Bounds.Bottom := FOwner.Height - FOwner.OuterOffset;
          Cell.Bounds.Top := Cell.Bounds.Bottom - FOwner.CellHeight;
          Cell.Bounds.Right := Cell.Bounds.Left + FOwner.DoCalculateCellWidth(Cell);
          NextLeft := Cell.Bounds.Right + FOwner.Spacing;
        end;
      end;
  end;
  FRequiresUpdate := False;
end;

{ TCellInfo }

function TCellInfo.GetWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

function TCellInfo.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
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

procedure TLuiBar.DrawRoundedRect(XStart, YStart, XOuter1, YOuter1, XOuter2, YOuter2, XEnd, YEnd: Integer);
begin
  with Context do
  begin
    MoveTo(XStart + FInnerRadius, YStart);
    CurveTo(XStart + FInnerRadius, YStart,
      XStart, YStart,
      XStart, YStart - FInnerRadius);
    LineTo(XOuter1, YOuter1 - FOuterRadius);
    CurveTo(XOuter1, YOuter1 - FOuterRadius,
      XOuter1, YOuter1,
      XOuter1 + FOuterRadius, YOuter1);
    LineTo(XOuter2 - FOuterRadius, YOuter2);
    CurveTo(XOuter2 - FOuterRadius, YOuter2,
      XOuter2, YOuter2,
      XOuter2, YOuter2 + FOuterRadius);
    LineTo(XEnd, YEnd - FInnerRadius);
    CurveTo(XEnd, YEnd - FInnerRadius,
      XEnd, YEnd,
      XEnd - FInnerRadius, YEnd);
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

procedure TLuiBar.SetOnGetPatterns(const AValue: TLuiBarGetPattern);
begin
  FOnGetDefaultPatterns := AValue;
  FPatterns.FixedValues := AValue = nil;
end;

procedure TLuiBar.SetOuterOffset(const AValue: Integer);
begin
  if FOuterOffset=AValue then exit;
  FOuterOffset:=AValue;
end;

procedure TLuiBar.SetPosition(const AValue: TLuiBarPosition);
begin
  if FPosition=AValue then exit;
  FPosition:=AValue;
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

procedure TLuiBar.DoDrawCellPath(Cell: TCellInfo);
begin
  case FPosition of
    lbpTop:
      DrawRoundedRect(0, Cell.Height, 0, 0, Cell.Width, 0, Cell.Width, Cell.Height);
    lbpBottom:
      DrawRoundedRect(0, 0, 0, Cell.Height, Cell.Width, Cell.Height, Cell.Width, 0);
    lbpLeft:
      DrawRoundedRect(Cell.Width, 0, 0, 0, 0, Cell.Height, Cell.Width, Cell.Height);
    lbpRight:
      DrawRoundedRect(0, 0, Cell.Width, 0, Cell.Width, Cell.Height, 0, Cell.Height);
  end;
  if not (lboEmulateTab in FOptions) then
    Context.ClosePath;
end;

procedure TLuiBar.DoDrawCellTitle(Cell: TCellInfo);
var
  Extents: cairo_text_extents_t;
begin
  with Context do
  begin
    if Cell.Index = FSelectedIndex then
      Source := FPatterns.SelectedText
    else
      Source := FPatterns.Text;
    TextExtents(Cell.Title, @Extents);
    if (Cell.ImageIndex >=0) and (FImages <> nil) then
    begin
      Extents.width := Extents.width - (FImages.Width + CellTitlePadding div 4);
      FImages.Draw(Bitmap.Canvas, Cell.Bounds.Left + CellTitlePadding,
        (Cell.Bounds.Top + ((Cell.Height - FImages.Height) div 2)),
         Cell.ImageIndex, (Cell.Index = FSelectedIndex) or (Cell.Index = FHoverIndex));
    end;
    MoveTo((Cell.Width  - Extents.width) / 2 , (Cell.Height + Extents.height) / 2);
    ShowText(Cell.Title);
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

function TLuiBar.DoCalculateCellWidth(Cell: TCellInfo): Integer;
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
begin
  if FPatterns.RequiresUpdate then
    DoUpdatePatterns;
  if FCells.RequiresUpdate then
    FCells.UpdateCellBounds;
  DoDrawBackground;
  for i := 0 to FCells.Count - 1 do
    DoDrawCell(FCells[i]);
end;

procedure TLuiBar.DoDrawCell(Cell: TCellInfo);
begin
  with Context do
  begin
    Save;
    Translate(Cell.Bounds.Left, Cell.Bounds.Top);
    DoDrawCellPath(Cell);
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
    DoDrawCellTitle(Cell);
    Restore;
  end;
end;

procedure TLuiBar.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TLuiBar.DoDrawBackground;

  procedure DrawBaseLineSkipCell(Cell: TCellInfo);
  begin
    with Context do
    begin
      case FPosition of
        lbpTop:
          begin
            LineTo(Cell.Bounds.Left, FOuterOffset + FCellHeight);
            Stroke;
            MoveTo(Cell.Bounds.Right, FOuterOffset + FCellHeight);
          end;
        lbpLeft:
          begin
            LineTo(FOuterOffset + GetRealCellWidth, Cell.Bounds.Top);
            Stroke;
            MoveTo(FOuterOffset + GetRealCellWidth, Cell.Bounds.Bottom);
          end;
        lbpRight:
          begin
            LineTo(Width - (FOuterOffset + GetRealCellWidth), Cell.Bounds.Top);
            Stroke;
            LineTo(Width - (FOuterOffset + GetRealCellWidth), Cell.Bounds.Bottom);
          end;
        lbpBottom:
          begin
            LineTo(Cell.Bounds.Left, Height - (FOuterOffset + FCellHeight));
            Stroke;
            LineTo(Cell.Bounds.Right, Height - (FOuterOffset + FCellHeight));
          end;
      end;
    end;
  end;


var
  SkipCells: array[0..1] of Integer = (-1, -1);
  
begin
  with Context do
  begin
    Save;
    Source := FPatterns.Background;
    Rectangle(0, 0, Width, Height);
    Fill;
    if lboEmulateTab in FOptions then
    begin
      //todo: handle when there's no client area
      //todo: store client area size

        case FPosition of
          lbpTop:
            Rectangle(0, FOuterOffset + FCellHeight, Width, Height - (FOuterOffset + FCellHeight));
          lbpLeft:
            Rectangle(FOuterOffset + GetRealCellWidth, 0, Width , Height);
          lbpRight:
            Rectangle(0, 0, Width - (FOuterOffset + GetRealCellWidth), Height);
          lbpBottom:
            Rectangle(0, 0, Width, Height - (FOuterOffset + FCellHeight));
        end;
        Source := FPatterns.Selected;
        Fill;


      //Draw base outline
      LineWidth := FOutLineWidth;
      Source := FPatterns.OutLine;
      
      case FPosition of
        lbpTop:
          MoveTo(0, FOuterOffset + FCellHeight);
        lbpLeft:
          MoveTo(FOuterOffset + GetRealCellWidth, 0);
        lbpRight:
          MoveTo(Width - (FOuterOffset + GetRealCellWidth), 0);
        lbpBottom:
          MoveTo(0, Height - (FOuterOffset + FCellHeight));
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
          LineTo(Width, FOuterOffset + FCellHeight);
        lbpLeft:
          LineTo(FOuterOffset + GetRealCellWidth, Height);
        lbpRight:
          LineTo(Width - (FOuterOffset + GetRealCellWidth), Height);
        lbpBottom:
          LineTo(Width, Height - (FOuterOffset + FCellHeight));
      end;

      if lboOutLineClientArea in FOptions then
      begin
        case FPosition of
          lbpTop:
            begin
              LineTo(Width, Height);
              LineTo(0, Height);
              LineTo(0, FOuterOffset + FCellHeight);
            end;
          lbpLeft:
            begin
              LineTo(Width, Height);
              LineTo(Width, 0);
              LineTo(FOuterOffset + GetRealCellWidth, 0);
            end;
          lbpRight:
            begin
              LineTo(0, Height);
              LineTo(0, 0);
              LineTo(Width - (FOuterOffset + GetRealCellWidth), 0);
            end;
          lbpBottom:
            begin
              LineTo(Width, 0);
              LineTo(0, 0);
              LineTo(0, Height - (FOuterOffset + FCellHeight));
            end;
        end;
      end;
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
  FCellHeight := 20;
  FHoverIndex := -1;
  FSelectedIndex := -1;
end;

destructor TLuiBar.Destroy;
begin
  FCells.Destroy;
  inherited Destroy;
end;

procedure TLuiBar.Add(const CellTitle: String; ImageIndex: Integer = -1);
var
  NewCell: TCellInfo;
begin
  NewCell := FCells.Add(CellTitle, ImageIndex);
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

