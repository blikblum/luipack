unit LuiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, Graphics, LCLProc, math, CairoImaging, LMessages;

type

  TLuiImage = class;
  
  TLuiImageEvent = procedure (Sender: TLuiImage) of object;

  TLuiImageOption = (lioAutoSize, lioKeepAspectOnStretch);

  TLuiImageOptions = set of TLuiImageOption;

  TLuiImageState = (lisAutoSizePending, lisPaddingCalcPending, lisScaleCalcPending, lisScrollBarsUpdatePending);

  TLuiImageStates = set of TLuiImageState;

  TLuiImageViewStyle = (livNormal, livFitImage, livScale, livStretch, livTile);

  TLuiImageColors = record
    Background: TColor;
    OutLine: TColor;
  end;
  
  TLuiImagePatternType = (ptBackground, ptOutLine);
  
  TLuiImageGetPattern = procedure (Sender: TLuiImage;
    PatternType: TLuiImagePatternType; var Pattern: TCairoPattern) of object;
    
  TLuiImagePrepareMatrix = procedure (Sender: TLuiImage; const Matrix: TCairoMatrix) of object;

  { TLuiImagePadding }

  TLuiImagePadding = class
  private
    FBottom: Integer;
    FLeft: Integer;
    FOwner: TLuiImage;
    FRight: Integer;
    FTop: Integer;
    procedure Changed;
    procedure SetBottom(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRight(AValue: Integer);
    procedure SetTop(AValue: Integer);
  public
    constructor Create(AOwner: TLuiImage);
  published
    property Bottom: Integer read FBottom write SetBottom;
    property Left: Integer read FLeft write SetLeft;
    property Right: Integer read FRight write SetRight;
    property Top: Integer read FTop write SetTop;
  end;

  { TLuiImageScaleFactor }

  TLuiImageScaleFactor = class
  private
    FHorizontal: Double;
    FVertical: Double;
    FOwner: TLuiImage;
    procedure Changed;
    procedure SetHorizontal(const AValue: Double);
    procedure SetVertical(const AValue: Double);
  public
    constructor Create(AOwner: TLuiImage);
  published
    property Horizontal: Double read FHorizontal write SetHorizontal;
    property Vertical: Double read FVertical write SetVertical;
  end;
  
  { TLuiImagePatterns }

  TLuiImagePatterns = class
  private
    FBackGround: TCairoPattern;
    FInvalid: Boolean;
    FOutLine: TCairoPattern;
    procedure SetBackGround(const AValue: TCairoPattern);
    procedure SetOutLine(const AValue: TCairoPattern);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    procedure Updated;
    property RequiresUpdate: Boolean read FInvalid;
    //patterns
    property BackGround: TCairoPattern read FBackGround write SetBackGround;
    property OutLine: TCairoPattern read FOutLine write SetOutLine;
  end;

  { TLuiImage }

  TLuiImage = class(TCustomCairoControl)
  private
    FColors: TLuiImageColors;
    FEffectivePadding: TRect;
    FEffectiveXScale: Double;
    FEffectiveYScale: Double;
    FHScrollOffset: Integer;
    FHScrollRange: Integer;
    FOnAfterPaint: TLuiImageEvent;
    FOnBeforePaint: TLuiImageEvent;
    FOnDrawBackground: TLuiImageEvent;
    FOnDrawClipPath: TLuiImageEvent;
    FOnGetPattern: TLuiImageGetPattern;
    FOnPaintImage: TLuiImageEvent;
    FOnPrepareMatrix: TLuiImagePrepareMatrix;
    FOpacity: Double;
    FOptions: TLuiImageOptions;
    FOutLineWidth: Integer;
    FPadding: TLuiImagePadding;
    FPatterns: TLuiImagePatterns;
    FPicture: TCairoImagingPicture;
    FRoundRectRadius: Integer;
    FScaleFactor: TLuiImageScaleFactor;
    FStates: TLuiImageStates;
    FVScrollOffset: Integer;
    FVScrollRange: Integer;
    FViewStyle: TLuiImageViewStyle;
    FUpdateCount: Integer;
    procedure Changed;
    procedure CheckStates;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    procedure InternalAutoSize;
    procedure PictureChanged(Sender: TObject);
    procedure SetOpacity(const AValue: Double);
    procedure SetOptions(AValue: TLuiImageOptions);
    procedure SetOutLineWidth(AValue: Integer);
    procedure SetRoundEdgeRadius(AValue: Integer);
    procedure SetViewStyle(AValue: TLuiImageViewStyle);
    procedure UpdateHorizontalScrollBar;
    procedure UpdateVerticalScrollBar;
    procedure UpdateEffectivePadding;
    procedure UpdateEffectiveScale;
    procedure UpdatePatterns;
    procedure UpdateScrollBars;
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer) override;
    procedure DoAfterPaint; virtual;
    procedure DoBeforePaint; virtual;
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawClipPath; virtual;
    procedure DoOnResize; override;
    procedure DoPaintImage; virtual;
    procedure DoPrepareMatrix(const Matrix: TCairoMatrix); virtual;
    procedure DoSetSource; virtual;
    procedure Loaded; override;
    procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure DefaultAfterPaint;
    procedure DefaultBeforePaint;
    procedure DefaultDrawBackground;
    procedure EndUpdate;
    property Colors: TLuiImageColors read FColors write FColors;
    property Context;
  published
    property OnAfterPaint: TLuiImageEvent read FOnAfterPaint write FOnAfterPaint;
    property OnBeforePaint: TLuiImageEvent read FOnBeforePaint write FOnBeforePaint;
    property OnCreateContext;
    property OnDrawBackground: TLuiImageEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawClipPath: TLuiImageEvent read FOnDrawClipPath write FOnDrawClipPath;
    property OnGetPattern: TLuiImageGetPattern read FOnGetPattern write FOnGetPattern;
    property OnPaintImage: TLuiImageEvent read FOnPaintImage write FOnPaintImage;
    property OnPrepareMatrix: TLuiImagePrepareMatrix read FOnPrepareMatrix write FOnPrepareMatrix;
    property OnResize;
    property Opacity: Double read FOpacity write SetOpacity;
    property Options: TLuiImageOptions read FOptions write SetOptions;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Padding: TLuiImagePadding read FPadding;
    property Patterns: TLuiImagePatterns read FPatterns;
    property Picture: TCairoImagingPicture read FPicture;
    property RoundRectRadius: Integer read FRoundRectRadius write SetRoundEdgeRadius;
    property ViewStyle: TLuiImageViewStyle read FViewStyle write SetViewStyle;
    property ScaleFactor: TLuiImageScaleFactor read FScaleFactor;
  end;

implementation

uses
  cairo14, CairoUtils, LCLIntf, LCLType;

{ TLuiImage }

procedure TLuiImage.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not (lioAutoSize in FOptions) then
    inherited ChangeBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TLuiImage.Changed;
begin
  if (FUpdateCount = 0) and not (csLoading in ComponentState) then
  begin
    CheckStates;
    Redraw;
  end;
end;

procedure TLuiImage.CheckStates;
begin
  if lisScaleCalcPending in FStates then
    UpdateEffectiveScale;
  if lisPaddingCalcPending in FStates then
    UpdateEffectivePadding;
  if lisAutoSizePending in FStates then
    InternalAutoSize;
  if lisScrollBarsUpdatePending in FStates then
    UpdateScrollBars;
end;

function TLuiImage.GetImageWidth: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FPicture.Data.Width;
    livTile:
      Result := Width - FEffectivePadding.Left - FEffectivePadding.Right - (FOutLineWidth * 2);
    livScale:
      Result := Round(FPicture.Data.Width * FScaleFactor.Horizontal);
    livStretch, livFitImage:
      Result := Round(FPicture.Data.Width * FEffectiveXScale);
  end;
end;

function TLuiImage.GetImageHeight: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FPicture.Data.Height;
    livTile:
      Result := Height - FEffectivePadding.Top - FEffectivePadding.Bottom - (FOutLineWidth * 2);
    livScale:
      Result := Round(FPicture.Data.Height * FScaleFactor.Vertical);
    livStretch, livFitImage:
      Result := Round(FPicture.Data.Height * FEffectiveYScale);
  end;
end;

procedure TLuiImage.InternalAutoSize;
var
  DesiredWidth, DesiredHeight: Integer;
begin
  //todo: move to default LCL AutoSize ????
  if FViewStyle = livScale then
  begin
    DesiredWidth := Round(FPicture.Data.Width * FScaleFactor.Horizontal);
    DesiredHeight := Round(FPicture.Data.Height * FScaleFactor.Vertical);
  end
  else
  begin
    DesiredWidth := FPicture.Data.Width;
    DesiredHeight := FPicture.Data.Height;
  end;
  Inc(DesiredWidth, FEffectivePadding.Left + FEffectivePadding.Right + (FOutLineWidth * 2));
  Inc(DesiredHeight, FEffectivePadding.Top + FEffectivePadding.Bottom + (FOutLineWidth * 2));
  if (DesiredHeight <> Height) or (DesiredWidth <> Width) then
    inherited ChangeBounds(Left, Top, DesiredWidth, DesiredHeight);
  Exclude(FStates, lisAutoSizePending);
end;

procedure TLuiImage.PictureChanged(Sender: TObject);
begin
  if lioAutoSize in FOptions then
    Include(FStates, lisAutoSizePending);
  Include(FStates, lisScrollBarsUpdatePending);
  Changed;
  Redraw;
end;

procedure TLuiImage.SetOpacity(const AValue: Double);
begin
  if FOpacity = AValue then
    Exit;
  FOpacity := EnsureRange(AValue, 0, 1);
  Changed;
end;

procedure TLuiImage.SetOptions(AValue: TLuiImageOptions);
begin
  if FOptions = AValue then
    Exit;
  if lioAutoSize in (AValue - FOptions) then
  begin
    Include(FStates, lisAutoSizePending);
    Include(FStates, lisPaddingCalcPending);
    Include(FStates, lisScaleCalcPending);
  end;
  if (FViewStyle = livStretch) and
    ((lioKeepAspectOnStretch in FOptions) <> (lioKeepAspectOnStretch in AValue)) then
  begin
    Include(FStates, lisPaddingCalcPending);
    Include(FStates, lisScaleCalcPending);
  end;
  FOptions := AValue;
  Changed;
end;

procedure TLuiImage.SetOutLineWidth(AValue: Integer);
begin
  if FOutLineWidth = AValue then exit;
  FOutLineWidth := AValue;
  if lioAutoSize in FOptions then
    Include(FStates, lisAutoSizePending);
  Include(FStates, lisScaleCalcPending);
  Include(FStates, lisPaddingCalcPending);
  Changed;
end;

procedure TLuiImage.SetRoundEdgeRadius(AValue: Integer);
begin
  if FRoundRectRadius = AValue then exit;
  FRoundRectRadius := AValue;
  Changed;
end;

procedure TLuiImage.SetViewStyle(AValue: TLuiImageViewStyle);
begin
  if FViewStyle = AValue then
    Exit;
  Include(FStates, lisPaddingCalcPending);
  Include(FStates, lisScaleCalcPending);
  if not (AValue in [livFitImage, livStretch, livTile]) or
    (FViewStyle in [livNormal, livScale]) then
    Include(FStates, lisScrollBarsUpdatePending);
  FViewStyle := AValue;
  Changed;
end;

procedure TLuiImage.UpdateHorizontalScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FHScrollRange;
  ScrollInfo.nPos := FHScrollOffset;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Horz, ScrollInfo, True);
end;

procedure TLuiImage.UpdateVerticalScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  //ScrollInfo.nMin := 0;
  ScrollInfo.nMax := FVScrollRange;
  ScrollInfo.nPos := FVScrollOffset;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_Vert, ScrollInfo, True);
end;

procedure TLuiImage.UpdateEffectivePadding;
begin
  case FViewStyle of
    livFitImage:
      begin
        if lioAutoSize in FOptions then
        begin
          FEffectivePadding.Left := 0;
          FEffectivePadding.Top := 0;
          FEffectivePadding.Bottom := 0;
          FEffectivePadding.Right := 0;
        end
        else
        begin
          FEffectivePadding.Left := (Width - (GetImageWidth + FOutLineWidth * 2)) div 2;
          FEffectivePadding.Top := (Height - (GetImageHeight + FOutLineWidth * 2)) div 2;
        end;
      end;
    else
    begin
      FEffectivePadding.Left := FPadding.Left;
      FEffectivePadding.Top := FPadding.Top;
      FEffectivePadding.Right := FPadding.Right;
      FEffectivePadding.Bottom  := FPadding.Bottom;
    end;
  end;
  if (FViewStyle = livStretch) and (lioKeepAspectOnStretch in FOptions) then
  begin
    Inc(FEffectivePadding.Left, (Width - FPadding.Left - FPadding.Right - GetImageWidth - FOutLineWidth * 2) div 2);
    Inc(FEffectivePadding.Top, (Height - FPadding.Top - FPadding.Bottom - GetImageHeight - FOutLineWidth * 2) div 2);
  end;
  Exclude(FStates, lisPaddingCalcPending);
end;

procedure TLuiImage.UpdateEffectiveScale;
begin
  if (lioAutoSize in FOptions) and (FViewStyle <> livScale) then
  begin
    FEffectiveXScale := 1;
    FEffectiveYScale := 1;
    Exit;
  end;
  case FViewStyle of
    livStretch:
      begin
        FEffectiveXScale := (Width - FPadding.Left - FPadding.Right - (FOutLineWidth * 2)) / FPicture.Data.Width;
        FEffectiveYScale := (Height - FPadding.Top - FPadding.Bottom - (FOutLineWidth * 2)) / FPicture.Data.Height;
        if lioKeepAspectOnStretch in FOptions then
        begin
          FEffectiveXScale := Min(FEffectiveXScale, FEffectiveYScale);
          FEffectiveYScale := FEffectiveXScale;
        end;
      end;
    livScale:
      begin
        FEffectiveXScale := FScaleFactor.Horizontal;
        FEffectiveYScale := FScaleFactor.Vertical;
      end;
    livFitImage:
      begin
        if (FPicture.Data.Width + FOutLineWidth * 2) > Width then
          FEffectiveXScale := (Width - FOutLineWidth * 2) / FPicture.Data.Width
        else
          FEffectiveXScale := 1;
        if (FPicture.Data.Height + FOutLineWidth * 2) > Height then
          FEffectiveYScale := (Height - FOutLineWidth * 2) / FPicture.Data.Height
        else
          FEffectiveYScale := 1;
        FEffectiveXScale := Min(FEffectiveXScale, FEffectiveYScale);
        FEffectiveYScale := FEffectiveXScale;
      end;
  end;
  Exclude(FStates, lisScaleCalcPending);
end;

procedure TLuiImage.UpdatePatterns;

  function DoGetPattern(AType: TLuiImagePatternType; DefaultColor: TColor): TCairoPattern;
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
    if FColors.Background = clNone then
      FColors.Background := Parent.Color;
    BackGround := DoGetPattern(ptBackground, FColors.Background);
    OutLine := DoGetPattern(ptOutLine, FColors.OutLine);
    Updated;
  end;
end;

procedure TLuiImage.UpdateScrollBars;
var
  VScrollVisible, HScrollVisible: Boolean;
begin
  FVScrollOffset := 0;
  FVScrollRange := 0;
  FHScrollOffset := 0;
  FHScrollRange := 0;
  //shortcut
  if not (FViewStyle in [livNormal, livScale]) then
  begin
    ShowScrollBar(Handle, SB_BOTH, False);
    Exit;
  end;
  //vertical scrollbar
  FVScrollRange := FPadding.Top + FPadding.Bottom + GetImageHeight;
  VScrollVisible := (FVScrollRange - Height) > 0;
  DebugLn('VScrollVisible ', BoolToStr(VScrollVisible, True));
  ShowScrollBar(Handle, SB_Vert, VScrollVisible);
  //horizontal scrollbar (takes into account vertical scroll - if any)
  FHScrollRange := FPadding.Left + FPadding.Right + GetImageWidth;
  HScrollVisible := (FHScrollRange - ClientWidth) > 0;
  DebugLn('HScrollVisible ', BoolToStr(HScrollVisible, True));
  ShowScrollBar(Handle, SB_Horz, HScrollVisible);
  //update vertical scrollbar to take the horizontal scrollbar size into account
  if HScrollVisible and not VScrollVisible then
  begin
    VScrollVisible := (FVScrollRange - ClientHeight) > 0;
    if VScrollVisible then
      ShowScrollBar(Handle, SB_Vert, VScrollVisible);
  end;

  if VScrollVisible then
    UpdateVerticalScrollBar;
  if HScrollVisible then
    UpdateHorizontalScrollBar;

  Exclude(FStates, lisScrollBarsUpdatePending);
end;

procedure TLuiImage.DoAfterPaint;
begin
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self)
  else
    DefaultAfterPaint;
end;

procedure TLuiImage.DoBeforePaint;
begin
  if Assigned(FOnBeforePaint) then
    FOnBeforePaint(Self)
  else
    DefaultBeforePaint;
end;

procedure TLuiImage.DoDraw;
const
  Dash: Double = 2;
begin
  if csDesigning in ComponentState then
  begin
    Context.SetDash(@Dash, 1, 0);
    Context.Rectangle(0, 0, Width, Height);
    Context.Stroke;
    Exit;
  end;

  if FPicture.Surface = nil then
    Exit;
  if FPatterns.RequiresUpdate then
    UpdatePatterns;
    
  Context.Save;
  //Draw background
  DoDrawBackground;
  //Draw clip path and clip
  DoBeforePaint;
  //Save state before setting the matrix
  Context.Save;
  //Set the matrix and the source
  DoSetSource;
  //Paint the image
  DoPaintImage;
  //Restore the previous matrix
  Context.Restore;
  //Draw outline
  DoAfterPaint;
  Context.Restore;
end;

procedure TLuiImage.DoDrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
    DefaultDrawBackground;
end;

procedure TLuiImage.DoDrawClipPath;
var
  PosOffset: Double;
begin
  if Assigned(FOnDrawClipPath) then
    FOnDrawClipPath(Self)
  else
  begin
    if Odd(FOutLineWidth) then
      PosOffset := (FOutLineWidth div 2) + 0.5
    else
      PosOffset := FOutLineWidth / 2;
    RoundedRectangle(Context, PosOffset + FEffectivePadding.Left, PosOffset + FEffectivePadding.Top,
      GetImageWidth + FOutLineWidth, GetImageHeight + FOutLineWidth, FRoundRectRadius);
  end;
end;

procedure TLuiImage.DoOnResize;
begin
  inherited DoOnResize;
  if not (lisAutoSizePending in FStates) and (FPicture.Data.Size <> 0) then
  begin
    if FViewStyle in [livNormal, livScale] then
      Include(FStates, lisScrollBarsUpdatePending);
    Include(FStates, lisPaddingCalcPending);
    Include(FStates, lisScaleCalcPending);
    if lioAutoSize in FOptions then
      Include(FStates, lisAutoSizePending);
    Changed;
  end;
end;

procedure TLuiImage.DoPaintImage;
begin
  if Assigned(FOnPaintImage) then
    FOnPaintImage(Self)
  else
    Context.PaintWithAlpha(FOpacity);
end;

procedure TLuiImage.DoPrepareMatrix(const Matrix: TCairoMatrix);
begin
  if Assigned(FOnPrepareMatrix) then
    FOnPrepareMatrix(Self, Matrix)
  else
  begin
    Matrix.InitTranslate(FOutLineWidth + FEffectivePadding.Left - FHScrollOffset,
      FOutLineWidth + FEffectivePadding.Top - FVScrollOffset);
    if not (FViewStyle in [livTile, livNormal]) then
      Matrix.Scale(FEffectiveXScale, FEffectiveYScale);
  end;
end;

procedure TLuiImage.DoSetSource;
var
  TempPattern: TCairoPattern;
  Matrix: TCairoMatrix;
begin
  TempPattern := TCairoSurfacePattern.Create(FPicture.Surface);
  if FViewStyle = livTile then
    TempPattern.Extend := CAIRO_EXTEND_REPEAT;
  DoPrepareMatrix(Matrix);
  Context.SetMatrix(Matrix);
  Context.Source := TempPattern;
  TempPattern.Destroy;
end;

procedure TLuiImage.Loaded;
begin
  inherited Loaded;
  CheckStates;
end;

procedure TLuiImage.WMHScroll(var Message: TLMHScroll);
begin
  DebugLn('#WMHScroll#');
  case Message.ScrollCode of
    SB_BOTTOM:
      FHScrollOffset := FHScrollRange - ClientWidth;
    SB_ENDSCROLL:;
      //??;
    SB_LINEUP:
      FHScrollOffset := Max(FHScrollOffset - 10, 0);
    SB_LINEDOWN:
      FHScrollOffset := Min(FHScrollOffset + 10, FHScrollRange - ClientWidth);
    SB_PAGEUP:
      FHScrollOffset := Max(FHScrollOffset - ClientWidth, 0);
    SB_PAGEDOWN:
      FHScrollOffset := Min(FHScrollOffset + ClientWidth, FHScrollRange - ClientWidth);
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FHScrollOffset := Message.Pos;
      end;
    SB_TOP:
      FHScrollOffset := 0;
  end;
  UpdateHorizontalScrollBar;
  Message.Result := 0;
  Redraw;
end;

procedure TLuiImage.WMVScroll(var Message: TLMVScroll);
begin
  DebugLn('#WMVScroll#');
  case Message.ScrollCode of
    SB_BOTTOM:
      FVScrollOffset := FVScrollRange - ClientHeight;
    SB_ENDSCROLL:;
      //??;
    SB_LINEUP:
      FVScrollOffset := Max(FVScrollOffset - 10, 0);
    SB_LINEDOWN:
      FVScrollOffset := Min(FVScrollOffset + 10, FVScrollRange - ClientHeight);
    SB_PAGEUP:
      FVScrollOffset := Max(FVScrollOffset - ClientHeight, 0);
    SB_PAGEDOWN:
      FVScrollOffset := Min(FVScrollOffset + ClientHeight, FVScrollRange - ClientHeight);
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FVScrollOffset := Message.Pos;
      end;
    SB_TOP:
      FVScrollOffset := 0;
  end;
  UpdateVerticalScrollBar;
  Message.Result := 0;
  Redraw;
end;

constructor TLuiImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpacity := 1;
  FPadding := TLuiImagePadding.Create(Self);
  FPatterns := TLuiImagePatterns.Create;
  FPicture := TCairoImagingPicture.Create;
  FPicture.OnChange := @PictureChanged;
  FScaleFactor := TLuiImageScaleFactor.Create(Self);
  FStates := [lisPaddingCalcPending];
  with FColors do
  begin
    OutLine := clBlack;
    Background := clNone;
  end;
  SetInitialBounds(0, 0, 105, 105);
end;

destructor TLuiImage.Destroy;
begin
  FPadding.Destroy;
  FPatterns.Destroy;
  FPicture.Destroy;
  FScaleFactor.Destroy;
  inherited Destroy;
end;

procedure TLuiImage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLuiImage.DefaultAfterPaint;
begin
  if FOutLineWidth <= 0 then
    Exit;
  with Context do
  begin
    ResetClip;
    DoDrawClipPath;
    Source := FPatterns.OutLine;
    LineWidth := FOutLineWidth;
    Stroke;
  end;
end;

procedure TLuiImage.DefaultBeforePaint;
begin
  DoDrawClipPath;
  Context.Clip;
end;

procedure TLuiImage.DefaultDrawBackground;
begin
  with Context do
  begin
    Rectangle(0, 0, Width, Height);
    Source := FPatterns.BackGround;
    Fill;
  end;
end;

procedure TLuiImage.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

{ TLuiImagePatterns }

procedure TLuiImagePatterns.SetBackGround(const AValue: TCairoPattern);
begin
  FBackGround.Free;
  FBackGround := AValue;
end;

procedure TLuiImagePatterns.SetOutLine(const AValue: TCairoPattern);
begin
  FOutLine.Free;
  FOutLine := AValue;
end;

constructor TLuiImagePatterns.Create;
begin
  FInvalid := True;
end;

destructor TLuiImagePatterns.Destroy;
begin
  FOutLine.Free;
  FBackGround.Free;
end;

procedure TLuiImagePatterns.Invalidate;
begin
  FInvalid := True;
end;

procedure TLuiImagePatterns.Updated;
begin
  FInvalid := False;
end;

{ TLuiImageScaleFactor }

procedure TLuiImageScaleFactor.Changed;
begin
  with FOwner do
  begin
    if lioAutoSize in FOptions then
      Include(FStates, lisAutoSizePending);
    Include(FStates, lisScaleCalcPending);
    if FViewStyle = livScale then
      Include(FStates, lisScrollBarsUpdatePending);
    Changed;
  end;
end;

constructor TLuiImageScaleFactor.Create(AOwner: TLuiImage);
begin
  FOwner := AOwner;
  FHorizontal := 1;
  FVertical := 1;
end;

procedure TLuiImageScaleFactor.SetHorizontal(const AValue: Double);
begin
  if (FHorizontal = AValue) or (AValue = 0) then
    Exit;
  FHorizontal := AValue;
  Changed;
end;

procedure TLuiImageScaleFactor.SetVertical(const AValue: Double);
begin
  if (FVertical = AValue) or (AValue = 0) then
    Exit;
  FVertical := AValue;
  Changed;
end;

{ TLuiImagePadding }

procedure TLuiImagePadding.Changed;
begin
  with FOwner do
  begin
    if lioAutoSize in Options then
      Include(FStates, lisAutoSizePending);
    if ViewStyle = livStretch then
      Include(FStates, lisScaleCalcPending);
    Include(FStates, lisPaddingCalcPending);
    Changed;
  end;
end;

constructor TLuiImagePadding.Create(AOwner: TLuiImage);
begin
  FOwner := AOwner;
end;

procedure TLuiImagePadding.SetBottom(AValue: Integer);
begin
  if FBottom = AValue then exit;
  FBottom := AValue;
  Changed;
end;

procedure TLuiImagePadding.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then exit;
  FLeft := AValue;
  Changed;
end;

procedure TLuiImagePadding.SetRight(AValue: Integer);
begin
  if FRight = AValue then exit;
  FRight := AValue;
  Changed;
end;

procedure TLuiImagePadding.SetTop(AValue: Integer);
begin
  if FTop = AValue then exit;
  FTop := AValue;
  Changed;
end;

end.

