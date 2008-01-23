unit LuiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, Graphics, LCLProc, math;

type

  TLuiImage = class;
  
  TLuiImageEvent = procedure (Sender: TLuiImage) of object;

  TLuiImageOption = (lioAutoSize);

  TLuiImageOptions = set of TLuiImageOption;

  TLuiImageState = (lisAutoSizePending, lisPaddingCalcPending, lisScaleCalcPending);

  TLuiImageStates = set of TLuiImageState;

  TLuiImageViewStyle = (livNormal, livFitImage, livScale, livStretch, livTile);

  TLuiImageColors = record
    Background: TColor;
    OutLine: TColor;
  end;
  
  TLuiImagePatternType = (ptBackground, ptOutLine);
  
  TLuiImageGetPattern = procedure (Sender: TLuiImage;
    PatternType: TLuiImagePatternType; var Pattern: TCairoPattern) of object;

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
    FCurrentBitmap: TBitmap;
    FEffectivePadding: TRect;
    FEffectiveXScale: Double;
    FEffectiveYScale: Double;
    FImageSurface: TCairoDCSurface;
    FOnAfterDraw: TLuiImageEvent;
    FOnBeforeDraw: TLuiImageEvent;
    FOnDrawBackground: TLuiImageEvent;
    FOnDrawClipPath: TLuiImageEvent;
    FOnGetPattern: TLuiImageGetPattern;
    FOpacity: Double;
    FOptions: TLuiImageOptions;
    FOutLineWidth: Integer;
    FPadding: TLuiImagePadding;
    FPatterns: TLuiImagePatterns;
    FPicture: TPicture;
    FRoundRectRadius: Integer;
    FScaleFactor: TLuiImageScaleFactor;
    FStates: TLuiImageStates;
    FViewStyle: TLuiImageViewStyle;
    FUpdateCount: Integer;
    procedure Changed;
    procedure CheckStates;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    procedure InternalAutoSize;
    procedure PictureChanged(Sender: TObject);
    procedure ResetImageSurface;
    procedure SetOpacity(const AValue: Double);
    procedure SetOptions(AValue: TLuiImageOptions);
    procedure SetOutLineWidth(AValue: Integer);
    procedure SetRoundEdgeRadius(AValue: Integer);
    procedure SetViewStyle(AValue: TLuiImageViewStyle);
    procedure UpdateEffectivePadding;
    procedure UpdateEffectiveScale;
    procedure UpdatePatterns;
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer) override;
    procedure DoAfterDraw; virtual;
    procedure DoBeforeDraw; virtual;
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual;
    procedure DoDrawClipPath; virtual;
    procedure DoOnResize; override;
    procedure DoSetSource; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure DefaultAfterDraw;
    procedure DefaultBeforeDraw;
    procedure DefaultDrawBackground;
    procedure EndUpdate;
    property Colors: TLuiImageColors read FColors write FColors;
    property Context;
  published
    property OnAfterDraw: TLuiImageEvent read FOnAfterDraw write FOnAfterDraw;
    property OnBeforeDraw: TLuiImageEvent read FOnBeforeDraw write FOnBeforeDraw;
    property OnDrawBackground: TLuiImageEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawClipPath: TLuiImageEvent read FOnDrawClipPath write FOnDrawClipPath;
    property OnGetPattern: TLuiImageGetPattern read FOnGetPattern write FOnGetPattern;
    property OnResize;
    property Opacity: Double read FOpacity write SetOpacity;
    property Options: TLuiImageOptions read FOptions write SetOptions;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Padding: TLuiImagePadding read FPadding;
    property Patterns: TLuiImagePatterns read FPatterns;
    property Picture: TPicture read FPicture;
    property RoundRectRadius: Integer read FRoundRectRadius write SetRoundEdgeRadius;
    property ViewStyle: TLuiImageViewStyle read FViewStyle write SetViewStyle;
    property ScaleFactor: TLuiImageScaleFactor read FScaleFactor;
  end;

implementation

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double; Radius: Double);
begin
  with Context do
  begin
    //Todo: add newpath??
    MoveTo(X, Y + Radius);
    CurveTo(X, Y + Radius,
      X, Y,
      X + Radius, Y);
    LineTo(X + Width - Radius, Y);
    CurveTo(X + Width - Radius,
      Y, X + Width, Y,
      X + Width, Y + Radius);
    LineTo(X + Width, Y + Height - Radius);
    CurveTo(X + Width, Y + Height - Radius,
      X + Width, Y + Height,
      Width + X - Radius, Y + Height);
    LineTo(X + Radius, Y + Height);
    CurveTo(X + Radius, Y + Height,
      X, Y + Height,
      X, Y + Height - Radius);
    ClosePath;
  end;
end;

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
end;

function TLuiImage.GetImageWidth: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FCurrentBitmap.Width;
    livStretch:
      Result := Width - FEffectivePadding.Left - FEffectivePadding.Right - (FOutLineWidth * 2);
    livScale:
      Result := Round(FCurrentBitmap.Width * FScaleFactor.Horizontal);
    livFitImage:
      Result := Round(FCurrentBitmap.Width * FEffectiveXScale);
  end;
end;

function TLuiImage.GetImageHeight: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FCurrentBitmap.Height;
    livStretch:
      Result := Height - FEffectivePadding.Top - FEffectivePadding.Bottom - (FOutLineWidth * 2);
    livScale:
      Result := Round(FCurrentBitmap.Height * FScaleFactor.Vertical);
    livFitImage:
      Result := Round(FCurrentBitmap.Height * FEffectiveYScale);
  end;
end;

procedure TLuiImage.InternalAutoSize;
var
  DesiredWidth, DesiredHeight: Integer;
begin
  //todo: move to default LCL AutoSize ????
  if FViewStyle = livScale then
  begin
    DesiredWidth := Round(FCurrentBitmap.Width * FScaleFactor.Horizontal);
    DesiredHeight := Round(FCurrentBitmap.Height * FScaleFactor.Vertical);
  end
  else
  begin
    DesiredWidth := FCurrentBitmap.Width;
    DesiredHeight := FCurrentBitmap.Height;
  end;
  Inc(DesiredWidth, FEffectivePadding.Left + FEffectivePadding.Right + (FOutLineWidth * 2));
  Inc(DesiredHeight, FEffectivePadding.Top + FEffectivePadding.Bottom + (FOutLineWidth * 2));
  if (DesiredHeight <> Height) or (DesiredWidth <> Width) then
    inherited ChangeBounds(Left, Top, DesiredWidth, DesiredHeight);
  Exclude(FStates, lisAutoSizePending);
end;

procedure TLuiImage.PictureChanged(Sender: TObject);
var
  NewBitmap: TBitmap;
begin
  if FPicture.Graphic <> nil then
    NewBitmap := FPicture.Bitmap
  else
    NewBitmap := nil;
    
  if NewBitmap <> FCurrentBitmap then
  begin
    FCurrentBitmap := NewBitmap;
    ResetImageSurface;
  end;
  if lioAutoSize in FOptions then
    Include(FStates, lisAutoSizePending);
  Redraw;
end;

procedure TLuiImage.ResetImageSurface;
begin
  FImageSurface.Free;
  FImageSurface := TCairoDCSurface.Create(FCurrentBitmap.Canvas.Handle);
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
  FViewStyle := AValue;
  Changed;
end;

procedure TLuiImage.UpdateEffectivePadding;
begin
  if FViewStyle = livFitImage then
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
      FEffectivePadding.Left := (Width - (GetImageWidth + FOutLineWidth*2)) div 2;
      FEffectivePadding.Top := (Height - (GetImageHeight + FOutLineWidth*2)) div 2;
    end;
  end
  else
  begin
    FEffectivePadding.Left := FPadding.Left;
    FEffectivePadding.Top := FPadding.Top;
    FEffectivePadding.Right := FPadding.Right;
    FEffectivePadding.Bottom  := FPadding.Bottom;
  end;
  Exclude(FStates, lisPaddingCalcPending);
end;

procedure TLuiImage.UpdateEffectiveScale;
begin
  case FViewStyle of
    livStretch:
      begin
        FEffectiveXScale := GetImageWidth / FCurrentBitmap.Width;
        FEffectiveYScale := GetImageHeight / FCurrentBitmap.Height;
      end;
    livScale:
      begin
        FEffectiveXScale := FScaleFactor.Horizontal;
        FEffectiveYScale := FScaleFactor.Vertical;
      end;
    livFitImage:
      begin
        if (FCurrentBitmap.Width + FOutLineWidth*2) > Width then
          FEffectiveXScale := (Width - FOutLineWidth*2) / FCurrentBitmap.Width
        else
          FEffectiveXScale := 1;
        if (FCurrentBitmap.Height + FOutLineWidth*2) > Height then
          FEffectiveYScale := (Height - FOutLineWidth*2) / FCurrentBitmap.Height
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

procedure TLuiImage.DoAfterDraw;
begin
  if Assigned(FOnAfterDraw) then
    FOnAfterDraw(Self)
  else
    DefaultAfterDraw;
end;

procedure TLuiImage.DoBeforeDraw;
begin
  if Assigned(FOnBeforeDraw) then
    FOnBeforeDraw(Self)
  else
    DefaultBeforeDraw;
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
  if FImageSurface = nil then
    Exit;
  if FPatterns.RequiresUpdate then
    UpdatePatterns;
  with Context do
  begin
    Save;
    DoDrawBackground;
    DoBeforeDraw;
    DoSetSource;
    PaintWithAlpha(FOpacity);
    DoAfterDraw;
    Restore;
  end;
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
  if not (lisAutoSizePending in FStates) and (FCurrentBitmap <> nil) then
  begin
    Include(FStates, lisPaddingCalcPending);
    Include(FStates, lisScaleCalcPending);
    if lioAutoSize in FOptions then
      Include(FStates, lisAutoSizePending);
    Changed;
  end;
end;

procedure TLuiImage.DoSetSource;
var
  TempPattern: TCairoPattern;
  Matrix: TCairoMatrix;
  XScale, YScale: Double;
begin
  if (FViewStyle = livNormal) or
    ((lioAutoSize in FOptions) and (FViewStyle = livStretch)) then
    Context.SetSourceSurface(FImageSurface, FOutLineWidth + FEffectivePadding.Left,
      FOutLineWidth + FEffectivePadding.Top)
  else
    case FViewStyle of
      livStretch, livScale, livFitImage:
        begin
          TempPattern := TCairoSurfacePattern.Create(FImageSurface);
          Matrix.InitScale(1/FEffectiveXScale, 1/FEffectiveYScale);
          Matrix.Translate(-(FOutLineWidth + FEffectivePadding.Left),
            -(FOutLineWidth + FEffectivePadding.Top));
          TempPattern.SetMatrix(Matrix);
          Context.Source := TempPattern;
          TempPattern.Destroy;
        end;
    end;
end;

procedure TLuiImage.Loaded;
begin
  inherited Loaded;
  CheckStates;
end;

constructor TLuiImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpacity := 1;
  FPadding := TLuiImagePadding.Create(Self);
  FPatterns := TLuiImagePatterns.Create;
  FPicture := TPicture.Create;
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
  FImageSurface.Free;
end;

procedure TLuiImage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLuiImage.DefaultAfterDraw;
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

procedure TLuiImage.DefaultBeforeDraw;
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
  if FHorizontal = AValue then
    Exit;
  FHorizontal := AValue;
  Changed;
end;

procedure TLuiImageScaleFactor.SetVertical(const AValue: Double);
begin
  if FVertical = AValue then
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

