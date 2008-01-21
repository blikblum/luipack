unit LuiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, Graphics, LCLProc;

type

  TLuiImage = class;
  
  TLuiImageEvent = procedure (Sender: TLuiImage) of object;

  TLuiImageOption = (lioAutoSize);

  TLuiImageOptions = set of TLuiImageOption;

  TLuiImageState = (lisAutoSizePending);

  TLuiImageStates = set of TLuiImageState;

  TLuiImageViewStyle = (livNormal, livCenter, livScale, livAutoScale, livZoom, livTile);

  TLuiImageColors = record
    Background: TColor;
    OutLine: TColor;
  end;
  
  TLuiImagePatternType = (ptBackground, ptOutLine);
  
  TLuiImageGetPattern = procedure (Sender: TLuiImage;
    PatternType: TLuiImagePatternType; var Pattern: TCairoPattern) of object;

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
    FImageSurface: TCairoDCSurface;
    FOnAfterDraw: TLuiImageEvent;
    FOnBeforeDraw: TLuiImageEvent;
    FOnDrawClipPath: TLuiImageEvent;
    FOnGetPattern: TLuiImageGetPattern;
    FOptions: TLuiImageOptions;
    FOutLineWidth: Integer;
    FPadding: TRect;
    FPatterns: TLuiImagePatterns;
    FPicture: TPicture;
    FRoundEdgeRadius: Integer;
    FStates: TLuiImageStates;
    FViewStyle: TLuiImageViewStyle;
    FUpdateCount: Integer;
    procedure Changed;
    function ImageWidth: Integer;
    function ImageHeight: Integer;
    procedure InternalAutoSize;
    procedure PictureChanged(Sender: TObject);
    procedure ResetImageSurface;
    procedure SetOptions(AValue: TLuiImageOptions);
    procedure SetOutLineWidth(AValue: Integer);
    procedure SetPadding(const AValue: TRect);
    procedure SetRoundEdgeRadius(AValue: Integer);
    procedure SetViewStyle(AValue: TLuiImageViewStyle);
    procedure UpdatePatterns;
  protected
    procedure DoAfterDraw; virtual;
    procedure DoBeforeDraw; virtual;
    procedure DoDraw; override;
    procedure DoDrawClipPath; virtual;
    procedure DoSetSource; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure DefaultAfterDraw;
    procedure DefaultBeforeDraw;
    procedure EndUpdate;
    property Colors: TLuiImageColors read FColors write FColors;
    property Context;
    property Padding: TRect read FPadding write SetPadding;
  published
    property OnAfterDraw: TLuiImageEvent read FOnAfterDraw write FOnAfterDraw;
    property OnBeforeDraw: TLuiImageEvent read FOnBeforeDraw write FOnBeforeDraw;
    property OnDrawClipPath: TLuiImageEvent read FOnDrawClipPath write FOnDrawClipPath;
    property OnGetPattern: TLuiImageGetPattern read FOnGetPattern write FOnGetPattern;
    property Options: TLuiImageOptions read FOptions write SetOptions;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Patterns: TLuiImagePatterns read FPatterns;
    property Picture: TPicture read FPicture;
    property RoundEdgeRadius: Integer read FRoundEdgeRadius write SetRoundEdgeRadius;
    property ViewStyle: TLuiImageViewStyle read FViewStyle write SetViewStyle;
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

procedure TLuiImage.Changed;
begin
  if (FUpdateCount = 0) and not (csLoading in ComponentState) then
  begin
    if lisAutoSizePending in FStates then
      InternalAutoSize;
    Redraw;
  end;
end;

function TLuiImage.ImageWidth: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FCurrentBitmap.Width;
    livAutoScale:
      Result := Width - FPadding.Left - FPadding.Right - (FOutLineWidth * 2);
  end;
end;

function TLuiImage.ImageHeight: Integer;
begin
  case FViewStyle of
    livNormal:
      Result := FCurrentBitmap.Height;
    livAutoScale:
      Result := Height - FPadding.Top - FPadding.Bottom - (FOutLineWidth * 2);
  end;
end;

procedure TLuiImage.InternalAutoSize;
var
  DesiredWidth, DesiredHeight: Integer;
begin
  //todo: move to default LCL AutoSize ????
  DesiredWidth := FCurrentBitmap.Width + FPadding.Left +
    FPadding.Right + (FOutLineWidth * 2);
  DesiredHeight := FCurrentBitmap.Height + FPadding.Top +
    FPadding.Bottom + (FOutLineWidth * 2);
  if (DesiredHeight <> Height) or (DesiredWidth <> Width) then
    SetBounds(Left, Top, DesiredWidth, DesiredHeight);
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

procedure TLuiImage.SetOptions(AValue: TLuiImageOptions);
begin
  if FOptions = AValue then exit;
  if lioAutoSize in (AValue - FOptions) then
    Include(FStates, lisAutoSizePending);
  FOptions := AValue;
  Changed;
end;

procedure TLuiImage.SetOutLineWidth(AValue: Integer);
begin
  if FOutLineWidth = AValue then exit;
  FOutLineWidth := AValue;
  if lioAutoSize in FOptions then
    Include(FStates, lisAutoSizePending);
  Changed;
end;

procedure TLuiImage.SetPadding(const AValue: TRect);
begin
  if CompareRect(@FPadding, @AValue) then exit;
  FPadding := AValue;
  if lioAutoSize in FOptions then
    Include(FStates, lisAutoSizePending);
  Changed;
end;

procedure TLuiImage.SetRoundEdgeRadius(AValue: Integer);
begin
  if FRoundEdgeRadius = AValue then exit;
  FRoundEdgeRadius := AValue;
  Changed;
end;

procedure TLuiImage.SetViewStyle(AValue: TLuiImageViewStyle);
begin
  if FViewStyle = AValue then exit;
  FViewStyle := AValue;
  Changed;
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
  if FPatterns.RequiresUpdate then
    UpdatePatterns;
  if FImageSurface = nil then
    Exit;
  with Context do
  begin
    Save;
    DoBeforeDraw;
    DoSetSource;
    Translate(FPadding.Left, FPadding.Top);
    Paint;
    DoAfterDraw;
    Restore;
  end;
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
    RoundedRectangle(Context, PosOffset, PosOffset,
      ImageWidth + FOutLineWidth, ImageHeight + FOutLineWidth, FRoundEdgeRadius);
  end;
end;

procedure TLuiImage.DoSetSource;
var
  TempPattern: TCairoPattern;
  Matrix: TCairoMatrix;
begin
  if (FViewStyle = livNormal) or (lioAutoSize in FOptions) then
    Context.SetSourceSurface(FImageSurface, FOutLineWidth, FOutLineWidth)
  else
    case FViewStyle of
      livAutoScale:
        begin
          TempPattern := TCairoSurfacePattern.Create(FImageSurface);
          Matrix.InitScale(FCurrentBitmap.Width /  ImageWidth,
            FCurrentBitmap.Height / ImageHeight);
          Matrix.Translate(-FOutLineWidth, -FOutLineWidth);
          TempPattern.SetMatrix(Matrix);
          Context.Source := TempPattern;
          TempPattern.Destroy;
        end;
    end;
end;

constructor TLuiImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPatterns := TLuiImagePatterns.Create;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  with FColors do
  begin
    OutLine := clBlack;
    Background := clNone;
  end;
  SetInitialBounds(0, 0, 105, 105);
end;

destructor TLuiImage.Destroy;
begin
  FPatterns.Destroy;
  FPicture.Destroy;
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
  //Context.Source := FPatterns.BackGround;
  //Context.FillPreserve;
  Context.Clip;
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

end.

