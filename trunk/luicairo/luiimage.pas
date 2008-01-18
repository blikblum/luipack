unit LuiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, Graphics;

type

  TLuiImage = class;
  
  TLuiImageEvent = procedure (Sender: TLuiImage) of object;

  TLuiImageOption = (lioAutoSize, lioStretch);

  TLuiImageOptions = set of TLuiImageOption;

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
    FPatterns: TLuiImagePatterns;
    FPicture: TPicture;
    FRoundEdgeRadius: Integer;
    procedure InternalAutoSize;
    procedure PictureChanged(Sender: TObject);
    procedure ResetImageSurface;
    procedure SetOnAfterDraw(const AValue: TLuiImageEvent);
    procedure SetOnBeforeDraw(const AValue: TLuiImageEvent);
    procedure SetOnDrawClipPath(const AValue: TLuiImageEvent);
    procedure SetOnGetPattern(const AValue: TLuiImageGetPattern);
    procedure SetOptions(const AValue: TLuiImageOptions);
    procedure SetOutLineWidth(const AValue: Integer);
    procedure SetRoundEdgeRadius(const AValue: Integer);
    procedure UpdatePatterns;
  protected
    procedure DoAfterDraw; virtual;
    procedure DoBeforeDraw; virtual;
    procedure DoDraw; override;
    procedure DoDrawClipPath; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultAfterDraw;
    procedure DefaultBeforeDraw;
    property Colors: TLuiImageColors read FColors write FColors;
    property Context;
  published
    property OnAfterDraw: TLuiImageEvent read FOnAfterDraw write SetOnAfterDraw;
    property OnBeforeDraw: TLuiImageEvent read FOnBeforeDraw write SetOnBeforeDraw;
    property OnDrawClipPath: TLuiImageEvent read FOnDrawClipPath write SetOnDrawClipPath;
    property OnGetPattern: TLuiImageGetPattern read FOnGetPattern write SetOnGetPattern;
    property Options: TLuiImageOptions read FOptions write SetOptions;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Patterns: TLuiImagePatterns read FPatterns;
    property Picture: TPicture read FPicture;
    property RoundEdgeRadius: Integer read FRoundEdgeRadius write SetRoundEdgeRadius;
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

procedure TLuiImage.InternalAutoSize;
var
  DesiredWidth, DesiredHeight: Integer;
begin
  //todo: move to default LCL AutoSize ????
  DesiredWidth := FCurrentBitmap.Width + FOutLineWidth * 2;
  DesiredHeight := FCurrentBitmap.Height + FOutLineWidth * 2;
  if (DesiredHeight <> Height) or (DesiredWidth <> Width) then
    SetBounds(Left, Top, DesiredWidth, DesiredHeight);
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
    InternalAutoSize;
  Redraw;
end;

procedure TLuiImage.ResetImageSurface;
begin
  FImageSurface.Free;
  FImageSurface := TCairoDCSurface.Create(FCurrentBitmap.Canvas.Handle);
end;

procedure TLuiImage.SetOnAfterDraw(const AValue: TLuiImageEvent);
begin
  if FOnAfterDraw=AValue then exit;
  FOnAfterDraw:=AValue;
end;

procedure TLuiImage.SetOnBeforeDraw(const AValue: TLuiImageEvent);
begin
  if FOnBeforeDraw=AValue then exit;
  FOnBeforeDraw:=AValue;
end;

procedure TLuiImage.SetOnDrawClipPath(const AValue: TLuiImageEvent);
begin
  if FOnDrawClipPath=AValue then exit;
  FOnDrawClipPath:=AValue;
end;

procedure TLuiImage.SetOnGetPattern(const AValue: TLuiImageGetPattern);
begin
  if FOnGetPattern=AValue then exit;
  FOnGetPattern:=AValue;
end;

procedure TLuiImage.SetOptions(const AValue: TLuiImageOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TLuiImage.SetOutLineWidth(const AValue: Integer);
begin
  if FOutLineWidth=AValue then exit;
  FOutLineWidth:=AValue;
end;

procedure TLuiImage.SetRoundEdgeRadius(const AValue: Integer);
begin
  if FRoundEdgeRadius=AValue then exit;
  FRoundEdgeRadius:=AValue;
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
    SetSourceSurface(FImageSurface, FOutLineWidth, FOutLineWidth);
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
      FCurrentBitmap.Width + FOutLineWidth, FCurrentBitmap.Height + FOutLineWidth, FRoundEdgeRadius);
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
    Background := clWhite;
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

