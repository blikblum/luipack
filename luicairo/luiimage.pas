unit LuiImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoClasses, CairoLCL, Graphics;

type

  TLuiImage = class;
  
  TLuiImageEvent = procedure (Sender: TLuiImage) of object;

  { TLuiImage }

  TLuiImage = class(TCustomCairoControl)
  private
    FCurrentBitmap: TBitmap;
    FImageSurface: TCairoDCSurface;
    FOnAfterDraw: TLuiImageEvent;
    FOnBeforeDraw: TLuiImageEvent;
    FOnDrawClipPath: TLuiImageEvent;
    FOutLineWidth: Integer;
    FPicture: TPicture;
    procedure PictureChanged(Sender: TObject);
    procedure ResetImageSurface;
    procedure SetOnAfterDraw(const AValue: TLuiImageEvent);
    procedure SetOnBeforeDraw(const AValue: TLuiImageEvent);
    procedure SetOnDrawClipPath(const AValue: TLuiImageEvent);
    procedure SetOutLineWidth(const AValue: Integer);
  protected
    procedure DoAfterDraw; virtual;
    procedure DoBeforeDraw; virtual;
    procedure DoDraw; override;
    procedure DoDrawClipPath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultAfterDraw;
    procedure DefaultBeforeDraw;

    property Context;
  published
    property OnAfterDraw: TLuiImageEvent read FOnAfterDraw write SetOnAfterDraw;
    property OnBeforeDraw: TLuiImageEvent read FOnBeforeDraw write SetOnBeforeDraw;
    property OnDrawClipPath: TLuiImageEvent read FOnDrawClipPath write SetOnDrawClipPath;
    property OutLineWidth: Integer read FOutLineWidth write SetOutLineWidth;
    property Picture: TPicture read FPicture;
  end;

implementation

procedure RoundedRectangle(Context: TCairoContext; X, Y, Width, Height: Double; Radius: Double = 10);
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

procedure TLuiImage.SetOutLineWidth(const AValue: Integer);
begin
  if FOutLineWidth=AValue then exit;
  FOutLineWidth:=AValue;
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
  with Context do
  begin
    Save;
    DoBeforeDraw;
    SetSourceSurface(FImageSurface, 0, 0);
    Paint;
    DoAfterDraw;
    Restore;
  end;
end;

procedure TLuiImage.DoDrawClipPath;
begin
  if Assigned(FOnDrawClipPath) then
    FOnDrawClipPath(Self)
  else
    RoundedRectangle(Context, 0, 0, FCurrentBitmap.Width, FCurrentBitmap.Height);
end;

constructor TLuiImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  SetInitialBounds(0, 0, 105, 105);
end;

destructor TLuiImage.Destroy;
begin
  FPicture.Destroy;
  inherited Destroy;
  FImageSurface.Free;
end;

procedure TLuiImage.DefaultAfterDraw;
begin
  if FOutLineWidth > 0 then
  begin
    Context.ResetClip;
    DoDrawClipPath;
    Context.SetSourceRgb(0,0,0);
    Context.LineWidth := FOutLineWidth;
    Context.Stroke;
  end;
end;

procedure TLuiImage.DefaultBeforeDraw;
begin
  DoDrawClipPath;
  Context.Clip;
end;

end.

