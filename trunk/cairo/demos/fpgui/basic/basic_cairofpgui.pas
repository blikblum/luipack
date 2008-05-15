program basic_cairofpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  gfxbase, fpgfx, gui_form, gfx_imgfmt_bmp, CairofpGui, CairoClasses, CairoUtils, cairo14;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
  protected
  public
    PaintBox: TfpgCairoPaintBox;
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    procedure PaintBoxDraw(Sender: TObject);
  end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PaintBox := TfpgCairoPaintBox.Create(Self);
  with PaintBox do
  begin
    Parent := Self;
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Visible := True;
    Align := alClient;
    OnDraw := @PaintBoxDraw;
  end;
end;

procedure TMainForm.PaintBoxDraw(Sender: TObject);
var
  LinearGradient: TCairoLinearGradient;
  Extents: cairo_text_extents_t;
  AText: String;
begin
  with PaintBox, Context do
  begin
    //draws a sharp white line
    LineWidth := 1;
    RoundedRectangle(Context, 0.5, 0.5, Width - 1, Height - 1, 10);
    SetSourceRgb(1, 1, 1);
    Stroke;

    //draws a inner black line and fill with a semi transparent color
    RoundedRectangle(Context, 1.5, 1.5, Width - 3, Height - 3, 10);
    SetSourceRgba(0, 0, 0, 100/255);
    FillPreserve;
    SetSourceRgb(0, 0, 0);
    Stroke;

    //shine the upper
    RoundedRectangle(Context, 2.5, 2.5, Width - 5, (Height - 4)/ 2, 10, 0);
    LinearGradient := TCairoLinearGradient.Create(0,0,0, Height /2);
    LinearGradient.AddColorStopRgba(0, 1, 1, 1, 153/255);
    LinearGradient.AddColorStopRgba(1, 1, 1, 1, (153/255)/3);
    Source := LinearGradient;
    Fill;
    LinearGradient.Destroy;
    
    //Some text
    AText := 'Welcome To Cairo + FpGui';
    FontSize := 20;
    SelectFontFace('Sans', CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_WEIGHT_NORMAL);
    SetSourceRgb(1, 0, 0);
    TextExtents(AText, @Extents);
    MoveTo((Width - Extents.Width) / 2, ((Height - Extents.Height) / 2) + Extents.Height);
    ShowText(AText);
  end;
end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(316, 186, 400, 100);
  WindowTitle := 'Basic Cairo Demo';
  WindowPosition := wpScreenCenter;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


