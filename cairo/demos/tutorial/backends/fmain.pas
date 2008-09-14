unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL,
  CairoClasses, Cairo, StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSaveTo: TButton;
    CairoPaintBox: TCairoPaintBox;
    ComboSelectBackend: TComboBox;
    procedure ButtonSaveToClick(Sender: TObject);
    procedure CairoPaintBoxDraw(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

procedure DrawTo(Context: TCairoContext);
begin
  with Context do
  begin
    SetSourceRgb(0, 0, 0);
    SelectFontFace('Sans', CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
    FontSize := 40.0;
    MoveTo(10.0, 50.0);
    ShowText('Disziplin ist Macht.');
  end;
end;

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
begin
  DrawTo(CairoPaintBox.Context);
end;

procedure TFormMain.ButtonSaveToClick(Sender: TObject);
var
  Context: TCairoContext;
  Surface: TCairoSurface;
begin
  case ComboSelectBackend.ItemIndex of
    0 {pdf} : Surface := TCairoPDFSurface.Create('output.pdf', 504, 648);
    1 {png} : Surface := TCairoImageSurface.Create(CAIRO_FORMAT_ARGB32, 390, 60);
    2 {svg} : Surface := TCairoSVGSurface.Create('output.svg', 390, 60);
  end;
  
  Context := TCairoContext.Create(Surface);
  DrawTo(Context);
  
  case ComboSelectBackend.ItemIndex of
    0 {pdf} : Context.ShowPage;
    1 {png} : Surface.WriteToPng('output.png');
    2 {svg} :;
  end;

  Context.Destroy;
  Surface.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

