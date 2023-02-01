unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, SynEdit, SynHighlighterPas, Buttons, LCLProc;

type

  { TFormMain }

  TFormMain = class(TForm)
    ListSnippets: TListBox;
    PaintBoxMain: TPaintBox;
    SynPasSyn1: TSynPasSyn;
    SynSnippetCode: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure ListSnippetsSelectionChange (Sender: TObject; User: boolean );
    procedure PaintBoxMainPaint(Sender: TObject);
    procedure EraseBackground(DC: HDC); override;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  Cairo, CairoLCL, snippets, InterfaceBase, LCLPlatformDef;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  i, major, minor, micro: Integer;
begin
  cairo_version(major, minor, micro);
  Caption := Caption + Format(' - Cairo Version %d.%d.%d [%s]',
    [major, minor, micro, LCLPlatformDirNames[WidgetSet.LCLPlatform]]);
  SetCurrentDir('..');
  for i := 0 to snippet_count - 1 do
    ListSnippets.Items.Add(snippet_name[i]);
  ListSnippets.ItemIndex := 0;
end;

procedure TFormMain.ListSnippetsSelectionChange (Sender: TObject; User: boolean );
begin
  PaintBoxMain.Invalidate;
  if FileExists('snippets' + PathDelim + ListSnippets.Items[ListSnippets.ItemIndex] + '.cairo') then
    SynSnippetCode.Lines.LoadFromFile('snippets' + PathDelim + ListSnippets.Items[ListSnippets.ItemIndex] + '.cairo');
end;

procedure TFormMain.PaintBoxMainPaint(Sender: TObject);
var
  surface: Pcairo_surface_t;
  cr: Pcairo_t;
  Buffer: TBitmap;
  w, h: Integer;
begin
  w := PaintBoxMain.Width;
  h := PaintBoxMain.Height;

  Buffer := TBitmap.Create;
  Buffer.SetSize(w, h);

  surface := CreateSurfaceFromDC(Buffer.Canvas.Handle);
  cr := cairo_create (surface);

  cairo_save(cr);
  cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_paint(cr);
  cairo_restore(cr);

  snippet_do(cr, ListSnippets.ItemIndex, w, h);

  PaintBoxMain.Canvas.Draw(0, 0, Buffer);

  cairo_destroy(cr);
  cairo_surface_destroy(surface);

  Buffer.Destroy;
end;

procedure TFormMain.EraseBackground (DC: HDC );
var
  ARect: TRect;
begin
  //avoid flickering in PaintBox under win32
  if DC = 0 then
    Exit;
  with PaintBoxMain do
    ExcludeClipRect(Dc, Left, Top, Left + Width, Top + Height);
  ARect := Rect(0, 0, Width,Height);
  FillRect(DC, ARect, Brush.Handle);
  SelectClipRGN(DC, 0);
end;

initialization
  {$I main.lrs}

end.

