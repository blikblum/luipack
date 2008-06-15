unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, snippets, LCLType, SynEdit, SynHighlighterPas, Buttons, LCLProc;

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
  cairo14, CairoLCL, InterfaceBase;

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
  TempDC: HDC;
  TempBitmap, OldBitmap: HBITMAP;
begin
  TempDC := CreateCompatibleDC(PaintBoxMain.Canvas.Handle);
  {$ifdef LCLQt}
  TempBitmap := CreateBitmap(PaintBoxMain.Width, PaintBoxMain.Height, 1, 32, nil);
  {$else}
  TempBitmap := CreateCompatibleBitmap(PaintBoxMain.Canvas.Handle, PaintBoxMain.Width, PaintBoxMain.Height);
  {$endif}
  OldBitmap := SelectObject(TempDC, TempBitmap);

  surface := CreateSurfaceFromDC(TempDC);
  cr := cairo_create (surface);

  cairo_save(cr);
  cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_paint(cr);
  cairo_restore(cr);
  
  snippet_do (cr, ListSnippets.ItemIndex, PaintBoxMain.Width, PaintBoxMain.Height);

  BitBlt(PaintBoxMain.Canvas.Handle, 0, 0, PaintBoxMain.Width, PaintBoxMain.Height,
    TempDC, 0, 0, SRCCOPY);

  cairo_destroy (cr);
  cairo_surface_destroy (surface);

  SelectObject(TempDC, OldBitmap);
  DeleteDC(TempDC);
  DeleteObject(TempBitmap);
end;

procedure TFormMain.EraseBackground (DC: HDC );
var
  ARect: TRect;
begin
  if DC=0 then exit;
  with PaintBoxMain do
    ExcludeClipRect(Dc, Left, Top, Left + Width, Top + Height);
  ARect:=Rect(0,0,Width,Height);
  FillRect(DC,ARect,Brush.Handle);
  SelectClipRGN(DC, 0);
end;

initialization
  {$I main.lrs}

end.

