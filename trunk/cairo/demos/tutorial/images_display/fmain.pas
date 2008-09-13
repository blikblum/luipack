unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  CairoLCL, CairoClasses;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    procedure CairoPaintBoxDraw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FImage: TCairoImageSurface;
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.CairoPaintBoxDraw(Sender: TObject);
begin
  with CairoPaintBox.Context do
  begin
    SetSourceSurface(FImage, 10, 10);
    Paint;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ImagePath: String;
begin
  ImagePath := '..' + PathDelim + 'images' + PathDelim + 'plaveckycastle.png';
  FImage := TCairoImageSurface.Create(ImagePath);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FImage.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

