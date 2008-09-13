unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL;

type

  { TFormMain }

  TFormMain = class(TForm)
    CairoPaintBox: TCairoPaintBox;
    procedure CairoPaintBoxDraw(Sender: TObject);
  private
    { private declarations }
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
    Save;
    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(20, 30, 80, 50);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
    Restore;

    Save;
    Translate(130, 30);
    Scale(0.7, 0.7);
    
    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(0, 0, 80, 50);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
    Restore;

    Save;
    Translate(220, 30);
    Scale(1.5, 1.5);

    SetSourceRgb(0.6, 0.6, 0.6);
    Rectangle(0, 0, 80, 50);
    StrokePreserve;
    SetSourceRgb(1, 1, 1);
    Fill;
    Restore;
  end;
end;

initialization
  {$I fmain.lrs}

end.

