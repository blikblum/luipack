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
var
  i: Integer;
begin
  with CairoPaintBox.Context do
  begin
    for i := 1 to 10 do
    begin
      SetSourceRgba(0, 0, 1, i * 0.1);
      Rectangle(50 * i, 20, 40, 40);
      Fill;
    end;
  end;
end;

initialization
  {$I fmain.lrs}

end.

