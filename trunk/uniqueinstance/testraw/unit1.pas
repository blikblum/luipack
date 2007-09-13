unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButCrashApp: TButton;
    procedure ButCrashAppClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ButCrashAppClick(Sender: TObject);
var
  d: Double;
  x: Integer;
begin
  x := 0;
  d := 1 / x;
end;

initialization
  {$I unit1.lrs}

end.

