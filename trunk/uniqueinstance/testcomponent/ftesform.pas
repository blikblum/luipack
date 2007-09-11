unit ftesform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  uniqueinstance, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    UniqueInstance1: TUniqueInstance;
    procedure UniqueInstance1OtherInstance(Sender: TObject; Count: Integer;
      Parameters: array of String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject; Count: Integer;
  Parameters: array of String);
var
  i:Integer;
begin
  Label1.Caption:='A new instance was created. '+ IntToStr(Count)+' Parameters';
  ListBox1.Clear;
  for i := 0 to Count - 1 do
    ListBox1.Items.Add(Parameters[i]);
  BringToFront;
end;

initialization
  {$I ftesform.lrs}

end.

