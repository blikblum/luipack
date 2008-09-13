unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, CpuWidget,
  ComCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FCpu: TCpuWidget;
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FCpu := TCpuWidget.Create(Self);
  with FCpu do
  begin
    Parent := Self;
    SetBounds(10, 10, 80, 100);
    Scale := TrackBar1.Max;
  end;
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  //invert the trackbar position
  FCpu.Value := (TrackBar1.Max - TrackBar1.Position);
end;

initialization
  {$I fmain.lrs}

end.

