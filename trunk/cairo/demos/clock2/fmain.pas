unit fmain;

{$mode objfpc}{$H+}

{Based in JanaGtk Clock widget}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, JanaClock, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckShowSeconds: TCheckBox;
    CheckDrawShadow: TCheckBox;
    CheckDigital: TCheckBox;
    PanelOptions: TPanel;
    TimerClock: TTimer;
    procedure CheckDigitalChange(Sender: TObject);
    procedure CheckDrawShadowChange(Sender: TObject);
    procedure CheckShowSecondsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
  private
    { private declarations }
    FClock: TJanaClock;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  FClock := TJanaClock.Create(Self);
  with FClock do
  begin
    Parent := Self;
    Time := SysUtils.Time;
    Align := alClient;
  end;
end;

procedure TForm1.CheckDigitalChange(Sender: TObject);
begin
  FClock.Digital := CheckDigital.Checked;
end;

procedure TForm1.CheckDrawShadowChange(Sender: TObject);
begin
  FClock.DrawShadow := CheckDrawShadow.Checked;
end;

procedure TForm1.CheckShowSecondsChange(Sender: TObject);
begin
  FClock.ShowSeconds := CheckShowSeconds.Checked;
end;

procedure TForm1.TimerClockTimer(Sender: TObject);
begin
  FClock.Time := Time;
end;

initialization
  {$I fmain.lrs}

end.

