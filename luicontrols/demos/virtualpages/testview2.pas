unit TestView2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TTestView2Frame }

  TTestView2Frame = class(TFrame)
    Button1: TButton;
  private
    procedure SetCaption(AValue: String);
    { private declarations }
  public
    { public declarations }
    procedure Paint; override;
  published
    property Caption: String write SetCaption;
  end;

implementation

{$R *.lfm}

{ TTestView2Frame }

procedure TTestView2Frame.SetCaption(AValue: String);
begin
  Button1.Caption := AValue;
end;

procedure TTestView2Frame.Paint;
begin
  inherited Paint;
  Canvas.Frame(ClientRect);
end;

initialization
  RegisterClass(TTestView2Frame);

end.

