unit TestView1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TTestView1Frame }

  TTestView1Frame = class(TFrame)
    Label1: TLabel;
  private
    FCaption: String;
    procedure SetCaption(AValue: String);
    { private declarations }
  public
    procedure Paint; override;
  published
    property Caption: String read FCaption write SetCaption;
  end;

implementation

{$R *.lfm}

{ TTestView1Frame }

procedure TTestView1Frame.SetCaption(AValue: String);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  Label1.Caption := AValue;
end;

procedure TTestView1Frame.Paint;
begin
  inherited Paint;
  Canvas.Frame(ClientRect);
end;

end.

