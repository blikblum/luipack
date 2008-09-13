unit CpuWidget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CairoLCL;

type

  { TCpuWidget }

  TCpuWidget = class (TCustomCairoControl)
  private
    FScale: Integer;
    FValue: Integer;
    procedure SetScale(const AValue: Integer);
    procedure SetValue(const AValue: Integer);
  protected
    procedure DoDraw; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Scale: Integer read FScale write SetScale;
    property Value: Integer read FValue write SetValue;
  end;

implementation

{ TCpuWidget }

procedure TCpuWidget.SetScale(const AValue: Integer);
begin
  if (FScale=AValue) or (AValue < 20) then exit;
  FScale:=AValue;
end;

procedure TCpuWidget.SetValue(const AValue: Integer);
begin
  if (FValue=AValue) or (AValue < 0) then exit;
  FValue:=AValue;
  Redraw;
end;

procedure TCpuWidget.DoDraw;
var
  RectNum, i: Integer;
begin
  with Context do
  begin
    Save;
    Translate(0, 7);

    SetSourceRgb(0, 0, 0);
    Paint;

    RectNum := FValue div (FScale div 20);

    for i := 1 to  20 do
    begin
      if (i > (20 - RectNum)) then
          SetSourceRgb(0.6, 1.0, 0)
      else
          SetSourceRgb(0.2, 0.4, 0);

      Rectangle(8, i*4, 30, 3);
      Rectangle(42, i*4, 30, 3);
      Fill;
    end;
    Restore;
  end;
end;

constructor TCpuWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScale := 20;
end;

end.

