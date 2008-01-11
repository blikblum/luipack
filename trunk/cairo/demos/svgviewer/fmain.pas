unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CairoLCL, RsvgClasses, CairoClasses, rsvg,
  EditBtn, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CairoControl1: TCairoControl;
    TopPanel: TPanel;
    SvgFileEdit: TFileNameEdit;
    procedure CairoControl1Draw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SvgFileEditAcceptFileName(Sender: TObject; var Value: String);
  private
    { private declarations }
    FSvgFile: TRsvgFile;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.CairoControl1Draw(Sender: TObject);
var
  Dimensions: RsvgDimensionData;
begin
  with CairoControl1, Context do
  begin
    if FSvgFile.Handle <> nil then
    begin
      Rectangle(0, 0 , Width, Height);
      Stroke;
      FSvgFile.GetDimensions(Dimensions);
      Scale(Width / Dimensions.width, Height / Dimensions.Height);
      FSvgFile.RenderToCairo(Context);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSvgFile := TRsvgFile.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSvgFile.Destroy;
end;

procedure TForm1.SvgFileEditAcceptFileName(Sender: TObject; var Value: String);
begin
  FSvgFile.Load(Value);
  CairoControl1.Redraw;
end;

initialization
  {$I fmain.lrs}

end.

