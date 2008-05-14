unit CairofpGui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpgfx, CairoClasses;

type

  { TCairofpgSurface }

  TCairofpgSurface = class(TCairoSurface)
  public
    constructor Create(Canvas: TfpgCanvas);
  end;
  
  function CreateSurfaceFromCanvas(Canvas: TfpgCanvas): Pcairo_surface_t;
  
implementation

{ TCairofpgSurface }

constructor TCairofpgSurface.Create(Canvas: TfpgCanvas);
begin
  FHandle := CreateSurfaceFromCanvas(Canvas);
end;

end.

