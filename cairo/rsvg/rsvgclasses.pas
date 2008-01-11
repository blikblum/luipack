unit RsvgClasses;

{$mode objfpc}{$H+}

interface

uses
  CairoClasses, rsvg;

type

  { TRsvgFile }

  TRsvgFile = class
  private
    FHandle: PRsvgHandle;
    procedure FreeHandle;
  public
    destructor Destroy; override;
    procedure GetDimensions(const DimensionData: RsvgDimensionData);
    procedure Load(const FileName: String);
    procedure RenderToCairo(Context: TCairoContext);
    property Handle: PRsvgHandle read FHandle;
  end;

implementation

uses
  glib2;

{ TRsvgFile }

procedure TRsvgFile.FreeHandle;
begin
  if FHandle <> nil then
  begin
    g_object_unref(FHandle);
    FHandle := nil;
  end;
end;

destructor TRsvgFile.Destroy;
begin
  FreeHandle;
end;

procedure TRsvgFile.GetDimensions(const DimensionData: RsvgDimensionData);
begin
  rsvg_handle_get_dimensions(FHandle, @DimensionData);
end;

procedure TRsvgFile.Load(const FileName: String);
begin
  FreeHandle;
  //todo: add error handling
  FHandle := rsvg_handle_new_from_file(PChar(FileName), nil);
end;

procedure TRsvgFile.RenderToCairo(Context: TCairoContext);
begin
  rsvg_handle_render_cairo(FHandle, Context.Handle);
end;

initialization
  rsvg_init;

finalization
  rsvg_term;

end.

