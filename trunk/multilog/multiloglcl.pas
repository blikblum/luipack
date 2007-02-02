unit multiloglcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, multilog;

type

  { TLCLLogger }

  TLCLLogger = class(TLogger)
  private
  public
    procedure SendBitmap(const AText: String; ABitmap: TBitmap); //inline;
    procedure SendBitmap(AClass: TDebugClass; const AText: String; ABitmap: TBitmap);
  end;

implementation

{ TLCLLogger }

procedure TLCLLogger.SendBitmap(const AText: String; ABitmap: TBitmap);
begin
  SendBitmap(DefaultClass,AText,ABitmap);
end;

procedure TLCLLogger.SendBitmap(AClass: TDebugClass; const AText: String;
  ABitmap: TBitmap);
var
  AStream: TStream;
begin
  if not (AClass in ActiveClasses) then Exit;
  if ABitmap <> nil then
  begin
    AStream:=TMemoryStream.Create;
    ABitmap.SaveToStream(AStream);
  end
  else
    AStream:=nil;
  //SendStream free AStream
  SendStream(ltBitmap,AText,AStream);
end;

end.

