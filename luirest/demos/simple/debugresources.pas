unit DebugResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, HTTPDefs;

type

  { TEnvDump }

  TEnvDump = class(TRESTResource)
  public
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

{ TEnvDump }

procedure TEnvDump.HandleGet(ARequest: TRequest; AResponse: TResponse);
var
  i, Count: Integer;
begin
  Count := GetEnvironmentVariableCount;
  for i := 0 to Count - 1 do
    AResponse.Contents.Add(GetEnvironmentString(i));
  AResponse.ContentType := 'text';
end;

end.

