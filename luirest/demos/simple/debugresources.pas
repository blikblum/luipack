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
    procedure HandlePatch(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePost(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandlePut(ARequest: TRequest; AResponse: TResponse); override;
    procedure HandleDelete(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

procedure LoadEnviroment(const AResponse: TResponse);
var
  Count: Integer;
  i: Integer;
begin
  Count := GetEnvironmentVariableCount;
  for i := 1 to Count do
    AResponse.Contents.Add(GetEnvironmentString(i));
  AResponse.ContentType := 'text/plain';
end;

{ TEnvDump }


procedure TEnvDump.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  LoadEnviroment(AResponse);
end;

procedure TEnvDump.HandlePatch(ARequest: TRequest; AResponse: TResponse);
begin
  LoadEnviroment(AResponse);
end;

procedure TEnvDump.HandlePost(ARequest: TRequest; AResponse: TResponse);
begin
  LoadEnviroment(AResponse);
end;

procedure TEnvDump.HandlePut(ARequest: TRequest; AResponse: TResponse);
begin
  LoadEnviroment(AResponse);
end;

procedure TEnvDump.HandleDelete(ARequest: TRequest; AResponse: TResponse);
begin
  LoadEnviroment(AResponse);
end;

end.

