unit uniqueinstanceraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc;
  
  function InstanceRunning(const Identifier: String): Boolean;

  function InstanceRunning: Boolean;

implementation

const
  BaseServerId = 'tuniqueinstance_';

var
  FIPCServer: TSimpleIPCServer;
  
function InstanceRunning(const Identifier: String): Boolean;

  function GetServerId: String;
  begin
    if Identifier <> '' then
      Result:=BaseServerId+Identifier
    else
      Result:=BaseServerId+ExtractFileName(ParamStr(0));
  end;
  
begin
  with TSimpleIPCClient.Create(nil) do
  try
    ServerId:=GetServerId;
    Result:=ServerRunning;
    if not Result then
    begin
      //It's the first instance. Init the server
      if FIPCServer = nil then
        FIPCServer:=TSimpleIPCServer.Create(nil);
      with FIPCServer do
      begin
        ServerID:=GetServerId;
        Global:=True;
        StartServer;
      end;
    end;
  finally
    Free;
  end;
end;

function InstanceRunning: Boolean;
begin
  InstanceRunning('');
end;

finalization  
  FIPCServer.Free;
end.

