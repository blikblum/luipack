unit SimpleIPCWrapper;

//SimpleIPC is buggy under unix. Since there's no way to extend directly (bug 19136)
// use this workaround

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC;

procedure InitServer(Server: TSimpleIPCServer);

function IsServerRunning(Client: TSimpleIPCClient): Boolean;

procedure ShutDownServer(Server: TSimpleIPCServer);

implementation

{$ifdef unix}
uses
  BaseUnix;

const
  F_RDLCK = 0;
  F_WRLCK = 1;
  F_UNLCK = 2;

function GetPipeFileName(Server: TSimpleIPCServer): String;
begin
  Result := Server.ServerID;
  if not Server.Global then
    Result := Result + '-' + IntToStr(fpGetPID);
  Result := '/tmp/' + Result;
end;

function GetPipeFileName(Client: TSimpleIPCClient): String;
begin
  Result := Client.ServerID;
  if Client.ServerInstance <> '' then
    Result := Result + '-' + Client.ServerInstance;
  Result := '/tmp/' + Result;
end;

function SetLock(FileDescriptor: cint): Boolean;
var
  LockInfo: FLock;
begin
  LockInfo.l_type := F_WRLCK;
  LockInfo.l_whence := SEEK_SET;
  LockInfo.l_len := 0;
  LockInfo.l_start := 0;
  Result := FpFcntl(FileDescriptor, F_SetLk, LockInfo) <> -1;
end;

procedure InitServer(Server: TSimpleIPCServer);
var
  LockFileName: String;
  LockDescriptor: cint;
begin
  Server.StartServer;
  LockFileName := GetPipeFileName(Server) + '.lck';
  LockDescriptor := FpOpen(LockFileName, O_RDWR, $1B6);
  if LockDescriptor <> -1 then
  begin
    //Lock FileExists
    if not SetLock(LockDescriptor) then
    begin
      FpClose(LockDescriptor);
      raise Exception.CreateFmt('UniqueInstance - Server %s is already running', [Server.ServerID]);
    end;
  end
  else
  begin
    LockDescriptor := FpOpen(LockFileName, O_EXCL or O_RDWR or O_CREAT, $1B6);
    if LockDescriptor = -1 then
      raise Exception.CreateFmt('UniqueInstance - Error creating lock file for server %s', [Server.ServerID])
    else if not SetLock(LockDescriptor) then
    begin
      FpClose(LockDescriptor);
      raise Exception.CreateFmt('UniqueInstance - Error setting lock for server %s', [Server.ServerID]);
    end;
  end;
end;

function IsServerRunning(Client: TSimpleIPCClient): Boolean;
var
  LockFileName: String;
  LockDescriptor: cint;
begin
  Result := Client.ServerRunning;
  if Result then
  begin
    //check the lock
    LockFileName := GetPipeFileName(Client) + '.lck';
    LockDescriptor := FpOpen(LockFileName, O_RDWR, $1B6);
    Result := LockDescriptor <> -1;
    if Result then
    begin
      // lock file exists
      // if is possible to create a lock then the process that created the lock is not running
      Result := not SetLock(LockDescriptor);
      FpClose(LockDescriptor);
      if not Result then
      begin
        //delete lock file
        FpUnlink(LockFileName);
      end;
    end
    else
    begin
      // lock file does NOT exists
      // delete the pipe ??
      //FpUnlink(GetPipeFileName(Client));
    end;
  end;
end;

procedure ShutDownServer(Server: TSimpleIPCServer);
var
  LockFileName: String;
  LockDescriptor: cint;
begin
  Server.StopServer;
  LockFileName := GetPipeFileName(Server) + '.lck';
  LockDescriptor := FpOpen(LockFileName, O_RDWR, $1B6);
  //this should remove the lock
  FpClose(LockDescriptor);
  //delete lock file
  FpUnlink(LockFileName);
end;

{$else}

procedure InitServer(Server: TSimpleIPCServer);
begin
   Server.StartServer;
end;

function IsServerRunning(Client: TSimpleIPCClient): Boolean;
begin
  Result := Client.ServerRunning;
end;

procedure ShutDownServer(Server: TSimpleIPCServer);
begin
  Server.StopServer;
end;

{$endif}


end.

