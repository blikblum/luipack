unit FTPQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  { TFTPQueue }

  TFTPQueue = class(TComponent)
  private
    FData: TJSONArray;
    function Upload(FTPData: TJSONObject): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Add(const TaskName, FilePath: String; FTPData: TJSONObject);
  end;

implementation

uses
  ftpsend, blcksock, PasswordUtils, LuiJSONHelpers;

procedure ConfigureSockProxy(Sock: TTCPBlockSocket; ProxyData: TJSONObject);
begin
  Sock.HTTPTunnelIP := ProxyData.Get('host', '');
  Sock.HTTPTunnelPort := ProxyData.Get('port', '');
  Sock.HTTPTunnelPass := ProxyData.Get('password', '');
  Sock.HTTPTunnelUser := ProxyData.Get('username', '');
end;

{ TFTPQueue }

function TFTPQueue.Upload(FTPData: TJSONObject): Boolean;
var
  FTP: TFTPSend;
  ProxyData: TJSONObject;
  FilePath: String;
  StartTime, EndTime: TDateTime;
begin
  Result := False;
  FTP := TFTPSend.Create;
  try
    FTP.TargetHost := FTPData.Get('host', '');
    FTP.TargetPort := FTPData.Get('port', '21');
    FTP.UserName := FTPData.Get('username', '');
    FTP.Password := DecodePassword(FTPData.Get('password', ''));
    if FTPData.Find('proxy', ProxyData) then
    begin
      ConfigureSockProxy(FTP.Sock, ProxyData);
      ConfigureSockProxy(FTP.DSock, ProxyData);
    end;
    Result := FTP.Login;
    StartTime := Time;
    if Result then
    begin
      Result := FTP.ChangeWorkingDir(FTPData.Get('directory', ''));
      if Result then
      begin
        FilePath := FTPData.Get('filepath', '');
        FTP.DirectFile := True;
        FTP.DirectFileName := FilePath;
        Result := FTP.StoreFile(ExtractFileName(FilePath), True);
        EndTime := Time;
      end;
      if Result then
        WriteLn('FTP upload finished: ', FormatDateTime('nn:ss:zzz', EndTime - StartTime));
    end;
    if not Result then
    begin
      WriteLn('FTP upload failed');
      WriteLn('  Result code: ', FTP.ResultCode, ' - ', FTP.ResultString);
      WriteLn('  Control sock code: ', FTP.Sock.LastError, ' - ', FTP.Sock.GetErrorDescEx);
      WriteLn('  Data sock code: ', FTP.DSock.LastError, ' - ', FTP.DSock.GetErrorDescEx);
    end;
  finally
    FTP.Destroy;
  end;
end;

constructor TFTPQueue.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TJSONArray.Create;
end;

destructor TFTPQueue.Destroy;
begin
  FData.Destroy;
  inherited Destroy;
end;

procedure TFTPQueue.Execute;
var
  i: Integer;
begin
  for i := 0 to FData.Count - 1 do
    Upload(FData.Objects[i]);
end;

procedure TFTPQueue.Add(const TaskName, FilePath: String; FTPData: TJSONObject);
var
  ItemData: TJSONObject;
begin
  ItemData := TJSONObject(FTPData.Clone);
  ItemData.Strings['filepath'] := FilePath;
  ItemData.Strings['taskname'] := TaskName;
  FData.Add(ItemData);
end;

end.

