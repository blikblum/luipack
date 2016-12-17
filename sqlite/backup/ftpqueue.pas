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
  ftpsend, blcksock, PasswordUtils, LuiJSONHelpers, MultiLog;

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
    Logger.EnterMethod([lcInfo], 'execftp', FTPData.Get('taskname', '') + ' task');
    Logger.Send([lcInfo], 'Upload started');
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
        Logger.Send([lcInfo], 'FTP upload finished - Time ', FormatDateTime('nn:ss:zzz', EndTime - StartTime));
    end;
    if not Result then
    begin
      Logger.Send([lcWarning], 'FTP upload failed');
      Logger.Send([lcWarning], '  Result code: ' + IntToStr(FTP.ResultCode) + ' - ' + FTP.ResultString);
      Logger.Send([lcWarning], '  Control sock code: ' + IntToStr(FTP.Sock.LastError) + ' - ' + FTP.Sock.GetErrorDescEx);
      Logger.Send([lcWarning], '  Data sock code: ' + IntToStr(FTP.DSock.LastError) + ' - ' + FTP.DSock.GetErrorDescEx);
    end;
    Logger.ExitMethod([lcInfo], 'execftp', ' ');
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
  if FData.Count > 0 then
  begin
    Logger.Send([lcInfo], 'FTP uploads' + LineEnding);
    for i := 0 to FData.Count - 1 do
      Upload(FData.Objects[i]);
  end;
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

