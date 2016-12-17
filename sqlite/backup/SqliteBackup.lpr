program SqliteBackup;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fpjson,
  LuiJSONUtils, LuiJSONHelpers, MultiLog, FileChannel,
  BackupTask, FTPQueue, PasswordUtils;

type

  { TSqliteBackupApplication }

  TSqliteBackupApplication = class(TCustomApplication)
  protected
    FTaskList: TStrings;
    FDefaultsData: TJSONObject;
    FFTPQueue: TFTPQueue;
    FConfigData: TJSONObject;
    function ExecuteTask(const TaskName: String; TaskData: TJSONObject): Boolean;
    procedure ExecuteTasks;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSqliteBackupApplication }

function TSqliteBackupApplication.ExecuteTask(const TaskName: String; TaskData: TJSONObject): Boolean;
var
  Task: TSqliteBackupTask;
  FTPData: TJSONObject;
begin
  Task := TSqliteBackupTask.Create;
  Task.Name := TaskName;
  Task.Data := TaskData;
  try
    Logger.EnterMethod([lcInfo], 'ExecuteTask', TaskName + ' task');
    try
      Result := Task.Execute;
      if Result then
      begin
        if TaskData.FindPath('targets.ftp', FTPData) then
          FFTPQueue.Add(TaskName, Task.TargetPath, FTPData);
      end;
    except
      on E: Exception do
        Logger.Send([lcError], TaskName + ': ' +  E.Message);
    end;
  finally
    Logger.ExitMethod('ExecuteTask', ' ');
  end;
  Task.Destroy;
end;

procedure TSqliteBackupApplication.ExecuteTasks;
var
  TaskData: TJSONObject;
  TaskName: String;
  i: Integer;
begin
  for i := 0 to FTaskList.Count - 1 do
  begin
    TaskName := FTaskList[i];
    if FConfigData.FindPath('tasks.' + TaskName, TaskData) then
    begin
      if FDefaultsData <> nil then
        TaskData.Merge(FDefaultsData);
      ExecuteTask(TaskName, TaskData);
    end
    else
      Logger.Send([lcWarning], TaskName, ': task configuration not found');
  end;
end;

procedure TSqliteBackupApplication.DoRun;
var
  ErrorMsg: String;
begin
  Logger.Channels.Add(TFileChannel.Create(ExtractFilePath(ParamStr(0)) + 'backup.log'));
  Logger.Channels.Add(TFileChannel.Create('', []));
  Logger.ActiveClasses := lcAll;
  // quick check parameters
  ErrorMsg := CheckOptions('h', ['help', 'encode-password'], nil, FTaskList);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('encode-password') then
  begin
    if FTaskList.Count > 0 then
    begin
      WriteLn(EncodePassword(FTaskList[0]))
    end
    else
      WriteLn('Is necessary to specify a password to encode');
    Terminate;
    Exit;
  end;

  if (FTaskList.Count = 0) then
  begin
    Logger.Send([lcWarning], 'Is necessary to specify at least a backup task');
    Terminate;
    Exit;
  end;

  if not TryReadJSONFile(ExtractFilePath(ParamStr(0)) + 'config.json', FConfigData) then
  begin
    Logger.Send([lcWarning], 'Unable to read config.json file. Must be a valid JSON object');
    Terminate;
    Exit;
  end;

  { add your program here }
  FConfigData.Find('defaults', FDefaultsData);
  ExecuteTasks;
  FFTPQueue.Execute;

  // stop program loop
  Terminate;
end;

constructor TSqliteBackupApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FTaskList := TStringList.Create;
  FFTPQueue := TFTPQueue.Create(Self);
end;

destructor TSqliteBackupApplication.Destroy;
begin
  FConfigData.Free;
  FTaskList.Destroy;
  inherited Destroy;
end;

procedure TSqliteBackupApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TSqliteBackupApplication;
begin
  Application := TSqliteBackupApplication.Create(nil);
  Application.Title := 'Sqlite Backup';
  Application.Run;
  Application.Free;
end.

