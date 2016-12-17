unit BackupRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSqliteBackupRunner }

  TSqliteBackupRunner = class
  private
    FError: String;
    FOutput: String;
  public
    function Execute(const Database: String): Boolean;
    property Error: String read FError;
    property Output: String read FOutput;
  end;

implementation

uses
  Sqlite3, ctypes, MultiLog;

{ TSqliteBackupRunner }

function TSqliteBackupRunner.Execute(const Database: String): Boolean;
var
  SourceDb, TargetDb: psqlite3;
  Backup: psqlite3backup;
  rc: cint;
  StartTime, EndTime: TDateTime;
begin
  Result := False;
  FOutput := '';
  FError := '';
  TargetDb := nil;
  SourceDb := nil;
  if not FileExists(Database) then
    raise Exception.CreateFmt('Database not found: "%s"', [Database]);
  rc := sqlite3_open_v2(PAnsiChar(Database), @SourceDb, SQLITE_OPEN_READONLY, nil);
  if rc <> SQLITE_OK then
    raise Exception.CreateFmt('Error opening database: "%s"', [Database]);
  try
    FOutput := GetTempFileName('', 'sqlite-backup');
    rc := sqlite3_open_v2(PAnsiChar(FOutput), @TargetDb, SQLITE_OPEN_CREATE or SQLITE_OPEN_READWRITE, nil);
    if rc <> SQLITE_OK then
      raise Exception.CreateFmt('Error creating backup file "%s" - %s', [FOutput, sqlite3_errmsg(TargetDb)]);
     //* Open the sqlite3_backup object used to accomplish the transfer */
    Backup := sqlite3_backup_init(TargetDb, 'main', SourceDb, 'main');
    if Backup <> nil then
    begin
      Logger.Send([lcInfo], 'Backup started (%s)', [Database]);
      StartTime := Time;
      repeat
        rc := sqlite3_backup_step(Backup, -1);
      until not ((rc = SQLITE_OK) or (rc = SQLITE_BUSY) or (rc = SQLITE_LOCKED));
      sqlite3_backup_finish(Backup);
      EndTime := Time;
      Logger.Send([lcInfo], 'Backup finished - Time: ' + FormatDateTime('nn:ss:zzz', EndTime - StartTime));
    end;
    rc := sqlite3_errcode(TargetDb);
    Result := rc = SQLITE_OK;
    if not Result then
    begin
      FError := String(sqlite3_errmsg(TargetDb));
      DeleteFile(FOutput);
      FOutput := '';
     end;
  finally
    sqlite3_close(TargetDb);
    sqlite3_close(SourceDb);
  end;
end;

end.

