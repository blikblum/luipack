unit BackupTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, BackupRunner;

type

  { TSqliteBackupTask }

  TSqliteBackupTask = class
  private
    FData: TJSONObject;
    FName: String;
    FTargetPath: String;
    function ReplaceTags(const Template: String): String;
    function StoreFile(const BackupFileName: String): Boolean;
  public
    function Execute: Boolean;
    property Data: TJSONObject read FData write FData;
    property Name: String read FName write FName;
    property TargetPath: String read FTargetPath;
  end;

implementation

uses
  LuiJSONHelpers, strutils, AbZipper, AbArcTyp;

{ TSqliteBackupTask }

function TSqliteBackupTask.ReplaceTags(const Template: String): String;
var
  day, month, year, hour, minute, seconds, mili: word;
  ADateTime: TDateTime;
begin
  ADateTime := Now;
  DecodeDate(ADateTime, year, month, day);
  DecodeTime(ADateTime, hour, minute, seconds, mili);
  Result := StringReplace(Template, '{{name}}', Name, [rfReplaceAll]);
  Result := StringReplace(Result, '{{day}}', AddChar('0', IntToStr(day), 2), [rfReplaceAll]);
  Result := StringReplace(Result, '{{month}}', AddChar('0', IntToStr(month), 2), [rfReplaceAll]);
  Result := StringReplace(Result, '{{year}}', IntToStr(year), [rfReplaceAll]);
  Result := StringReplace(Result, '{{hour}}', AddChar('0', IntToStr(hour), 2), [rfReplaceAll]);
  Result := StringReplace(Result, '{{minute}}', AddChar('0', IntToStr(minute), 2), [rfReplaceAll]);
end;

function TSqliteBackupTask.StoreFile(const BackupFileName: String): Boolean;
var
  FileName, TargetFileName, TargetDirectory: String;
  Zipper: TAbZipper;
  StartTime, EndTime: TDateTime;
begin
  Result  := False;
  FileName := ExtractFileName(FData.Get('database', ''));
  TargetFileName := FData.GetPath('target.filename', '{{name}}-{{year}}-{{month}}-{{day}}--{{hour}}-{{minute}}');
  TargetFileName := ReplaceTags(TargetFileName);
  TargetDirectory := IncludeTrailingPathDelimiter(FData.GetPath('target.directory', ''));
  if not DirectoryExists(TargetDirectory) then
    raise Exception.CreateFmt('Error - directory "%s" does not exist', [TargetDirectory]);
  FTargetPath := TargetDirectory + TargetFileName + '.zip';
  Zipper := TAbZipper.Create(nil);
  try
    StartTime := Time;
    Zipper.FileName := FTargetPath;
    Zipper.StoreOptions := [soStripPath];
    Zipper.AddFiles(BackupFileName, faAnyFile);
    Zipper.Move(Zipper.Items[0], FileName);
    Zipper.Save;
    EndTime := Time;
    WriteLn('Compression time ', FormatDateTime('nn:ss:zzz', EndTime - StartTime));
  finally
    Zipper.Destroy;
  end;
  Result := True;
end;

function TSqliteBackupTask.Execute: Boolean;
var
  Runner: TSqliteBackupRunner;
begin
  Runner := TSqliteBackupRunner.Create;
  try
    Result := Runner.Execute(FData.Get('database', ''));
    if not Result then
      WriteLn('Error executing task ', Runner.Error)
    else
    begin
      Result := StoreFile(Runner.Output);
    end;
  finally
    DeleteFile(Runner.Output);
    Runner.Destroy;
  end;
end;

end.

