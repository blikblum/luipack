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
    function CreateRegexExpression(const Template: String): String;
    procedure DeleteOldFiles(const Directory, Template: String; KeepCount: Integer);
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
  LuiJSONHelpers, strutils, AbZipper, AbArcTyp, MultiLog, RegExpr, FileUtil, Math;

type

  { TOldFileSearcher }

  TOldFileSearcher = class(TFileSearcher)
  private
    FPattern: TRegExpr;
    FList: TStringList;
  protected
    procedure DoFileFound; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Search(const Directory, Pattern: String); overload;
    property List: TStringList read FList;
  end;

function SortByDate(List: TStringList; Index1, Index2: Integer): Integer;
var
  Time1, Time2: PtrInt;
begin
  Time1 := PtrInt(List.Objects[Index1]);
  Time2 := PtrInt(List.Objects[Index2]);
  Result := CompareValue(Time1, Time2);
end;

{ TOldFileSearcher }

procedure TOldFileSearcher.DoFileFound;
begin
  if FPattern.Exec(FileName) then
    FList.AddObject(FileName, TObject(PtrInt(FileInfo.Time)));
end;

constructor TOldFileSearcher.Create;
begin
  inherited Create;
  FPattern := TRegExpr.Create;
  FList := TStringList.Create;
end;

destructor TOldFileSearcher.Destroy;
begin
  FList.Destroy;
  FPattern.Destroy;
  inherited Destroy;
end;

procedure TOldFileSearcher.Search(const Directory, Pattern: String);
begin
  FPattern.Expression := Pattern;
  Search(Directory, '', False);
  FList.CustomSort(@SortByDate);
end;

{ TSqliteBackupTask }

function TSqliteBackupTask.CreateRegexExpression(const Template: String): String;
begin
  Result := StringReplace(Template, '{{name}}', Name, [rfReplaceAll]);
  Result := StringReplace(Result, '{{day}}', '\d\d', [rfReplaceAll]);
  Result := StringReplace(Result, '{{month}}', '\d\d', [rfReplaceAll]);
  Result := StringReplace(Result, '{{year}}', '\d\d\d\d', [rfReplaceAll]);
  Result := StringReplace(Result, '{{hour}}', '\d\d', [rfReplaceAll]);
  Result := StringReplace(Result, '{{minute}}', '\d\d', [rfReplaceAll]);
end;

procedure TSqliteBackupTask.DeleteOldFiles(const Directory, Template: String; KeepCount: Integer);
var
  Searcher: TOldFileSearcher;
  i: Integer;
begin
  Searcher := TOldFileSearcher.Create;
  try
    Searcher.Search(Directory, CreateRegexExpression(Template));
    for i := 0 to Searcher.List.Count - KeepCount - 1 do
    begin
      Logger.Send([lcInfo], 'Delete old file: ' + Searcher.List[i]);
      DeleteFile(Searcher.List[i]);
    end;
  finally
    Searcher.Destroy;
  end;
end;

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
  FileName, TargetTemplate, TargetFileName, TargetDirectory: String;
  Zipper: TAbZipper;
  StartTime, EndTime: TDateTime;
begin
  Result  := False;
  FileName := ExtractFileName(FData.Get('database', ''));
  TargetTemplate := FData.GetPath('filename', '{{name}}-{{year}}-{{month}}-{{day}}--{{hour}}-{{minute}}');
  TargetFileName := ReplaceTags(TargetTemplate);
  TargetDirectory := IncludeTrailingPathDelimiter(FData.GetPath('targets.directory', ''));
  if not DirectoryExists(TargetDirectory) then
    raise Exception.CreateFmt('Error - directory "%s" does not exist', [TargetDirectory]);
  FTargetPath := TargetDirectory + TargetFileName + '.zip';
  Zipper := TAbZipper.Create(nil);
  try
    Logger.Send([lcInfo], 'File compression started (%s)', [FTargetPath]);
    StartTime := Time;
    Zipper.FileName := FTargetPath;
    Zipper.StoreOptions := [soStripPath];
    Zipper.AddFiles(BackupFileName, faAnyFile);
    Zipper.Move(Zipper.Items[0], FileName);
    Zipper.Save;
    EndTime := Time;
    Logger.Send([lcInfo], 'File compression finished - Time: ' + FormatDateTime('nn:ss:zzz', EndTime - StartTime));
  finally
    Zipper.Destroy;
  end;
  DeleteOldFiles(TargetDirectory, TargetTemplate, 3);
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
      Logger.Send([lcError], 'Error executing task ', Runner.Error)
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

