unit SevenZipWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProcessLineTalk;

type


  TSevenZipPackedFile = class
  private
    FCRC: String;
    FMofified: TDateTime;
    FPackedSize: Int64;
    FPath: String;
    FSize: Int64;
    procedure SetCRC(const AValue: String);
    procedure SetMofified(const AValue: TDateTime);
    procedure SetPackedSize(const AValue: Int64);
    procedure SetPath(const AValue: String);
    procedure SetSize(const AValue: Int64);
  public
    property CRC: String read FCRC write SetCRC;
    property Mofified: TDateTime read FMofified write SetMofified;
    property PackedSize: Int64 read FPackedSize write SetPackedSize;
    property Path: String read FPath write SetPath;
    property Size: Int64 read FSize write SetSize;
  end;


  { TSevenZipPackedFileList }

  TSevenZipPackedFileList = class
  private
    FList: TFpList;
    procedure Add(AFile: TSevenZipPackedFile);
    procedure Clear;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): TSevenZipPackedFile; inline;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSevenZipPackedFile read GetItems; default;
  end;

  TSevenZipReaderOption = (szoGetDetails);
  
  TSevenZipReaderOptions = set of TSevenZipReaderOption;
  
  TSevenZipErrorEvent = procedure (const ErrorStr: String; Handled: Boolean) of object;
  
  TFileTypeInfo = record
    FileListOffset: Integer;
    EntriesPerFile: Integer;
    SizeOffset: Integer;
    CRCOffset: Integer;
  end;
  
  { TSevenZipReader }

  TSevenZipReader = class
  private
    FExecutable: String;
    FFileName: String;
    FFileType: TFileTypeInfo;
    FOnError: TSevenZipErrorEvent;
    FOptions: TSevenZipReaderOptions;
    FPackedFiles: TSevenZipPackedFileList;
    FProcess: TProcessLineTalk;
    function HandleError(const ErrorStr: String): Boolean;
    procedure ParseDefaultOutput;
    procedure ParseDetailedOutput;
    procedure SetExecutable(const AValue: String);
    procedure SetFileName(const AValue: String);
    procedure SetOnError(const AValue: TSevenZipErrorEvent);
    procedure SetOptions(const AValue: TSevenZipReaderOptions);
    procedure UpdateFileTypeInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Extract(Index: Integer; const Directory: String = '');
    procedure Extract(const AFileName: String; const Directory: String = '');
    procedure Load;
    property Executable: String read FExecutable write SetExecutable;
    property FileName: String read FFileName write SetFileName;
    property OnError: TSevenZipErrorEvent read FOnError write SetOnError;
    property Options: TSevenZipReaderOptions read FOptions write SetOptions;
    property PackedFiles: TSevenZipPackedFileList read FPackedFiles;
  end;

implementation

uses
  process, strutils;

{ TSevenZipPackedFile }

procedure TSevenZipPackedFile.SetPath(const AValue: String);
begin
  if FPath=AValue then exit;
  FPath:=AValue;
end;

procedure TSevenZipPackedFile.SetPackedSize(const AValue: Int64);
begin
  if FPackedSize=AValue then exit;
  FPackedSize:=AValue;
end;

procedure TSevenZipPackedFile.SetMofified(const AValue: TDateTime);
begin
  if FMofified=AValue then exit;
  FMofified:=AValue;
end;

procedure TSevenZipPackedFile.SetCRC(const AValue: String);
begin
  if FCRC=AValue then exit;
  FCRC:=AValue;
end;

procedure TSevenZipPackedFile.SetSize(const AValue: Int64);
begin
  if FSize=AValue then exit;
  FSize:=AValue;
end;

{ TSevenZipPackedFileList }

procedure TSevenZipPackedFileList.Add(AFile: TSevenZipPackedFile);
begin
  FList.Add(AFile);
end;

procedure TSevenZipPackedFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    Items[i].Destroy;
  FList.Clear;
end;

function TSevenZipPackedFileList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSevenZipPackedFileList.GetItems(Index: Integer): TSevenZipPackedFile;
begin
  Result := TSevenZipPackedFile(FList[Index]);
end;

constructor TSevenZipPackedFileList.Create;
begin
  FList := TFPList.Create;
end;

destructor TSevenZipPackedFileList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

{ TSevenZipReader }

procedure TSevenZipReader.SetFileName(const AValue: String);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;
end;

procedure TSevenZipReader.SetOnError(const AValue: TSevenZipErrorEvent);
begin
  if FOnError=AValue then exit;
  FOnError:=AValue;
end;

procedure TSevenZipReader.SetOptions(const AValue: TSevenZipReaderOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

const
  FileTypes: array [0..2] of TFileTypeInfo =
  (
  (FileListOffset: 12; EntriesPerFile: 9; SizeOffset: 1; CRCOffset: 5),
  (FileListOffset: 9; EntriesPerFile: 12; SizeOffset: 2; CRCOffset: 8),
  (FileListOffset: 14; EntriesPerFile: 18; SizeOffset: 2; CRCOffset: 13)
  );
  
procedure TSevenZipReader.UpdateFileTypeInfo;
var
  i: Integer;
begin
  i := AnsiIndexText(ExtractFileExt(FFileName), ['.7z', '.zip', '.rar']);
  if i <> -1 then
    FFileType := FileTypes[i]
  else
    raise Exception.Create('Unknow File Extension');
  if szoGetDetails in FOptions then
    Dec(FFileType.FileListOffset);
end;

function TSevenZipReader.HandleError(const ErrorStr: String): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then
    FOnError(ErrorStr, Result);
end;

procedure TSevenZipReader.ParseDefaultOutput;
var
  LineCount: Integer;
  NewFile: TSevenZipPackedFile;
  ErrorStr: String;
  
  procedure ParseDefaultLine(const Line: String);
  begin
    Inc(LineCount);
    if LineCount = 6 then
      ErrorStr := Line;
    if LineCount >= FFileType.FileListOffset then
    begin
      //hack to avoid parsing after file list finished
      if Line[1] = '-' then
      begin
        LineCount := 0;
        Exit;
      end;
      NewFile := TSevenZipPackedFile.Create;
      PackedFiles.Add(NewFile);
      //get Size
      NewFile.Size := StrToInt64Def(Trim(Copy(Line, 27, 12)), 0);
      //get packed size
      NewFile.PackedSize := StrToInt64Def(Trim(Copy(Line, 41, 12)), 0);
      //get path
      NewFile.Path := Copy(Line, 54, Length(Line));
    end;
  end;

begin
  LineCount := 0;
  with FProcess do
  begin
    while Running do
      ParseDefaultLine(ReadLine);
    while HasOutput do
      ParseDefaultLine(ReadLine);
    if ExitStatus <> 0 then
      if not HandleError(ErrorStr) then
        raise Exception.Create('Error In Load: ' + ErrorStr);
  end;
end;

procedure TSevenZipReader.ParseDetailedOutput;
var
  LineCount: Integer;
  NewFile: TSevenZipPackedFile;
  ErrorStr: String;

  function ExtractString(const Line: String): String;
  var
    i: Integer;
  begin
    i := Pos('=', Line);
    Result := Copy(Line, i + 2, Length(Line) - i - 1);
  end;

  function ExtractInteger(const Line: String): Int64;
  begin
    Result := StrToInt64Def(ExtractString(Line), 0);
  end;

  procedure ParseDetailedLine(const Line: String);
  var
    EntryIndex: Integer;
  begin
    Inc(LineCount);
    if LineCount = 6 then
      ErrorStr := Line;
    if (LineCount >= FFileType.FileListOffset) and (Line <> '') then
    begin
      EntryIndex := (LineCount - FFileType.FileListOffset) mod FFileType.EntriesPerFile;
      //path is always the first entry
      if EntryIndex = 0 then
      begin
        NewFile := TSevenZipPackedFile.Create;
        NewFile.Path := ExtractString(Line);
        PackedFiles.Add(NewFile);
      end
      else
        if EntryIndex = FFileType.SizeOffset then
        begin
          NewFile.Size := ExtractInteger(Line);
        end
        else
          if EntryIndex = FFileType.SizeOffset + 1 then
          begin
            NewFile.PackedSize := ExtractInteger(Line);
          end
          else
            if EntryIndex = FFileType.CRCOffset then
              NewFile.CRC := ExtractString(Line);
    end;
  end;

begin
  LineCount := 0;
  with FProcess do
  begin
    while Running do
      ParseDetailedLine(ReadLine);
    while HasOutput do
      ParseDetailedLine(ReadLine);
    if ExitStatus <> 0 then
      if not HandleError(ErrorStr) then
        raise Exception.Create('Error In Load: ' + ErrorStr);
  end;
end;

procedure TSevenZipReader.SetExecutable(const AValue: String);
begin
  if FExecutable=AValue then exit;
  FExecutable:=AValue;
end;

constructor TSevenZipReader.Create;
begin
  FPackedFiles := TSevenZipPackedFileList.Create;
  FProcess := TProcessLineTalk.Create(nil);
  FProcess.Options := FProcess.Options + [poNoConsole];
end;

destructor TSevenZipReader.Destroy;
begin
  FPackedFiles.Destroy;
  FProcess.Destroy;
end;

procedure TSevenZipReader.Extract(Index: Integer; const Directory: String);
begin
  if (Index < 0) or (Index >= FPackedFiles.Count) then
    raise Exception.Create('Error In Extract: Index Out Of Bounds');
  Extract(FPackedFiles[Index].Path, Directory);
end;

procedure TSevenZipReader.Extract(const AFileName: String;
  const Directory: String);
var
  ErrorStr: String;
begin
  with FProcess do
  begin
    Options := Options + [poWaitOnExit];
    CommandLine := FExecutable + ' e "' + FFileName + '"' +
      IfThen(Directory <> '', ' -o' + Directory) + ' -y "' + AFileName + '"';
    Execute;
    Options := Options - [poWaitOnExit];
    if ExitStatus <> 0 then
    begin
      //grab the error str (the 6th line)
      ErrorStr := ReadLine(5);
      if not HandleError(ErrorStr) then
        raise Exception.Create('Error In Extract: ' + ErrorStr);
    end;
  end;
end;

procedure TSevenZipReader.Load;
begin
  PackedFiles.Clear;
  UpdateFileTypeInfo;
  with FProcess do
  begin
    CommandLine := FExecutable + ' l '+ IfThen(szoGetDetails in FOptions, '-slt ') +
      '"' + FFileName + '"';
    Execute;
    if szoGetDetails in FOptions then
      ParseDetailedOutput
    else
      ParseDefaultOutput;
  end;
end;

end.

