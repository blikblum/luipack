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
  
  { TSevenZipReader }

  TSevenZipReader = class
  private
    FExecutable: String;
    FFileName: String;
    FOptions: TSevenZipReaderOptions;
    FPackedFiles: TSevenZipPackedFileList;
    FProcess: TProcessLineTalk;
    procedure ParseDefaultOutput;
    procedure ParseDetailedOutput;
    procedure SetExecutable(const AValue: String);
    procedure SetFileName(const AValue: String);
    procedure SetOptions(const AValue: TSevenZipReaderOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    property Executable: String read FExecutable write SetExecutable;
    property FileName: String read FFileName write SetFileName;
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

procedure TSevenZipReader.SetOptions(const AValue: TSevenZipReaderOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TSevenZipReader.ParseDefaultOutput;
var
  LineCount: Integer;
  NewFile: TSevenZipPackedFile;

  procedure ParseDefaultLine(const Line: String);
  begin
    Inc(LineCount);
    if LineCount > 11 then
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
  end;
end;

procedure TSevenZipReader.ParseDetailedOutput;
var
  LineCount: Integer;
  NewFile: TSevenZipPackedFile;

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
  begin
    Inc(LineCount);
    if (LineCount > 10) and (Line <> '') then
    begin
      case (LineCount - 10) mod 9 of
        1:
        begin
          NewFile := TSevenZipPackedFile.Create;
          NewFile.Path := ExtractString(Line);
          PackedFiles.Add(NewFile);
        end;
        2:
          NewFile.Size := ExtractInteger(Line);
        3:
          NewFile.PackedSize := ExtractInteger(Line);
        6:
          NewFile.CRC := ExtractString(Line);
      end;
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

procedure TSevenZipReader.Load;
begin
  PackedFiles.Clear;
  with FProcess do
  try
    CommandLine := FExecutable + ' l '+ IfThen(szoGetDetails in FOptions, '-slt ') +
      '"' + FFileName + '"';
    Execute;
    if szoGetDetails in FOptions then
      ParseDetailedOutput
    else
      ParseDefaultOutput;
  except
    raise Exception.Create('Error executing "' + CommandLine + '"');
  end;
end;

end.

