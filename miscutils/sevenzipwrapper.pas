unit SevenZipWrapper;

{
  Wrapper to 7-zip program

  Copyright (C) 2008 Luiz Americo Pereira Camara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

{$ifdef unix}
{$define USE_P7ZIP}
{$endif}

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
  public
    property CRC: String read FCRC write FCRC;
    property Mofified: TDateTime read FMofified write FMofified;
    property PackedSize: Int64 read FPackedSize write FPackedSize;
    property Path: String read FPath write FPath;
    property Size: Int64 read FSize write FSize;
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
    procedure UpdateFileTypeInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Extract(Index: Integer; const Directory: String = '');
    procedure Extract(const AFileName: String; const Directory: String = '');
    procedure Load;
    property Executable: String read FExecutable write FExecutable;
    property FileName: String read FFileName write FFileName;
    property OnError: TSevenZipErrorEvent read FOnError write FOnError;
    property Options: TSevenZipReaderOptions read FOptions write FOptions;
    property PackedFiles: TSevenZipPackedFileList read FPackedFiles;
  end;

implementation

uses
  process, strutils;

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

const
  //output of p7zip is different from 7zip
  {$ifdef USE_P7ZIP}
  FileTypes: array [0..2] of TFileTypeInfo =
  (
  (FileListOffset: 9; EntriesPerFile: 9; SizeOffset: 1; CRCOffset: 5), //7z
  (FileListOffset: 9; EntriesPerFile: 12; SizeOffset: 2; CRCOffset: 8), //zip
  (FileListOffset: 9; EntriesPerFile: 18; SizeOffset: 2; CRCOffset: 13) //rar
  );
  {$else}
  FileTypes: array [0..2] of TFileTypeInfo =
  (
  (FileListOffset: 12; EntriesPerFile: 9; SizeOffset: 1; CRCOffset: 5), //7z
  (FileListOffset: 9; EntriesPerFile: 12; SizeOffset: 2; CRCOffset: 8), //zip
  (FileListOffset: 14; EntriesPerFile: 18; SizeOffset: 2; CRCOffset: 13)  //rar
  );
  {$endif}
  
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
    Dec(FFileType.FileListOffset {$ifdef USE_P7ZIP}, 2{$endif});
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
        raise Exception.Create('TSevenZipReader.ParseDefaultOutput (Load): ' + ErrorStr);
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
        raise Exception.Create('TSevenZipReader.ParseDetailedOutput (Load): ' + ErrorStr);
  end;
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
    raise Exception.Create('TSevenZipReader.Extract: Index Out Of Bounds');
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
        raise Exception.Create('TSevenZipReader.Extract: ' + ErrorStr);
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
    //todo: see why is necessary to wait under linux/unix
    {$ifdef unix}
    Sleep(100);
    {$endif}
    if szoGetDetails in FOptions then
      ParseDetailedOutput
    else
      ParseDefaultOutput;
  end;
end;

end.

