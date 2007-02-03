unit ATxFProc;
{$mode delphi}
{$H+}
interface

uses
  Windows;

function IsFileExist(const fn: WideString; var IsDir: boolean): boolean; overload;
function IsFileExist(const fn: WideString): boolean; overload;
function IsDirExist(const fn: WideString): boolean; overload;
function IsFileAccessed(const fn: WideString): boolean;

function FFileOpen(const fn: WideString): THandle;
function FFileCopy(const fnSrc, fnDest: WideString): boolean;
function FGetFileSize(const fn: WideString): Int64; overload;
function FGetFileSize(Handle: THandle): Int64; overload;
function FGetShortName(const fn: WideString): WideString;

type
  PInt64Rec = ^TInt64Rec;
  TInt64Rec = packed record
    Lo, Hi: DWORD;
  end;

procedure FOpenURL(const s: string; hWnd: THandle);
function FExecute(const Command, Params: WideString; hWnd: THandle): boolean;
function FCreateDir(const fn: WideString): boolean;

function IsFileUnicode(h: THandle): boolean;
function IsFileUTF8(h: THandle): boolean;
function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: boolean; var IsOEM: boolean): boolean;

implementation

uses
  SysUtils, ShellAPI;

var
  IsNT: boolean;

function IsFileExist(const fn: WideString; var IsDir: boolean): boolean; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
begin
  IsDir:= false;
  if IsNT then
    begin
    h:= FindFirstFileW(PWChar(fn), @fdW);
    Result:= h<>INVALID_HANDLE_VALUE;
    if Result then
      begin
      IsDir:= (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
      Windows.FindClose(h);
      end;
    end
  else
    begin
    h:= FindFirstFileA(PChar(string(fn)), fdA);
    Result:= h<>INVALID_HANDLE_VALUE;
    if Result then
      begin
      IsDir:= (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0;
      Windows.FindClose(h);
      end;
    end;
end;

function IsFileExist(const fn: WideString): boolean; overload;
var
  IsDir: boolean;
begin
  Result:= IsFileExist(fn, IsDir) and (not IsDir);
end;

function IsDirExist(const fn: WideString): boolean; overload;
var
  IsDir: boolean;
begin
  Result:= IsFileExist(fn, IsDir) and IsDir;
end;

function FFileOpen(const fn: WideString): THandle;
begin
  if IsNT then
    Result:= CreateFileW(PWChar(fn),
             GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
             nil, OPEN_EXISTING, 0, 0)
  else
    Result:= CreateFileA(PChar(string(fn)),
             GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
             nil, OPEN_EXISTING, 0, 0);
end;

function FFileCopy(const fnSrc, fnDest: WideString): boolean;
begin
  if IsNT then
    Result:= CopyFileW(PWChar(fnSrc), PWChar(fnDest), false)
  else
    Result:= CopyFileA(PChar(string(fnSrc)), PChar(string(fnDest)), false);
end;

function IsFileAccessed(const fn: WideString): boolean;
var
  h: THandle;
begin
  h:= FFileOpen(fn);
  Result:= h<>INVALID_HANDLE_VALUE;
  if Result then CloseHandle(h);
end;

function FGetFileSize(const fn: WideString): Int64; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  SizeRec: TInt64Rec absolute Result;
begin
  Result:= -1;
  if IsNT then
    begin
    h:= FindFirstFileW(PWChar(fn), @fdW);
    if h<>INVALID_HANDLE_VALUE then
      begin
      //Attr:= fdW.dwFileAttributes;
      SizeRec.Hi:= fdW.nFileSizeHigh;
      SizeRec.Lo:= fdW.nFileSizeLow;
      //Time:= fdW.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end
  else
    begin
    h:= FindFirstFileA(PChar(string(fn)), fdA);
    if h<>INVALID_HANDLE_VALUE then
      begin
      //Attr:= fdA.dwFileAttributes;
      SizeRec.Hi:= fdA.nFileSizeHigh;
      SizeRec.Lo:= fdA.nFileSizeLow;
      //Time:= fdA.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end;
end;

function FGetFileSize(Handle: THandle): Int64; overload;
var
  Size: Int64;
  SizeRec: TInt64Rec absolute Size;
begin
  SizeRec.Lo:= GetFileSize(Handle, @SizeRec.Hi);
  if (SizeRec.Lo=$FFFFFFFF) and (GetLastError<>NO_ERROR)
    then Result:= -1
    else Result:= Size;
end;

function FGetShortName(const fn: WideString): WideString;
var
  bufA: array[0..MAX_PATH] of char;
  bufW: array[0..MAX_PATH] of WideChar;
  res: string;
begin
  if IsNT
    then
      begin
      FillChar(bufW, SizeOf(bufW), 0);
      SetString(Result, bufW, GetShortPathNameW(PWChar(fn), bufW, SizeOf(bufW) div 2));
      end
    else
      begin
      FillChar(bufA, SizeOf(bufA), 0);
      SetString(res, bufA, GetShortPathName(PChar(string(fn)), bufA, SizeOf(bufA)));
      Result:= res;
      end;
end;

procedure FOpenURL(const s: string; hWnd: THandle);
begin
  ShellExecute(hWnd, 'open', PChar(s), nil, nil, 0{SW_SHOW});
end;

function FExecute(const Command, Params: WideString; hWnd: THandle): boolean;
begin
  if IsNT
    then Result:= ShellExecuteW(hWnd, 'open', PWChar(Command), PWChar(Params), nil, SW_SHOW) > 32
    else Result:= ShellExecuteA(hWnd, 'open', PChar(string(Command)), PChar(string(Params)), nil, SW_SHOW) > 32;
end;

function FCreateDir(const fn: WideString): boolean;
begin
  if IsNT
    then Result:= CreateDirectoryW(PWChar(fn), nil)
    else Result:= CreateDirectoryA(PChar(string(fn)), nil);
end;


function IsFileUnicode(h: THandle): boolean;
var
  BufferSign: Word;
  BytesRead: DWORD;
begin
  BufferSign:= 0;
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result:= ReadFile(h, BufferSign, SizeOf(BufferSign), BytesRead, nil) and
    (BytesRead>=SizeOf(BufferSign)) and
    ((BufferSign=$FEFF) or (BufferSign=$FFFE));
end;

function IsFileUTF8(h: THandle): boolean;
var
  BufferSign: packed array[0..2] of byte;
  BytesRead: DWORD;
begin
  FillChar(BufferSign, SizeOf(BufferSign), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result:= ReadFile(h, BufferSign, SizeOf(BufferSign), BytesRead, nil) and
    (BytesRead>=SizeOf(BufferSign)) and
    ((BufferSign[0]=$EF) and (BufferSign[1]=$BB) and (BufferSign[2]=$BF));
end;

type
  TFreqTable = array[$80..$FF] of integer;

function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: boolean; var IsOEM: boolean): boolean;
var
  Buffer: PChar;
  BufSize, BytesRead, i: DWORD;
  n: integer;
  Table: TFreqTable;
  TableSize: integer;
begin
  Result:= false;
  IsOEM:= false;

  if BufSizeKb=0 then Exit;
  Buffer:= nil;
  BufSize:= BufSizeKb*1024;

  //Init freq table
  TableSize:= 0;
  FillChar(Table, SizeOf(Table), 0);

  try
    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);

    if ReadFile(h, Buffer^, BufSize, BytesRead, nil) then
      if BytesRead>0 then
        begin
        Result:= true;
        for i:= 0 to BytesRead-1 do
          begin
          n:= Ord(Buffer[i]);

          //If control chars present, then non-text
          if (n<32) and (n<>09) and (n<>13) and (n<>10) then
            begin Result:= false; Break end;

          //Calculate freq table
          if DetectOEM then
            if (n>=Low(Table)) and (n<=High(Table)) then
              begin
              Inc(TableSize);
              Inc(Table[n]);
              end;
          end;
        end;

    //Analize table
    if DetectOEM then
      if Result and (TableSize>0) then
        for i:= Low(Table) to High(Table) do
          begin
          Table[i]:= Table[i]*100 div TableSize;
          if ((i>=$B0) and (i<=$DF)) or (i=$FF) or (i=$A9) then
            if Table[i]>=18 then
              begin IsOEM:= true; Break end;
          end;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;


initialization
  IsNT:= Win32Platform=VER_PLATFORM_WIN32_NT;

end.
