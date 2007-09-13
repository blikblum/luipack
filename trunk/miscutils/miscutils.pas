unit miscutils;

{ Miscelaneous routines and classes

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}


{$Mode ObjFpc}
{$H+}

interface

uses 
  sysutils,classes;

const
  {$ifdef win32}
  OSWildCard = '*.*';
  OSIgnoreCase = True;
  {$else}
  OSWildCard = '*';
  OSIgnoreCase = False;
  {$endif}

type

  { TIndexedList }

  TIndexedList = class
    FValues:TFPList;
    FIndexes:TFPList;
    procedure SetCapacity (Value: Integer);
    function GetCapacity:Integer;
    function GetCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AIndex:Integer;AValue:Pointer);  
    function Get(AIndex:Integer;var AValue:Pointer):boolean;
    function Get(AIndex:Integer):Pointer;
    function GetIndex(APosition:Integer):Integer;
    function GetValue(APosition:Integer):Pointer;
    procedure Clear;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Indexes [APosition: Integer]: Integer read GetIndex;
    property Values [APosition: Integer]: Pointer read GetValue; default;
  end;

  { TStringsParser }

  TStringsParser = class (TObject)
  private
    FExcludeList: TStrings;
    FFinalDelimiters: TStrings;
    FInitialDelimiters: TStrings;
    FOutList: TStrings;
    FSrcList: TStrings;
    FSrcListOwner: Boolean;
    FOutListOwner: Boolean;
    procedure SetOutList(const AValue: TStrings);
    procedure SetSrcList(const AValue: TStrings);
    function MatchExclude (const ALine: String): Boolean;
    function StripLine (const ALine: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure LoadFromFile(const FileName: String);
    property InitialDelimiters: TStrings read FInitialDelimiters;
    property FinalDelimiters: TStrings read FFinalDelimiters;
    property ExcludeList: TStrings read FExcludeList;
    property SrcList: TStrings read FSrcList write SetSrcList;
    property OutList: TStrings read FOutList write SetOutList;
  end;
  
  { TCSVFile }
  
  TCSVColumnNotify = procedure (const Value: String; Index: Integer) of Object;
  TCSVRowNotify = procedure (RowNo: Integer) of Object;
  TCSVHeaderNotify = procedure (Fields: array of String; FieldCount: Integer) of Object;

  TCSVFile = class
  private
    FFileHandle: Text;
    FDelimiter: Char;
    FFilename: String;
    FOnColumn: TCSVColumnNotify;
    FOnHeader: TCSVHeaderNotify;
    FOnStartRow: TCSVRowNotify;
    FOnEndRow: TCSVRowNotify;
    procedure ReadHeader;
    procedure SetFilename(const AValue: String);
  public
    constructor Create;
    procedure Parse(WithHeader: Boolean = True);
    procedure GetHeader;
    property OnStartRow: TCSVRowNotify read FOnStartRow write FOnStartRow;
    property OnEndRow: TCSVRowNotify read FOnEndRow write FOnEndRow;
    property OnColumn: TCSVColumnNotify read FOnColumn write FOnColumn;
    property OnHeader: TCSVHeaderNotify read FOnHeader write FOnHeader;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property Filename: String read FFilename write SetFilename;
  end;

procedure GetDirectoryTree(const ARootDir:String; AList: TStrings);

procedure GetFileList(const AFilter: String; AStrList: TStrings);

procedure CSVToStrings(const ACSVStr: String; AStrList: TStrings; Delimiter: Char = ';');

function StringsToCSV(AStrList:TStrings; Delimiter: Char = ';'):String;

function ValidMail(const AMail: String): Boolean;

implementation
uses
  StrUtils;

procedure GetDirectoryTree(const ARootDir:String; AList: TStrings);
  procedure RecurseDir(const ADir:String);
  var
    RecInfo:TSearchRec;
    OldDir:String;
  begin
    OldDir:=GetCurrentDir;
    AList.Add(ADir);
    ChDir(ADir);   
    if FindFirst(OSWildCard,faDirectory,RecInfo) = 0 then
    repeat
      if (RecInfo.Attr and faDirectory > 0) then
      begin
        if (RecInfo.Name <> '.') and (RecInfo.Name <> '..') then
          RecurseDir(ADir+PathDelim+RecInfo.Name);
      end;  
    until FindNext(RecInfo) <> 0;
    FindClose(RecInfo);
    ChDir(OldDir);
  end;
begin  
  RecurseDir(ExcludeTrailingPathDelimiter(ARootDir));
end; 
  
procedure GetFileList(const AFilter: String; AStrList: TStrings);
var
  FileInfo: TSearchRec;
begin
  AStrList.Clear;
  if FindFirst(AFilter,faArchive,FileInfo) = 0 then
  repeat
    if (FileInfo.Attr and faArchive > 0) then
      AStrList.Add(FileInfo.Name);
  until FindNext(FileInfo) <> 0;
  FindClose(FileInfo);
end;  

procedure CSVToStrings(const ACSVStr: String; AStrList: TStrings; Delimiter: Char = ';');
var
  pos1,pos2:Integer;
begin
  pos1:=1;
  pos2:=pos(Delimiter,ACSVStr);
  while pos1 < pos2 do
  begin
    //writeln('pos1: ',pos1);
    //writeln('pos2: ',pos2);
    //writeln('Copying ',pos2-pos1,' chars from ',pos1,' - Result: ',Copy(ACSVStr,pos1,pos2-pos1));
    AStrList.Add(Copy(ACSVStr,pos1,pos2-pos1));
    pos1:=pos2+1;
    pos2:=posex(Delimiter,ACSVStr,pos1);
  end;
  if pos1 < length(Trim(ACSVStr)) then
    AStrList.Add(Copy(ACSVStr,pos1,succ(length(ACSVStr)-pos1)));  
end;

function StringsToCSV(AStrList: TStrings; Delimiter: Char = ';'): String;
var
  i: Integer;
begin
  Result:='';
  for i:= 0 to AStrList.Count - 1 do
    Result:=Result+AStrList[i]+Delimiter;
end;

function ValidMail(const AMail: String): Boolean;
var
  PosArroba: Integer;
  Trimmed: String;
begin
  Result := False;
  Trimmed := Trim(AMail);
  if Pos(' ', Trimmed) = 0 then
  begin
    PosArroba := Pos('@',Trimmed);
    if PosArroba <> 0 then
      Result := posex('.', Trimmed, PosArroba) <> 0;
  end;
end;

{TIndexedList}
   
constructor TIndexedList.Create;
begin
  FIndexes:=TFPList.Create;
  FValues:=TFPList.Create;
end;  

destructor TIndexedList.Destroy;
begin
  FIndexes.Destroy;
  FValues.Destroy;
  inherited Destroy;
end;  

procedure TIndexedList.Add(AIndex:Integer;AValue:Pointer);  
begin
  FIndexes.Add(Pointer(AIndex));
  FValues.Add(AValue);
end;  

function TIndexedList.Get(AIndex:Integer;var AValue:Pointer):boolean;
var
  TempIndex:Integer;
begin
  //Todo: access FValues.List[]
  TempIndex:=FIndexes.IndexOf(Pointer(AIndex));
  Result:= TempIndex <> -1;
  if Result then
    AValue:=FValues[TempIndex];
end;

function TIndexedList.Get(AIndex: Integer): Pointer;
var
  TempIndex:Integer;
begin
  TempIndex:=FIndexes.IndexOf(Pointer(AIndex));
  if TempIndex <> -1 then
    Result:=FValues[TempIndex]
  else
    Result:=nil;
end;

procedure TIndexedList.SetCapacity (Value: Integer);
begin
  FIndexes.Capacity:=Value;
  FValues.Capacity:=Value;
end;  

function TIndexedList.GetCapacity:Integer; 
begin
  Result:=FIndexes.Capacity;
end;
   
function TIndexedList.GetCount:Integer;
begin
  Result:=FIndexes.Count;
end;  

function TIndexedList.GetIndex(APosition:Integer):Integer;
begin
  Result:=Integer(FIndexes[APosition]);
end;
  
function TIndexedList.GetValue(APosition:Integer):Pointer;
begin
  Result:=FValues[APosition];
end;

procedure TIndexedList.Clear;
begin
  FIndexes.Clear;
  FValues.Clear;
end;  
  
{ TStringsParser }

procedure TStringsParser.SetSrcList(const AValue: TStrings);
begin
  if FSrcList=AValue then exit;
  FSrcList:=AValue;
end;

function TStringsParser.MatchExclude(const ALine: String): Boolean;
var
  k: Integer;
begin
  Result:=False;
  if ALine = '' then
    Exit;
  for k:= 0 to FExcludeList.Count-1 do
  begin
    if Pos(FExcludeList[k],ALine) > 0 then
    begin
      Result:=True;
      Exit;
    end;
  end;
end;

function TStringsParser.StripLine(const ALine: String): String;
var
  i,j: Integer;
begin
  Result:= ALine;
  for i:= 0 to FFinalDelimiters.Count - 1 do
  begin
    j:=Pos(FFinalDelimiters[i],Result);
    if j > 0 then
      Delete(Result,j,Length(Result));
  end;
  for i:= 0 to FInitialDelimiters.Count - 1 do
  begin
    j:=Pos(FInitialDelimiters[i],Result);
    if j > 0 then
      Delete(Result,1,j+Length(FInitialDelimiters[i]));
  end;
end;

constructor TStringsParser.Create;
begin
  FInitialDelimiters:=TStringList.Create;
  FFinalDelimiters:=TStringList.Create;
  FExcludeList:=TStringList.Create;
end;

destructor TStringsParser.Destroy;
begin
  FInitialDelimiters.Destroy;
  FFinalDelimiters.Destroy;
  FExcludeList.Destroy;
  if FSrcListOwner then
    FSrcList.Destroy;
  if FOutListOwner then
    FOutList.Destroy;
  inherited Destroy;
end;

procedure TStringsParser.Execute;
var
  i: Integer;
  TempStr: String;
begin
  if FOutList = nil then
  begin
    FOutList:=TStringList.Create;
    FOutListOwner:=True;
  end;
  
  //FOutList.Clear;
  with FSrcList do
  begin
    for i:= 0 to Count - 1 do
    begin
      TempStr:=Trim(Strings[i]);
      if not MatchExclude(TempStr) then
        FOutList.Add(TempStr);
    end;
  end;
end;

procedure TStringsParser.LoadFromFile(const FileName: String);
begin
  if (FSrcList = nil) or not FSrcListOwner then
  begin
    FSrcList:=TStringList.Create;
    FSrcListOwner:=True;
  end;
  FSrcList.LoadFromFile(FileName);
end;

procedure TStringsParser.SetOutList(const AValue: TStrings);
begin
  if FOutList=AValue then exit;
  FOutList:=AValue;
end;

{ TCSVFile }

procedure TCSVFile.ReadHeader;
var
  HeaderValues: array of String;
  ColCount, Pos1, Pos2: Integer;
  CSVStr: String;
begin
  ReadLn(FFileHandle, CSVStr);
  if Assigned(FOnHeader) then
  begin
    ColCount:=0;
    pos1:=1;
    pos2:=pos(FDelimiter, CSVStr);
    while pos1 < pos2 do
    begin
      SetLength(HeaderValues, ColCount+1);
      HeaderValues[ColCount]:=Copy(CSVStr, pos1, pos2-pos1);
      pos1:=pos2+1;
      pos2:=posex(FDelimiter, CSVStr, pos1);
      Inc(ColCount);
    end;
    if pos1 < length(Trim(CSVStr)) then
    begin
      SetLength(HeaderValues, ColCount+1);
      HeaderValues[ColCount]:=Copy(CSVStr, pos1, succ(length(CSVStr)-pos1)
        );
      Inc(ColCount);
    end;
    FOnHeader(HeaderValues, ColCount);
  end;
end;

procedure TCSVFile.SetFilename(const AValue: String);
begin
  FFilename := AValue;
  Assign(FFileHandle,FFilename);
end;

constructor TCSVFile.Create;
begin
  FDelimiter := ';';
end;

procedure TCSVFile.Parse(WithHeader: Boolean = True);
var
  pos1,pos2,RowCount,ColCount:Integer;
  CSVStr: String;
begin
  //Assign(FFileHandle,FFilename);
  Reset(FFileHandle);
  if WithHeader then
    ReadHeader;
  RowCount := 1;
  if not EOF(FFileHandle) then
  repeat
    ReadLn(FFileHandle,CSVStr);
    if Assigned(FOnStartRow) then 
      FOnStartRow(RowCount);
    pos1:=1;
    pos2:=pos(FDelimiter,CSVStr);
    ColCount:=0;
    while pos1 <= pos2 do
    begin
      if Assigned(FOnColumn) then
        FOnColumn(Copy(CSVStr,pos1,pos2-pos1),ColCount);
      pos1:=pos2 + 1;
      pos2:=posex(FDelimiter,CSVStr,pos1);
      Inc(ColCount);
    end;
    if (pos1 < length(Trim(CSVStr))) and Assigned(FOnColumn) then
      FOnColumn(Copy(CSVStr,pos1,succ(length(CSVStr)-pos1)),ColCount);
    if Assigned(FOnEndRow) then
      FOnEndRow(RowCount);
    Inc(RowCount);
  until EOF(FFileHandle);
  Close(FFileHandle);
end;

procedure TCSVFile.GetHeader;
begin
  //Assign(FFileHandle,FFilename);
  Reset(FFileHandle);
  ReadHeader;
  Close(FFileHandle);
end;

end.
