unit ChronoLog;

{
  Class to record and log benchmarks

  Copyright (C) 2005 Luiz Américo Pereira Câmara

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



{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, sysutils, cpu;

type
  TInt64Array = array of Int64;
  TIntArray = array of longint;
  
  { TChronoLog }
  //todo: see effect of inlining Stop/Mark in the size
  //      add lap
  //      compare precision with ZenTimer
  TChronoLog = class
  private
    FZenTimer:TZenTimer;
    FResults: TInt64Array;
    FActions: TIntArray;
    FCount: Integer;
    FDescriptions: TStringList;
    FSessionName: String;
    FComments: String;
    function FormatedString (Value: Int64):String;
    function GetAverage:Int64;
    function GetLastResult:Int64;
    function GetAccumulated: Int64;
    procedure SetCapacity(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop(AId:Integer);
    procedure Stop; inline;
    procedure Reset;
    procedure Mark(AId:Integer);
    procedure RegisterId(AId:longint;const ADesc: String);
    function AsSecond (Index: Integer): Int64;
    function AsSecondStr (Index: Integer): String;
    function AsMiliSecond (Index: Integer): Int64;
    function AsMiliSecondStr (Index: Integer): String;
    function AsMicroSecond (Index: Integer): Int64;
    function AsMicroSecondStr (Index: Integer): String;
    function GetActionStr(AIndex: Integer): String;
    function GetSystemInfo:String;
    procedure SaveToText (const FileName: String; ClearFile: Boolean = False);
    property Average: Int64 read GetAverage;
    property LastResult: Int64 read GetLastResult;
    property Accumulated: Int64 read GetAccumulated;
    property SessionName: String read FSessionName write FSessionName;
    property Comments: String read FComments write FComments;
    property Capacity: Integer write SetCapacity;
    property Count: Integer read FCount;
  end;
    
implementation
uses
  StrUtils;
  
constructor TChronoLog.Create;
begin
  SetLength(FActions,100);
  SetLength(FResults,100);
  FZenTimer:=TZenTimer.Create;
  FZenTimer.AutoReset:=True;
  FDescriptions:=TStringList.Create;
end;

destructor TChronoLog.Destroy;
begin
  FZenTimer.Destroy;
  FDescriptions.Destroy;
end;

procedure TChronoLog.Start;
begin
  FZenTimer.Start;
end;

procedure TChronoLog.Stop(AId:Integer);
begin
  FResults[FCount]:=FZenTimer.Stop;
  FActions[FCount]:=AId;
  Inc(FCount);
end;

procedure TChronoLog.Stop; inline;
begin
  Stop(-1);
end;

procedure TChronoLog.Reset;
begin
  FCount:=0;
  FZenTimer.Stop;
end;

procedure TChronoLog.Mark(AId:Integer);
begin
  FResults[FCount]:=FZenTimer.Stop;
  FActions[FCount]:=AId;
  Inc(FCount);
  FZenTimer.Start;
end;

procedure TChronoLog.RegisterId(AId: longint; const ADesc: String);
begin
  FDescriptions.AddObject(ADesc,TObject(AId))
end;

function TChronoLog.AsSecond (Index: Integer): Int64;
begin
  Result:=Round(FResults[Index]/1000000);
end;

function TChronoLog.AsSecondStr (Index: Integer): String;
begin
  Result:=FormatedString(Round(FResults[Index]/1000000));
end;

function TChronoLog.AsMiliSecond (Index: Integer): Int64;
begin
  Result:=Round(FResults[Index]/1000);
end;

function TChronoLog.AsMiliSecondStr (Index: Integer): String;
begin
  Result:=FormatedString(Round(FResults[Index]/1000));
end;

function TChronoLog.AsMicroSecond (Index: Integer): Int64;
begin
  Result:=FResults[Index];
end;

function TChronoLog.AsMicroSecondStr (Index: Integer): String;
begin
  Result:=FormatedString(FResults[Index]);
end;

function TChronoLog.FormatedString (Value: Int64):String;
var
  TempStr:String;
  Counter,Digits:Integer;
begin
  Digits:=0;
  Result:='';
  TempStr:=IntToStr(Value);
  for Counter := length(TempStr) downto 1 do
  begin
    if Digits = 3 then
    begin
      Digits:=0;
      Result:=ThousandSeparator+Result;
    end; 
    Result:=TempStr[Counter]+Result;
    Inc(Digits);     
  end;    
end;

function TChronoLog.GetAverage:Int64;
begin
  if FCount > 0 then
    Result:=Round(Accumulated/FCount)
  else
    Result:=Accumulated;    
end;

function TChronoLog.GetLastResult: Int64;
begin
  Result:=FResults[FCount-1];
end;

function TChronoLog.GetAccumulated: Int64;
var
  i:integer;
begin
  Result:=0;
  for i := 0 to FCount - 1 do
    Result:=Result+FResults[i];
end;

function TChronoLog.GetActionStr(AIndex: Integer): String;
var
  i,AId:Integer;
begin
  AId:=FActions[AIndex];
  i:=FDescriptions.IndexOfObject(TObject(AId));
  if i <> -1 then
    Result:=FDescriptions[i]
  else
    Result:='';
end;

function TChronoLog.GetSystemInfo: String;
begin
  //todo: Get mem + SO
  Result:=GetCPUString(this_cpu);
end;

procedure TChronoLog.SetCapacity(Value: Integer);
begin
  //todo: see if Setlength keeps previous data
  SetLength(FResults,Value);
  SetLength(FActions,Value);
end;

procedure TChronoLog.SaveToText (const FileName: String; ClearFile: Boolean = False);
var
  AFile:Text;
  Counter, Idx:Integer;
begin
  Assign(AFile, FileName);
  if FileExists(FileName) and not ClearFile then
  begin
    Append(AFile);
    WriteLn(AFile);
  end
  else
    Rewrite(AFile);

  Writeln(AFile,'########################################');
  Writeln(AFile,'TChronoLog results - ',FormatDateTime(ShortDateFormat+' hh:nn:ss',Now));
  WriteLn(AFile,'Session: ',FSessionName);

  Writeln(AFile,'Results (',FCount,'):                              microsec     milisec     seconds');
  Writeln(AFile,'-------------------------------------   -----------   ---------   ---------');
  for Counter:= 0 to FCount -1 do
  begin
    //todo: reposition text
    Idx:=FDescriptions.IndexOfObject(TObject(FActions[Counter]));
    if Idx <> -1 then
      Write(AFile,PadRight(IntToStr(Counter)+' - '+FDescriptions[Idx],38))
    else
      Write(AFile,PadRight(IntToStr(Counter)+' - '+'#No Description#',38));
    WriteLn(AFile,AsMicroSecondStr(Counter):13,
      AsMiliSecondStr(Counter):12,AsSecondStr(Counter):12);
  end;

  WriteLn(AFile);
  Writeln(AFile, 'Accumulated (Miliseconds): ', FormatedString(Round(Accumulated/1000)):12);
  Close(AFile);
end;

begin
end.
