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

  TSessionInfo = record
    Results: TInt64Array;
    Actions: TIntArray;
    Name: String;
    Count: Integer;
    Capacity: Cardinal;
  end;

  TSessionInfoArray = array of TSessionInfo;
  
  { TChronoLog }
  //todo: see effect of inlining Stop/Mark in the size
  //      add lap
  //      compare precision with ZenTimer
  TChronoLog = class
  private
    FZenTimer:TZenTimer;
    FResults: TInt64Array;
    FActions: TIntArray;
    FCount: Cardinal;
    FSessions: TSessionInfoArray;
    FCapacity: Cardinal;
    FActionDefs: TStringList;
    FSessionName: String;
    FComments: String;
    procedure Expand;
    function FormatedString (Value: Int64):String;
    function FindSession(const Name: String): Integer;
    function GetAverage:Int64;
    function GetElapsedTime: Int64;
    function GetAccumulated: Int64;
    procedure SetCapacity(Value: Cardinal);
    procedure SetSessionName(const Value: String);
    procedure UpdateCount;
    procedure UpdateCurrentSession;
    procedure UpdateSessionName(const NewSessionName: String);
    procedure WriteSessionResults(var AFile: Text; ACount: Cardinal;
      AResults: TInt64Array; AActions: TIntArray; const Session: String);
    procedure WriteActionResults(var AFile: Text; ACount: Cardinal;
      AResults: TInt64Array; AActions: TIntArray; const Session: String; ActionId: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop(AId:Integer);
    procedure Stop; inline;
    procedure Reset;
    procedure Mark(AId:Integer);
    procedure RegisterAction(ActionId: PtrInt; const ActionName: String);
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
    property ElapsedTime: Int64 read GetElapsedTime;
    property Accumulated: Int64 read GetAccumulated;
    property SessionName: String read FSessionName write SetSessionName;
    property Comments: String read FComments write FComments;
    property Count: Cardinal read FCount;
  end;
    
implementation

uses
  StrUtils;

const
  DEFAULT_CAPACITY = 64;
  
constructor TChronoLog.Create;
begin
  SetCapacity(DEFAULT_CAPACITY);
  FZenTimer:=TZenTimer.Create;
  FZenTimer.AutoReset:=True;
  FActionDefs:=TStringList.Create;
end;

destructor TChronoLog.Destroy;
begin
  FZenTimer.Destroy;
  FActionDefs.Destroy;
end;

procedure TChronoLog.Start;
begin
  FZenTimer.Start;
end;

procedure TChronoLog.Stop(AId:Integer);
begin
  FResults[FCount]:=FZenTimer.Stop;
  FActions[FCount]:=AId;
  UpdateCount;
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
  UpdateCount;
  FZenTimer.Start;
end;

procedure TChronoLog.RegisterAction(ActionId: PtrInt; const ActionName: String);
begin
  FActionDefs.AddObject(ActionName, TObject(ActionId));
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

procedure TChronoLog.Expand;
var
  IncSize: Cardinal;
begin
  if FCount < FCapacity then
    Exit;
  //alghoritm borrowed from FPList
  IncSize := 16;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2);
  SetCapacity(FCapacity + IncSize);
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

function TChronoLog.FindSession(const Name: String): Integer;
begin
  Result := 0;
  while Result < Length(FSessions) do
  begin
    if FSessions[Result].Name = Name then
      Exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TChronoLog.GetAverage:Int64;
begin
  if FCount > 0 then
    Result:=Round(Accumulated/FCount)
  else
    Result:=Accumulated;    
end;

function TChronoLog.GetElapsedTime: Int64;
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

procedure TChronoLog.UpdateSessionName(const NewSessionName: String);
var
  i, SessionsCount: Integer;
begin
  SessionsCount := Length(FSessions);
  i := FindSession(FSessionName);
  if i <> -1 then
  begin
    //update current session
    FSessions[i].Count := FCount;
    FSessions[i].Actions := FActions;
    FSessions[i].Results := FResults;
    FSessions[i].Capacity := FCapacity;
  end
  else
  begin
    // if there's already log calls and no session found. create an unammed session
    if FCount > 0 then
    begin
      SetLength(FSessions, SessionsCount + 1);
      FSessions[SessionsCount].Count := FCount;
      FSessions[SessionsCount].Actions := FActions;
      FSessions[SessionsCount].Results := FResults;
      FSessions[SessionsCount].Capacity := FCapacity;
      Inc(SessionsCount);
    end;
  end;

  i := FindSession(NewSessionName);
  if i = -1 then
  begin
    //no session create a new session
    SetLength(FSessions, SessionsCount + 1);
    FSessions[SessionsCount].Name := NewSessionName;
    FSessions[SessionsCount].Count := 0;
    FSessions[SessionsCount].Capacity := DEFAULT_CAPACITY;
    SetLength(FSessions[SessionsCount].Actions, DEFAULT_CAPACITY);
    SetLength(FSessions[SessionsCount].Results, DEFAULT_CAPACITY);
    i := SessionsCount;
  end;
  // update data with the new selected session
  FCount := FSessions[i].Count;
  FActions := FSessions[i].Actions;
  FResults := FSessions[i].Results;
  FCapacity := FSessions[i].Capacity;
  FSessionName := NewSessionName;
end;

procedure TChronoLog.WriteSessionResults(var AFile: Text; ACount: Cardinal; AResults: TInt64Array; AActions: TIntArray; const Session: String);
var
  j, i: Integer;
  ATime: Int64;
begin
  Writeln(AFile, LineEnding, PadRight('Session: "' + Session +'" (' + IntToStr(ACount) + ' results)', 43),'microsec     milisec     seconds');
  Writeln(AFile, '-------------------------------------   -----------   ---------   ---------');
  for i := 0 to ACount - 1 do
  begin
    //todo: reposition text
    j := FActionDefs.IndexOfObject(TObject(AActions[i]));
    if j <> - 1 then
      Write(AFile, PadRight(IntToStr(i) + ' - ' + FActionDefs[j], 38))
    else
      Write(AFile, PadRight(IntToStr(i) + ' - ' + '#No Description#', 38));
    ATime := AResults[i];
    WriteLn(AFile, FormatedString(ATime): 13, FormatedString(Round(ATime/1000)): 12,
      FormatedString(Round(ATime/1000000)): 12);
  end;
end;

procedure TChronoLog.WriteActionResults(var AFile: Text; ACount: Cardinal;
  AResults: TInt64Array; AActions: TIntArray; const Session: String; ActionId: Integer);
var
  i: Integer;
  ATime: Int64;
begin
  for i := 0 to ACount - 1 do
  begin
    if AActions[i] <> ActionId then
      continue;
    Write(AFile, PadRight(Session, 38));
    ATime := AResults[i];
    WriteLn(AFile, FormatedString(ATime): 13, FormatedString(Round(ATime/1000)): 12,
      FormatedString(Round(ATime/1000000)): 12);
  end;
end;

function TChronoLog.GetActionStr(AIndex: Integer): String;
var
  i,AId:Integer;
begin
  AId:=FActions[AIndex];
  i:=FActionDefs.IndexOfObject(TObject(AId));
  if i <> -1 then
    Result:=FActionDefs[i]
  else
    Result:='';
end;

function TChronoLog.GetSystemInfo: String;
begin
  //todo: Get mem + SO
  Result:=GetCPUString(this_cpu);
end;

procedure TChronoLog.SetCapacity(Value: Cardinal);
begin
  if Value < FCount then
    Exit;
  FCapacity := Value;
  //todo: see if Setlength keeps previous data
  SetLength(FResults, Value);
  SetLength(FActions, Value);
end;

procedure TChronoLog.SetSessionName(const Value: String);
begin
  if FSessionName = Value then Exit;
  UpdateSessionName(Value);
end;

procedure TChronoLog.UpdateCount;
begin
  if FCount = FCapacity then
    Expand;
  Inc(FCount);
end;

procedure TChronoLog.UpdateCurrentSession;
var
  i: Integer;
begin
  i := FindSession(FSessionName);
  if i <> -1 then
  begin
    FSessions[i].Count := FCount;
    FSessions[i].Actions := FActions;
    FSessions[i].Results := FResults;
    FSessions[i].Capacity := FCapacity;
  end;
end;

procedure TChronoLog.SaveToText (const FileName: String; ClearFile: Boolean = False);
var
  AFile:Text;
  SessionCount, i, j: Integer;
  ActionId: PtrInt;
  SessionInfo: TSessionInfo;
begin
  UpdateCurrentSession;
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

  SessionCount := Length(FSessions);
  if SessionCount = 0 then
  begin
    WriteLn(AFile,'No Session Defined');
    WriteSessionResults(AFile, FCount, FResults, FActions, '');
  end
  else
  begin
    WriteLn(AFile);
    WriteLn(AFile, '===================');
    WriteLn(AFile, 'Grouped By Sessions');
    WriteLn(AFile, '===================');
    for i := 0 to SessionCount - 1 do
    begin
      SessionInfo := FSessions[i];
      WriteSessionResults(AFile, SessionInfo.Count, SessionInfo.Results, SessionInfo.Actions, SessionInfo.Name);
    end;
    WriteLn(AFile);
    WriteLn(AFile, '==================');
    WriteLn(AFile, 'Grouped By Actions');
    WriteLn(AFile, '==================');

    for j := 0 to FActionDefs.Count - 1 do
    begin
      ActionId := PtrInt(FActionDefs.Objects[j]);
      WriteLn(AFile, LineEnding, PadRight('Action: "' + FActionDefs[j] + '"', 43), 'microsec     milisec     seconds');
      Writeln(AFile, '-------------------------------------   -----------   ---------   ---------');
      for i := 0 to SessionCount - 1 do
      begin
        SessionInfo := FSessions[i];
        WriteActionResults(AFile, SessionInfo.Count, SessionInfo.Results, SessionInfo.Actions, SessionInfo.Name, ActionId);
      end;
    end;
  end;
  WriteLn(AFile);
  //Writeln(AFile, 'Accumulated (Miliseconds): ', FormatedString(Round(Accumulated/1000)):12);
  Close(AFile);
end;

begin
end.
