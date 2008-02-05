unit WatchList;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

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


{$mode objfpc}{$H+}
{.$define DEBUG_WATCHLIST}

interface

uses
  Classes, SysUtils, sharedlogger;
  
type

  TWatchUpdate = procedure (const AVariable, AValue: String) of object;
  TNewWatchVariable = procedure (const AVariable: String; AIndex: PtrInt) of object;

  TVariableValue = record
    Index: LongWord;
    Value: String;
  end;
  
  PVariableValue = ^TVariableValue;

  { TWatchVariable }

  TWatchVariable = class
  private
    FFirstIndex: LongWord;
    FName: String;
    FList: TFpList;
    FCurrentIndex: Integer;
    function GetCount: Integer;
    function GetCurrentValue:String;
    function GetValues(AIndex: Integer): String;
  public
    constructor Create(const AName: String; AIndex: LongWord);
    destructor Destroy; override;
    {$ifdef DEBUG_WATCHLIST}
    procedure DumpVariable;
    {$endif}
    procedure AddValue (const AValue: String; AIndex: LongWord);
    function Find (AIndex: LongWord): Boolean;
    property Name: String read FName;
    property CurrentValue: String read GetCurrentValue;
    property Values[AIndex: Integer]: String read GetValues; default;
    property Count: Integer read GetCount;
  end;
  
  { TWatchList }

  TWatchList = class
  private
    FList: TFpList;
    FOnNewVariable: TNewWatchVariable;
    FOnUpdate: TWatchUpdate;
    function GetCount: Integer;
    function GetItems(AValue: Integer): TWatchVariable;
    {$ifdef DEBUG_WATCHLIST}
    procedure DumpList;
    {$endif}
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const AName: String): Integer;
    procedure Add(const ANameValue: String; AIndex: LongWord; SkipOnNewVariable: Boolean);
    procedure Clear;
    procedure Update(AIndex: LongWord);
    property OnUpdate: TWatchUpdate read FOnUpdate write FOnUpdate;
    property OnNewVariable: TNewWatchVariable read FOnNewVariable write FOnNewVariable;
    property Items[AValue: Integer]: TWatchVariable read GetItems; default;
    property Count: Integer read GetCount;
  end;

implementation
  {$ifdef DEBUG_WATCHLIST}
   var
     TempDbgStr:String;
  {$endif}

{ TWatchVariable }

function TWatchVariable.GetCurrentValue: String;
begin
  Result := PVariableValue(FList[FCurrentIndex])^.Value;
end;

function TWatchVariable.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchVariable.GetValues(AIndex: Integer): String;
begin
  Result := PVariableValue(FList[AIndex])^.Value;
end;

constructor TWatchVariable.Create(const AName: String; AIndex: LongWord);
begin
  FList := TFPList.Create;
  FName := AName;
  FFirstIndex := AIndex;
end;

destructor TWatchVariable.Destroy;
var
  i:Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PVariableValue(FList[i]));
  FList.Destroy;
end;

{$ifdef DEBUG_WATCHLIST}
procedure TWatchVariable.DumpVariable;
var
  i: Integer;
begin
  for i:= 0 to FList.Count - 1 do
  begin
    Logger.Send('Value',PVariableValue(FList[i])^.Value);
    Str(PVariableValue(FList[i])^.Index,TempDbgStr);
    Logger.Send('Index',TempDbgStr);
  end;
end;
{$endif}

procedure TWatchVariable.AddValue(const AValue: String; AIndex: LongWord);
var
  TempValue: PVariableValue;
begin
  New(TempValue);
  TempValue^.Index:=AIndex;
  TempValue^.Value:=AValue;
  FList.Add(TempValue);
  {$ifdef DEBUG_WATCHLIST}
  Str(AIndex,TempDbgStr);
  Logger.Send('Adding a value (%s - %s)  to variable %s',[AValue,TempDbgStr,FName]);
  {$endif}
end;

function TWatchVariable.Find(AIndex: LongWord): Boolean;
var
  i:Integer;
begin
  Result:=False;
  if AIndex < FFirstIndex then
    Exit;
  for i:= FList.Count - 1 downto 0 do
  begin
    if AIndex >= PVariableValue(FList[i])^.Index then
    begin
      Result := True;
      FCurrentIndex := i;
      {$ifdef DEBUG_WATCHLIST}
      Str(AIndex,TempDbgStr);
      Logger.Send('Found a value - RequestDate ',TempDbgStr);
      Str(PVariableValue(FList[i])^.Index,TempDbgStr);
      Logger.Send('              Variable Date: %s /  Value: %s',[TempDbgStr,Value]);
      {$endif}
      Exit;
    end;
  end;
end;

{ TWatchList }
{$ifdef DEBUG_WATCHLIST}
procedure TWatchList.DumpList;
var
  i:Integer;
begin
  for i:= 0 to FList.Count-1 do
  begin
    Logger.EnterMethod(TWatchVariable(FList[i]).Name);
    TWatchVariable(FList[i]).DumpVariable;
    Logger.ExitMethod(TWatchVariable(FList[i]).Name);
  end;
end;
{$endif}

function TWatchList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchList.GetItems(AValue: Integer): TWatchVariable;
begin
  Result := TWatchVariable(FList[AValue]);
end;

constructor TWatchList.Create;
begin
  FList := TFPList.Create;
end;

destructor TWatchList.Destroy;
begin
  Clear;
  FList.Destroy;
end;

function TWatchList.IndexOf(const AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if Uppercase(TWatchVariable(FList[i]).Name) = Uppercase(AName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TWatchList.Add(const ANameValue: String; AIndex: LongWord; SkipOnNewVariable: Boolean);
var
  PosEqual,i:Integer;
  TempStr: String;
begin
  PosEqual := Pos('=',ANameValue);
  TempStr := Copy(ANameValue,1,PosEqual-1);
  i := IndexOf(TempStr);
  if i = -1 then
  begin
    i := FList.Add(TWatchVariable.Create(TempStr,AIndex));
    if not SkipOnNewVariable then
      FOnNewVariable(TempStr, i);
  end;
  TempStr := Copy(ANameValue,PosEqual+1,Length(ANameValue)-PosEqual);
  TWatchVariable(FList[i]).AddValue(TempStr,AIndex);
end;

procedure TWatchList.Clear;
var
  i:Integer;
begin
  {$ifdef DEBUG_WATCHLIST}
  DumpList;
  {$endif}
  for i:= 0 to FList.Count - 1 do
    TWatchVariable(FList[i]).Destroy;
  FList.Clear;
end;

procedure TWatchList.Update(AIndex: LongWord);
var
  i: Integer;
begin
  {$ifdef DEBUG_WATCHLIST}
  DumpList;
  {$endif}
  for i:= 0 to FList.Count - 1 do
  begin
    with TWatchVariable(FList[i]) do
    begin
      if Find(AIndex) then
        FOnUpdate(Name,CurrentValue);
    end;
  end;
end;

end.

