unit filechannel;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, multilog;

type

  { TFileChannel }

  TFileChannel = class (TLogChannel)
  private
    FFileHandle: Text;
    FRelativeIdent: Integer;
    FBaseIdent: Integer;
    FShowTime: Boolean;
    FShowPrefix: Boolean;
    FShowStrings: Boolean;
    procedure SetShowTime(const AValue: Boolean);
    procedure UpdateIdentation;
    procedure WriteStrings(AStream: TStream);
    procedure WriteComponent(AStream: TStream);
  public
    constructor Create (const AFileName: String);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Deliver(const AMsg: TLogMessage);override;
    property ShowPrefix: Boolean read FShowPrefix write FShowPrefix;
    property ShowTime: Boolean read FShowTime write SetShowTime;
  end;

implementation

const
  LogPrefixes: array [ltInfo..ltWatch] of String = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<EXIT METHOD',
    'CONDITIONAL',
    'CHECKPOINT',
    'STRINGS',
    'CALL STACK',
    'OBJECT',
    'EXCEPTION',
    'BITMAP',
    'HEAP INFO',
    'MEMORY',
    '','','','','',
    'WATCH');

{ TFileChannel }

procedure TFileChannel.UpdateIdentation;
var
  S:String;
begin
  S:='';
  if FShowTime then
    S:=FormatDateTime('hh:nn:ss:zzz',Time);
  FBaseIdent:=Length(S)+3;
end;

procedure TFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime:=AValue;
  UpdateIdentation;
end;

procedure TFileChannel.WriteStrings(AStream: TStream);
var
  i: Integer;
begin
  if AStream.Size = 0 then Exit;
  with TStringList.Create do
  try
    AStream.Position:=0;
    LoadFromStream(AStream);
    for i:= 0 to Count - 1 do
      WriteLn(FFileHandle,Space(FRelativeIdent+FBaseIdent)+Strings[i]);
  finally
    Destroy;
  end;
end;

procedure TFileChannel.WriteComponent(AStream: TStream);
var
  TextStream: TStringStream;
  S:String;
begin
  TextStream:=TStringStream.Create(S);
  AStream.Seek(0,soFromBeginning);
  ObjectBinaryToText(AStream,TextStream);
  //todo: better handling of format
  Write(FFileHandle,TextStream.DataString);
  TextStream.Destroy;
end;

constructor TFileChannel.Create(const AFileName: String);
begin
  FShowPrefix:=True;
  FShowTime:=True;
  FShowStrings:=True;
  Active:=True;
  Assign(FFileHandle,AFileName);
  if FileExists(AFileName) then
    Append(FFileHandle)
  else
    Rewrite(FFileHandle);
  WriteLn(FFileHandle,'=== Log Session Started at ',DateTimeToStr(Now),' by ',ApplicationName,' ===');
  Close(FFileHandle);
  UpdateIdentation;
end;

destructor TFileChannel.Destroy;
begin
  //remove it?
end;

procedure TFileChannel.Clear;
begin
  Rewrite(FFileHandle);
end;

procedure TFileChannel.Deliver(const AMsg: TLogMessage);
begin
  Append(FFileHandle);
  //Exit method identation must be set before
  if AMsg.MsgType = ltExitMethod then
    if FRelativeIdent >= 2 then
      Dec(FRelativeIdent,2);
  if FShowTime then
    Write(FFileHandle,FormatDateTime('hh:nn:ss:zzz',AMsg.MsgTime)+' ');
  Write(FFileHandle,Space(FRelativeIdent));
  if FShowPrefix then
    Write(FFileHandle,LogPrefixes[AMsg.MsgType]+': ');
  Writeln(FFileHandle,AMsg.MsgText);
  if FShowStrings and (AMsg.Data <> nil) then
  begin
    case AMsg.MsgType of
      ltStrings,ltCallStack,ltHeapInfo,ltException:WriteStrings(AMsg.Data);
      ltObject:WriteComponent(AMsg.Data);
    end;
  end;
  Close(FFileHandle);
  //Update enter method identation
  if AMsg.MsgType = ltEnterMethod then
    Inc(FRelativeIdent,2);
end;

end.

