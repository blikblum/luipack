unit ipcchannel;

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
  Classes, SysUtils,simpleipc,multilog;

type

  { TIPCChannel }

  TIPCChannel = class (TLogChannel)
  private
    FClient: TSimpleIPCClient;
    FBuffer: TMemoryStream;
    FClearMessage: TLogMessage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Deliver(const AMsg: TLogMessage);override;
  end;


implementation

{ TIPCChannel }

constructor TIPCChannel.Create;
begin
  with FClearMessage do
  begin
    MsgType:=ltClear;
    //MsgText:='';
    //MsgTime:=Now;
    //Data:=nil;
    //Those are already nil
  end;
  FBuffer:=TMemoryStream.Create;
  FClient:=TSimpleIPCClient.Create(nil);
  with FClient do
  begin
    ServerID:='ipc_log_server';
    //todo: Start server only when channel is active
    if ServerRunning then
    begin
      Self.Active:=True;
      Active:=True;
      Connect;
    end
    else
      Self.Active:=False;
  end;
end;

destructor TIPCChannel.Destroy;
begin
  FClient.Destroy;
  FBuffer.Destroy;
end;

procedure TIPCChannel.Clear;
begin
  Deliver(FClearMessage);
end;

procedure TIPCChannel.Deliver(const AMsg: TLogMessage);
var
  TextSize, DataSize:Integer;
begin
  with FBuffer do
  begin
    TextSize:=Length(AMsg.MsgText);
    Seek(0,soFromBeginning);
    WriteBuffer(AMsg.MsgType,SizeOf(Integer));
    WriteBuffer(AMsg.MsgTime,SizeOf(TDateTime));
    WriteBuffer(TextSize,SizeOf(Integer));
    WriteBuffer(AMsg.MsgText[1],TextSize);
    if AMsg.Data <> nil then
    begin
      DataSize:=AMsg.Data.Size;
      //WriteLn('[IPCChannel] Size Of Stream: ',DataSize);
      WriteBuffer(DataSize,SizeOf(Integer));
      AMsg.Data.Position:=0;
      CopyFrom(AMsg.Data,DataSize);
      //WriteLn('DataCopied: ',CopyFrom(AMsg.Data,DataSize));
    end
    else
      WriteBuffer(Integer(0),SizeOf(Integer));//necessary?
  end;
  FClient.SendMessage(mtUnknown,FBuffer);
end;

end.

