unit fmain;

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

interface

uses
  Classes, multilog, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,simpleipc,
  ComCtrls, Buttons,LCLIntf,LCLType,LCLProc, logtreeview;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    imgToolIcons: TImageList;
    LogTreeView1: TLogTreeView;
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    tbutExpand: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tbutCollapse: TToolButton;
    tbutClear: TToolButton;
    tbutStayOnTop: TToolButton;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReceiveMessage(Sender: TObject);
    procedure tbutClearClick(Sender: TObject);
    procedure tbutCollapseClick(Sender: TObject);
    procedure tbutExpandClick(Sender: TObject);
    procedure tbutStayOnTopClick(Sender: TObject);
  private
    FIPCServer: TSimpleIPCServer;
    FMessageCount: Integer;
    procedure UpdateStatusBar;
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef unix}
  Application.OnIdle:=@ApplicationIdle;
  {$endif}
  FIPCServer:=TSimpleIPCServer.Create(nil);
  with FIPCServer do
  begin
    ServerID:='ipc_log_server';
    Global:=True;
    OnMessage:=@ReceiveMessage;
    StartServer;
  end;
  UpdateStatusBar;
end;

procedure TfrmMain.ApplicationIdle(Sender: TObject; var Done: Boolean
  );
begin
  FIPCServer.PeekMessage(1,True);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FIPCServer.Destroy;
end;

procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  AMsg:TLogMessage;
  TextSize,DataSize:Integer;
begin
  with TSimpleIPCServer(Sender).MsgData do
  begin
    Seek(0,soFromBeginning);
    ReadBuffer(AMsg.MsgType,SizeOf(Integer));
    ReadBuffer(AMsg.MsgTime,SizeOf(TDateTime));
    ReadBuffer(TextSize,SizeOf(Integer));
    SetLength(AMsg.MsgText,TextSize);
    ReadBuffer(AMsg.MsgText[1],TextSize);
    ReadBuffer(DataSize,SizeOf(Integer));
    if DataSize > 0 then
    begin
      AMsg.Data:=TMemoryStream.Create;
      //WriteLn('[LogViewer] DataSize: ',DataSize);
      //WriteLn('DataCopied: ',AMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,DataSize));
      AMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,DataSize);
    end
    else
      AMsg.Data:=nil;
    LogTreeView1.AddMessage(AMsg);
    if DataSize > 0 then
      AMsg.Data.Free;
  end;
  Inc(FMessageCount);
  UpdateStatusBar;
end;

procedure TfrmMain.tbutClearClick(Sender: TObject);
begin
  LogTreeView1.Clear;
  FMessageCount:=0;
  UpdateStatusBar;
end;

procedure TfrmMain.tbutCollapseClick(Sender: TObject);
begin
  LogTreeView1.FullCollapse;
end;

procedure TfrmMain.tbutExpandClick(Sender: TObject);
begin
  LogTreeView1.FullExpand
end;

procedure TfrmMain.tbutStayOnTopClick(Sender: TObject);
begin
  if tbutStayOnTop.Down then
    SetWindowPos(Handle,HWND_TOPMOST,0,0,0,0,SWP_NOSIZE or SWP_NOMOVE)
  else
    SetWindowPos(Handle,HWND_NOTOPMOST,0,0,0,0,SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TfrmMain.UpdateStatusBar;
begin
  StatusBarMain.SimpleText:=IntToStr(FMessageCount)+' messages';
end;

initialization
  {$I fmain.lrs}

end.

