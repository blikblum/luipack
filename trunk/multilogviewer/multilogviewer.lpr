program multilogviewer;

{$mode objfpc}{$H+}
{. $Define DEBUG_MULTILOGVIEWER}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { add your units here } fMain, sharedlogger
  {$ifdef DEBUG_MULTILOGVIEWER}, filechannel{$endif}
  {$ifdef LCLWin32}, UniqueInstanceRaw{$endif};

begin
  {$ifdef LCLWin32}
  if InstanceRunning('multilogviewer') then
    Exit;
  {$endif}
  {$ifdef DEBUG_MULTILOGVIEWER}
  Logger.ActiveClasses:=lcAll;
  Logger.Channels.Add(TFileChannel.Create('viewer.log'));
  Logger.Clear;
  {$else}
  Logger.ActiveClasses:=[];
  {$endif}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

