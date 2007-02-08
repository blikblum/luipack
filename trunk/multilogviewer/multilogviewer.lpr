program multilogviewer;

{$mode objfpc}{$H+}
{. $Define DEBUG_MULTILOGVIEWER}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, fmain, sharedlogger,filechannel, atbinhex_lcl;

begin
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

