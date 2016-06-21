unit AppUpdateView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, AppUpdate, LazUTF8Classes, httpsend, synsock;

type

  TDownloadThread = class;

  { TAppUpdateForm }

  TAppUpdateForm = class(TForm)
    CurrentVersionLabel: TLabel;
    DownloadProgressBar: TProgressBar;
    DownloadTimer: TTimer;
    UpdateVersionLabel: TLabel;
    UpdateButton: TBitBtn;
    Label1: TLabel;
    SkipButton: TBitBtn;
    procedure DownloadTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  private
    FCurrentVersion: String;
    FUpdate: TAppUpdate;
    FThread: TDownloadThread;
    FTotalSize: Int64;
    FDownloaded: Int64;
    FError: string;
    procedure UpdateStatus(Data: PtrInt);
  public
    destructor Destroy; override;
  published
    property CurrentVersion: String read FCurrentVersion write FCurrentVersion;
    property Update: TAppUpdate read FUpdate write FUpdate;
  end;


  { TDownloadThread }

  TDownloadThread = class(TThread)
  private
    FHttp: THTTPSend;
    FForm: TAppUpdateForm;
    FUrl: string;
    FDestFileName: string;
    FOut: TFileStreamUTF8;
    procedure DoMonitor(Sender: TObject; {%H-}Writing: Boolean; const {%H-}Buffer: TMemory; Len: Integer);
    procedure WriteToFile;
  protected
    procedure Execute; override;
  end;

implementation

uses
  UTF8Process, process;

{ TDownloadThread }

procedure TDownloadThread.DoMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
begin
  if Terminated then begin
    FHttp.Abort;
    Exit;
  end;

  if FHttp.DownloadSize <> 0 then begin
    FForm.FTotalSize := FHttp.DownloadSize;
    Inc(FForm.FDownloaded, Len);
    WriteToFile;
  end;
end;

procedure TDownloadThread.WriteToFile;
begin
  if FOut = nil then
    FOut:=TFileStreamUTF8.Create(FDestFileName, fmCreate);

  FHttp.Document.Position := 0;
  FOut.CopyFrom(FHttp.Document, FHttp.Document.Size);
  FHttp.Document.Clear;
end;

procedure TDownloadThread.Execute;
var
  res: PtrInt;
begin
  res := 1;
  try
    FHttp:=THTTPSend.Create;
    try
      FHttp.Sock.OnMonitor := @DoMonitor;
      if FHttp.HTTPMethod('GET', FUrl) then begin
        if FHttp.ResultCode = 200 then begin
          FForm.FDownloaded:=FHttp.DownloadSize;
          WriteToFile;
          res:=2;
        end
        else
          if not Terminated then
            FForm.FError:=Format('HTTP error: %d', [FHttp.ResultCode]);
      end
      else
        if not Terminated then
          FForm.FError := FHttp.Sock.LastErrorDesc;
    finally
      FHttp.Free;
    end;
  except
    FForm.FError := Exception(ExceptObject).Message;
  end;
  FOut.Free;
  if res = 1 then
    DeleteFile(FDestFileName);
  //todo: check file content hash
  Application.QueueAsyncCall(@FForm.UpdateStatus, res);
  FForm.FThread:=nil;
end;

{$R *.lfm}

{ TAppUpdateForm }

procedure TAppUpdateForm.UpdateButtonClick(Sender: TObject);
begin
  UpdateButton.Enabled := False;
  SkipButton.Enabled := False;
  FThread := TDownloadThread.Create(True);
  FThread.FForm := Self;
  FThread.FUrl := Update.RemoteFilePath;
  FThread.FDestFileName := Update.LocalFilePath;
  DownloadTimer.Enabled := True;
  FThread.Start;
  Caption := Update.LocalFilePath;
end;

procedure TAppUpdateForm.DownloadTimerTimer(Sender: TObject);
begin
  if FTotalSize <> 0 then
  begin
    DownloadProgressBar.Max := FTotalSize;
    DownloadProgressBar.Position := FDownloaded;
  end;
end;

procedure TAppUpdateForm.FormShow(Sender: TObject);
begin
  CurrentVersionLabel.Caption := Format('Versão atual: %s', [FCurrentVersion]);
  UpdateVersionLabel.Caption := Format('Nova versão: %s', [Update.Version]);
end;

procedure TAppUpdateForm.UpdateStatus(Data: PtrInt);
var
  FileProcess: TProcessUTF8;
begin
  case Data of
  1:
    begin
      if FError <> '' then
        MessageDlg(Format('%s'+LineEnding+'Download de %s', [FError, Update.RemoteFilePath]), mtError, [mbOK], 0);
      SkipButton.Enabled := True;
    end;
  2:
    begin
      FileProcess := TProcessUTF8.Create(nil);
      try
        FileProcess.Executable := Update.LocalFilePath;
        FileProcess.Options := [];
        FileProcess.Execute;
        if FileProcess.Running then
          Application.Terminate;
      finally
        FileProcess.Destroy;
      end;
    end;
  end;
end;

destructor TAppUpdateForm.Destroy;
begin
  FThread.Free;
  inherited Destroy;
end;

end.

