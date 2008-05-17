program clock2_fpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  gfxbase, fpgfx, gui_form, gfx_imgfmt_bmp, JanaClock, gui_panel, gui_checkbox;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    FClock: TJanaClock;
    FTimer: TfpgTimer;
    FPanelTop: TfpgPanel;
    FCheckDigital: TfpgCheckBox;
    FCheckDrawShadow: TfpgCheckBox;
    FCheckShowSeconds: TfpgCheckBox;
    procedure TimerFired(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DigitalChanged(Sender: TObject);
    procedure DrawShadowChanged(Sender: TObject);
    procedure ShowSecondsChanged(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTimer.Free;
end;

procedure TMainForm.TimerFired(Sender: TObject);
begin
  FClock.Time := Time;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTimer.Enabled := False;
end;

procedure TMainForm.DigitalChanged(Sender: TObject);
begin
  FClock.Digital := FCheckDigital.Checked;
end;

procedure TMainForm.DrawShadowChanged(Sender: TObject);
begin
  FClock.DrawShadow := FCheckDrawShadow.Checked;
end;

procedure TMainForm.ShowSecondsChanged(Sender: TObject);
begin
  FClock.ShowSeconds := FCheckShowSeconds.Checked;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanelTop := TfpgPanel.Create(Self);
  with FPanelTop do
  begin
    Parent := Self;
    Text := '';
    Align := alTop;
    Height := 40;
  end;
  FCheckDigital := TfpgCheckBox.Create(FPanelTop);

  with FCheckDigital do
  begin
    Parent := FPanelTop;
    Align := alLeft;
    Text := 'Digital';
    OnChange := @DigitalChanged;
  end;

  FCheckDrawShadow := TfpgCheckBox.Create(FPanelTop);
  with FCheckDrawShadow do
  begin
    Parent := FPanelTop;
    Align := alLeft;
    Text := 'Draw Shadow';
    OnChange := @DrawShadowChanged;
  end;

  FCheckShowSeconds := TfpgCheckBox.Create(FPanelTop);
  with FCheckShowSeconds do
  begin
    Parent := FPanelTop;
    Align := alLeft;
    Text := 'Show Seconds';
    OnChange := @ShowSecondsChanged;
  end;

  FClock := TJanaClock.Create(Self);
  with FClock do
  begin
    Parent := Self;
    Align := alClient;
  end;
  FTimer := TfpgTimer.Create(100);
  FTimer.OnTimer := @TimerFired;
  FTimer.Enabled := True;
end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(316, 186, 429, 341);
  WindowTitle := 'Cairo Clock Demo 2';
  WindowPosition := wpScreenCenter;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


