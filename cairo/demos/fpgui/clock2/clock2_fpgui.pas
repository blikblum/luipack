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
    FTimer: TfpgTimer;
    {@VFD_HEAD_BEGIN: MainForm}
    FPanelTop: TfpgPanel;
    FCheckDigital: TfpgCheckBox;
    FCheckDrawShadow: TfpgCheckBox;
    FCheckShowSeconds: TfpgCheckBox;
    FClock: TJanaClock;
    {@VFD_HEAD_END: MainForm}
    procedure TimerFired(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DigitalChanged(Sender: TObject);
    procedure DrawShadowChanged(Sender: TObject);
    procedure ShowSecondsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;


{@VFD_NEWFORM_DECL}


{@VFD_NEWFORM_IMPL}


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

  FTimer := TfpgTimer.Create(100);
  FTimer.OnTimer := @TimerFired;
  FTimer.Enabled := True;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 429, 341);
  WindowTitle := 'Cairo Clock Demo 2 Under fpGUI';
  WindowPosition := wpScreenCenter;

  FPanelTop := TfpgPanel.Create(self);
  with FPanelTop do
  begin
    Name := 'FPanelTop';
    SetPosition(0, 0, 429, 40);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
  end;

  FCheckDigital := TfpgCheckBox.Create(FPanelTop);
  with FCheckDigital do
  begin
    Name := 'FCheckDigital';
    SetPosition(4, 8, 88, 20);
    FontDesc := '#Label1';
    Text := 'Digital';
    OnChange := @DigitalChanged;
  end;

  FCheckDrawShadow := TfpgCheckBox.Create(FPanelTop);
  with FCheckDrawShadow do
  begin
    Name := 'FCheckDrawShadow';
    SetPosition(92, 8, 116, 20);
    FontDesc := '#Label1';
    TabOrder := 1;
    Text := 'Draw Shadow';
    OnChange := @DrawShadowChanged;
  end;

  FCheckShowSeconds := TfpgCheckBox.Create(FPanelTop);
  with FCheckShowSeconds do
  begin
    Name := 'FCheckShowSeconds';
    SetPosition(224, 8, 120, 20);
    FontDesc := '#Label1';
    TabOrder := 2;
    Text := 'Show Seconds';
    OnChange := @ShowSecondsChanged;
  end;

  FClock := TJanaClock.Create(self);
  with FClock do
  begin
    Name := 'FClock';
    SetPosition(0, 40, 429, 301);
    Anchors := [anLeft,anRight,anTop,anBottom];
  end;

  {@VFD_BODY_END: MainForm}
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


