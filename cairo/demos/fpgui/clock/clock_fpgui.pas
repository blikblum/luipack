program clock_fpgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_imgfmt_bmp, fpg_dialogs,
  CairofpGui, CairoClasses, CairoUtils, Cairo;

const
  m_radius = 0.42;
  m_lineWidth = 0.05;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    procedure   TimerFired(Sender: TObject);
    procedure   FormDestroy(Sender: TObject);
    procedure   FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure   PaintBoxDraw(Sender: TObject);
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
       var consumed: boolean); override;
  public
    Timer: TfpgTimer;
    PaintBox: TCairoPaintBox;
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PaintBox := TCairoPaintBox.Create(Self);
  with PaintBox do
  begin
    Parent    := Self;
    Left      := 0;
    Top       := 0;
    Width     := Self.Width;
    Height    := Self.Height;
    Visible   := True;
    Align     := alClient;
    OnDraw    := @PaintBoxDraw;
  end;
  Timer := TfpgTimer.Create(1000);
  Timer.OnTimer := @TimerFired;
  Timer.Enabled := True;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Timer.Destroy;
end;

procedure TMainForm.PaintBoxDraw(Sender: TObject);
var
  i: Integer;
  Hour, Second, Minute, MSecond: Word;
  inset, HourAngle, MinuteAngle, SecondAngle: Double;
begin
  with PaintBox, Context do
  begin
    // scale to unit square and translate (0, 0) to be (0.5, 0.5), i.e.
    // the center of the window
    Scale(Width, Height);
    Translate(0.5, 0.5);
    LineWidth := m_lineWidth;

    Save;
    SetSourceRGBA(0.337, 0.612, 0.117, 0.9);   // green
    Paint;
    Restore;

    Arc(0, 0, m_radius, 0, 2 * PI);

    Save;
    SetSourceRGBA(1.0, 1.0, 1.0, 0.8);
    FillPreserve;
    Restore;
    
    StrokePreserve;
    Clip;

    //clock ticks
    for i := 0 to 11 do
    begin
      inset := 0.05;
      Save;
      LineCap := CAIRO_LINE_CAP_ROUND;

      if (i mod 3 <> 0) then
      begin
        inset := inset*0.8;
        LineWidth := 0.03;
      end;

      MoveTo( (m_radius - inset) * cos (i * PI / 6),
              (m_radius - inset) * sin (i * PI / 6));
      LineTo (m_radius * cos (i * PI / 6),
              m_radius * sin (i * PI / 6));
      Stroke;
      Restore; (* stack-pen-size *)
    end;

    // store the current time
    DecodeTime(Time, Hour, Minute, Second, MSecond);

    // compute the angles of the indicators of our clock
    MinuteAngle := Minute * PI / 30;
    HourAngle := hour * PI / 6;
    SecondAngle := Second * PI / 30;

    Save;
    LineCap := CAIRO_LINE_CAP_ROUND;

    // draw the seconds hand
    Save;
    LineWidth := m_lineWidth / 3;
    SetSourceRGBA(0.7, 0.7, 0.7, 0.8); // gray
    MoveTo(0, 0);
    LineTo(sin(SecondAngle) * (m_radius * 0.9),
            -cos(SecondAngle) * (m_radius * 0.9));
    Stroke;
    Restore;

    // draw the minutes hand
    SetSourceRGBA(0.117, 0.337, 0.612, 0.9);   // blue
    MoveTo(0, 0);
    LineTo(sin(MinuteAngle + SecondAngle / 60) * (m_radius * 0.8),
            -cos(MinuteAngle + SecondAngle / 60) * (m_radius * 0.8));
    Stroke;

    // draw the hours hand
    SetSourceRGBA(0.337, 0.612, 0.117, 0.9);   // green
    MoveTo(0, 0);
    LineTo(sin(HourAngle + MinuteAngle / 12.0) * (m_radius * 0.5),
            -cos(HourAngle + MinuteAngle / 12.0) * (m_radius * 0.5));
    Stroke;
    Restore;

    // draw a little dot in the middle
    Arc(0, 0, m_lineWidth / 3.0, 0, 2 * PI);
    Fill;
  end;
end;

procedure TMainForm.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then
  begin
    consumed := True;
    Close;
  end
  else if keycode = keyF1 then
  begin
    ShowMessage('F1 - Shows this help' + #13 +
                'F11 - Toggles fullscreen mode (X11 only)' + #13 +
                'Esc - Exits the application', 'Quick Help');
  end
  else if keycode = keyF11 then
  begin
    FullScreen := not FullScreen;
    SetFullscreen(FullScreen);
  end;
  
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TMainForm.TimerFired(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(316, 186, 400, 400);
  WindowTitle := 'Cairo Clock Demo Running Under fpGUI';
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


