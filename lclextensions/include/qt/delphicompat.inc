
{
  Qt Interface
  
  Initial implementation by Zeljan Rikalo

  SetTimer/KillTimer implementation by Luiz Americo
}

function CF_UNICODETEXT: TClipboardFormat;
begin
  //todo
  Result := TClipboardFormat(0);
end;

{$define HAS_GETCURRENTOBJECT}
{$define HAS_MAPMODEFUNCTIONS}
{$define HAS_GETTEXTEXTENTEXPOINT}
{$define HAS_GETDOUBLECLICKTIME}
{$define HAS_GETTEXTALIGN}
{$define HAS_GETWINDOWDC}
{$define HAS_INVERTRECT}
{$define HAS_OFFSETRGN}
{$define HAS_REDRAWWINDOW}
{$define HAS_SCROLLWINDOW}
{$define HAS_SETBRUSHORGEX}


{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}

{
function GetBkColor(DC:HDC):COLORREF;
begin
  //TODO: FIXME
  if QtWidgetSet.IsValidDC(DC) then
    Result := TQtDeviceContext(DC).CurrentBackColor.ColorRef
  else
    Result := CLR_INVALID;
end;
}

function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := 0;
  if QtWidgetSet.IsValidDC(hdc) then
  with TQtDeviceContext(hdc) do
  begin      {TODO: FIXME}
    case uObjectType of
      OBJ_BITMAP: Result := HGDIOBJ(vImage);
      OBJ_BRUSH: Result := HGDIOBJ(vBrush);
      OBJ_FONT: Result := HGDIOBJ(vFont);
      OBJ_PEN: Result := HGDIOBJ(vPen);
    end;
  end;
end;

function GetDoubleClickTime: UINT;
begin
  Result := QApplication_doubleClickInterval;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  lbearing, rbearing, width, ascent, descent: LongInt;
  UseFont : TQtFont;
  IsDBCSFont: Boolean;
  NewCount, Accumulator, i: Integer;
  QtFontMetrics: QFontMetricsH;
begin
  //based in lcl code
  Result := QtWidgetSet.IsValidDC(DC);
  if Result then
  with TQtDeviceContext(DC) do
  begin
    if (vFont = nil) or (vFont.Widget = nil)
    then begin
      UseFont := TQtFont.Create(True); //QtWidgetSet.GetDefaultGtkFont(false);
    end
    else begin
      UseFont := vFont;
    end;
    if UseFont = nil then
      DebugLn('WARNING: [TQtWidgetSet.GetTextExtentPoint] Missing font')
    else begin
      QtFontMetrics := QFontMetrics_create(UseFont.Widget);
      try
      descent:=0;
      {
      UpdateDCTextMetric(TDeviceContext(DC));
      IsDBCSFont:=TDeviceContext(DC).DCTextMetric.IsDoubleByteChar;

      if IsDBCSFont then begin
        NewCount:=Count*2;
        if FExtUTF8OutCacheSize<NewCount then begin
          ReAllocMem(FExtUTF8OutCache,NewCount);
          FExtUTF8OutCacheSize:=NewCount;
        end;
        NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
        gdk_text_extents(UseFont, FExtUTF8OutCache, NewCount,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end else begin
        gdk_text_extents(UseFont, Str, Count,
                         @lbearing, @rBearing, @width, @ascent, @descent);
      end;
      }
      {TODO: FIXME !}
      // gdk_text_extents(UseFont, Str, Count,@lbearing, @rBearing, @width, @ascent, @descent);
      ascent := QFontMetrics_ascent(QtFontMetrics);
      descent := QFontMetrics_descent(QtFontMetrics);
      Size.cX := Width;
      Size.cY := ascent+descent;
      if PartialWidths <> nil then
      begin
        Accumulator:=0;
        for i:= 0 to Count - 1 do
        begin
          {TODO: FIXME !}
          Inc(Accumulator,QFontMetrics_width(QtFontMetrics,PWideChar((Str+i))));
          // gdk_char_width(UseFont,(Str+i)^));
          PartialWidths[i] := Accumulator;
        end;
      end;

      finally
        QFontMetrics_destroy(QtFontMetrics);
      end;
    end;
  end;
end;

function GetMapMode(DC: HDC): LongInt;
begin
  {TODO: FIXME}
  Result := MM_TEXT;
end;

function GetTextAlign(hDC:HDC): LongWord;
var
  QtDC: TQtDeviceContext;
  QtFontMetrics: QFontMetricsH;
  QtFont: QFontH;
begin
  Result := 0;
  if not QtWidgetSet.IsValidDC(hdC) then
    Exit;
  QtDC := TQtDeviceContext(hDC);
  QtFont := QtDC.vFont.Widget;
  QtFontMetrics := QFontMetrics_create(QtFont);
  try
  {TODO: FIXME we should save somehow text flags into QtDC
   cause we don't have any function which returns current flags !}
  finally
    QFontMetrics_destroy(QtFontMetrics);
  end;
end;

function GetWindowDC(hWnd:THandle): HDC;
begin
  if hWnd = 0 then
    Exit;
  //todo: see if is necessary to check the handle validity
  Result := TQtWidget(hWnd).Context;
end;

function InvertRect(DC: HDC; const lprc: TRect): Boolean;
var
  DCOrigin: TQtPoint;
begin
  //todo: see the windows result when rect is invalid
  Result := QtWidgetSet.IsValidDC(DC) and (lprc.Bottom > lprc.Top)
    and (lprc.Right > lprc.Left);
  if Result then
  begin
    with lprc do
      Result := BitBlt(DC, Left, Top, Right - Left, Bottom-Top,
        DC, Left, Top, LongWord(QPainterCompositionMode_DestinationOver));
    {TODO: FIXME !}
  end;
end;

{
function LPtoDP(DC: HDC; var Points; Count: Integer): BOOLEAN;
var
  P: TQtPoint;
begin
  //TODO: Use QPainter_combinedTransform(), but we are missing QTransform implementation !
  //Result := False;
end;
}

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
var
  Region: TQtRegion;
begin
  Region := TQtRegion(hrgn);
  QRegion_translate(Region.Widget, nxOffset, nYOffset);
  Result := Region.GetRegionType;
end;

function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
var
  QtWidget: TQtWidget;
  Region: TQtRegion;
begin
  {TODO: Check msdn docs for flags ... this implementation could raise AV !}
  QtWidget := TQtWidget(hWnd);
  if (lprcUpdate = nil) and (hrgnUpdate = 0) then
    QWidget_update(QtWidget.Widget)
  else
  begin
    if lprcUpdate <> nil then
      QWidget_update(QtWidget.Widget, lprcUpdate)
    else
    begin
      Region := TQtRegion(hrgnUpdate);
      QWidget_update(QtWidget.Widget, Region.Widget);
    end;
  end;
end;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT): Boolean;
begin
  if hWnd = 0 then
    Exit;
  QWidget_scroll(TQtWidget(hWnd).Widget, XAmount, YAmount, lpRect);
end;

function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
var
  QtDC: TQtDeviceContext;
begin
  Result := False;
  if not QtWidgetSet.IsValidDC(DC) then
    Exit;
  QtDC := TQtDeviceContext(DC);
  if lppt <> nil then
    QtDC.getBrushOrigin(lppt);
  QtDC.setBrushOrigin(nXorg, nYOrg);
  Result := True;
end;

function SetMapMode(DC: HDC; fnMapMode: LongInt): LongInt;
var
  Matrix: QMatrixH;
  MatrixNew: QMatrixH;
  dpi: TSize;
  m11, m22: Double;
  dx, dy: Double;
  QtDC: TQtDeviceContext;

  procedure SetM(const Am11, Am22: Double);
  begin
    m11 := Am11;
    m22 := Am22;
  end;

begin
  Result := 0;
  if not QtWidgetSet.IsValidDC(DC) then
    Exit;

  {$note implement - wait for LCL}

  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  WriteLn('***** [WinAPI TQtWidgetSet.SetMapMode] missing implementation ');
  {$endif}

  QtDC := TQtDeviceContext(DC);

  dpi.cx := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSX);
  dpi.cy := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSY);

  Result := GetMapMode(DC);

  case fnMapMode of
    MM_TEXT:
      SetM(1, 1);

    MM_LOMETRIC:
      SetM((dpi.cx / 2.54) / 100, -(dpi.cy / 2.54) / 100);
    MM_HIMETRIC:
      SetM((dpi.cx / 2.54) / 1000, -(dpi.cy / 2.54) / 1000);

    MM_LOENGLISH:
      SetM(dpi.cx / 10, -dpi.cy / 10);
    MM_HIENGLISH:
      SetM(dpi.cx / 100, -dpi.cy / 100);

    MM_TWIPS:
      SetM(dpi.cx / 1440, -dpi.cy / 1440);
    //MM_ISOTROPIC:
  end;

  if QPainter_worldMatrixEnabled(QtDC.Widget) then
  begin
    Matrix := QPainter_worldMatrix(QtDC.Widget);
    dx := QMatrix_dx(Matrix);
    dy := QMatrix_dy(Matrix);
  end else
  begin
    dx := 0;
    dy := 0;
  end;

  MatrixNew := QMatrix_create(m11, 0, 0, m22, dx, dy);
  try
    QPainter_setWorldMatrix(QtDC.Widget, MatrixNew, False);
  finally
    QMatrix_destroy(MatrixNew);
  end;
end;

type

  { TQtTimerEx }

  TQtTimerEx = class(TQtObject)
  private
    FCallbackFunc: TTimerNotify;
    FUserId: LongWord;
    FControl: TControl;
    FAppObject: QObjectH;
  public
    constructor Create(TimerFunc: TTimerNotify;
      UserId: LongWord; Control: TControl); virtual;
    destructor Destroy; override;
  public
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure Reset(TimerFunc: TTimerNotify; UserId: LongWord; Control: TControl);
    procedure Start(Interval: Integer);
    procedure Stop;
  end;


  { TTimerList }

  TTimerList = class
  private
    FMap: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify;
      WinControl: TControl): TQtTimerEx;
    function GetTimerObject(hWnd: THandle; ID: LongWord): TQtTimerEx;
  end;

  TQtWidgetSetHack = Class(TWidgetSet)
  private
    App: QApplicationH;
  end;

{ TQtTimerEx }

constructor TQtTimerEx.Create(TimerFunc: TTimerNotify;
  UserId: LongWord; Control: TControl);
begin
  inherited Create;

  FCallbackFunc := TimerFunc;
  FUserId := UserId;
  FControl := Control;
  
  //very big ultra extreme hack to get the app from QtWidgetset
  TheObject := QTimer_create(TQtWidgetSetHack(QtWidgetSet).App);

  AttachEvents;
end;

destructor TQtTimerEx.Destroy;
begin
  FCallbackFunc := nil;
  FControl := nil;
  inherited Destroy;
end;

function TQtTimerEx.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  BeginEventProcessing;
  Result := False;

  if QEvent_type(Event) = QEventTimer then
  begin
    Result := True;

    QEvent_accept(Event);

    if Assigned(FCallbackFunc) then
      FCallbackFunc(FUserId)
    else
      if FControl <> nil then
        LCLSendTimerMsg(FControl, FUserId, 0);
  end;
  EndEventProcessing;
end;

procedure TQtTimerEx.Reset(TimerFunc: TTimerNotify;
  UserId: LongWord; Control: TControl);
begin
  FControl := Control;
  FCallbackFunc := TimerFunc;
  FUserId := UserId;
end;

procedure TQtTimerEx.Start(Interval: Integer);
begin
  //QTimer_setInterval(QTimerH(TheObject), Interval);
  QTimer_start(QTimerH(TheObject), Interval);
end;

procedure TQtTimerEx.Stop;
begin
  //todo: destroy the timer here instead of just stopping
  QTimer_stop(QTimerH(TheObject));
end;

var
  FTimerList: TTimerList;

function MakeQWord(d1, d2: dword): QWord; inline;
begin
  Result := (QWord(d2) shl 32) or d1;
end;

  
function KillTimer(hWnd: THandle; nIDEvent: LongWord):Boolean;
var
  TimerObject: TQtTimerEx;
begin
  TimerObject := FTimerList.GetTimerObject(hWnd, nIDEvent);
  //DebugLn('KillTimer HWnd: %d ID: %d TimerObject: %d',[hWnd, nIDEvent, PtrInt(TimerObject)]);
  if TimerObject <> nil then
    TimerObject.Stop;
end;

function SetTimer(hWnd: THandle; nIDEvent: LongWord; uElapse: LongWord; lpTimerFunc: TTimerNotify): LongWord;
var
  Control: TControl;
  TimerObject: TQtTimerEx;
begin
  if hWnd <> 0 then
    Control := FindControl(hWnd)
  else
    Control := nil;
  TimerObject := FTimerList.Add(hWnd, nIDEvent, lpTimerFunc, Control);
  TimerObject.Start(uElapse);
  //DebugLn('SetTimer HWnd: %d ID: %d TimerObject: %d',[hWnd, nIDEvent, PtrInt(TimerObject)]);
end;

function TTimerList.Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl): TQtTimerEx;
var
  AID: QWord;
begin
  AID := MakeQWord(hWnd, ID);
  with FMap do
  begin
    if HasId(AID) then
    begin
      //DebugLn('Reset timer for HWnd: %d ID: %d', [hWnd, ID]);
      GetData(AID, Result);
      Result.Reset(NotifyFunc, ID, WinControl);
    end
    else
    begin
      //DebugLn('Create timer for HWnd: %d ID: %d', [hWnd, ID]);
      Result := TQtTimerEx.Create(NotifyFunc, ID, WinControl);
      Add(AID, Result);
    end;
  end;
end;

constructor TTimerList.Create;
begin
  //todo: handle 64bit
  FMap := TMap.Create(itu8, SizeOf(TQtTimerEx));
end;

destructor TTimerList.Destroy;
var
  Iterator: TMapIterator;
  TimerObject: TQtTimerEx;
begin
  Iterator := TMapIterator.Create(FMap);
  with Iterator do
  begin
    while not EOM do
    begin
      GetData(TimerObject);
      TimerObject.Free;
      Next;
    end;
    Destroy;
  end;
  FMap.Destroy;
end;

function TTimerList.GetTimerObject(hWnd: THandle; ID: LongWord): TQtTimerEx;
var
  DataPtr: ^TQtTimerEx;
begin
  Result := nil;
  DataPtr := FMap.GetDataPtr(MakeQWord(hWnd, ID));
  if DataPtr <> nil then
    Result := DataPtr^;
end;

