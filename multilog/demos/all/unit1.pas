unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, sharedloggerlcl,
  Buttons, logtreeview, ExtCtrls, StdCtrls, Spin, ipcchannel, multilog;

type

  { TForm1 }

  TForm1 = class(TForm)
    butTestLog: TButton;
    butClear: TButton;
    butSubLog: TButton;
    butString: TButton;
    butInteger: TButton;
    butFloat: TButton;
    butBoolean: TButton;
    butEnterMethod: TButton;
    butExitMethod: TButton;
    butCalledBy: TButton;
    butCallStack: TButton;
    butHeapInfo: TButton;
    butException: TButton;
    butStrings: TButton;
    butInfo: TButton;
    ButSendMemory: TButton;
    ButGenericCheckPoint: TButton;
    ButAddNamedCheckPoint: TButton;
    ButOpenImage: TButton;
    ButSendBitmap: TButton;
    butWatchString: TButton;
    butWatchInteger: TButton;
    butWarning: TButton;
    butError: TButton;
    EditNamedCheckPoint: TEdit;
    EditWatchString: TEdit;
    EditInfo: TEdit;
    EditWarning: TEdit;
    EditError: TEdit;
    Image1: TImage;
    memoStrings: TMemo;
    butObject: TButton;
    comboBoolean: TComboBox;
    comboEnterMethod: TComboBox;
    editCalledBy: TEdit;
    editExitMethod: TEdit;
    editString: TEdit;
    OpenDialog1: TOpenDialog;
    PageBitmap: TPage;
    pageGeneral: TPage;
    spinWatchInteger: TSpinEdit;
    spinFloat: TFloatSpinEdit;
    LogTreeView1: TLogTreeView;
    Notebook1: TNotebook;
    pageWatches: TPage;
    pageSpecialized: TPage;
    pageMethods: TPage;
    pageVariables: TPage;
    spinInteger: TSpinEdit;
    Splitter1: TSplitter;
    procedure ButAddNamedCheckPointClick(Sender: TObject);
    procedure butBooleanClick(Sender: TObject);
    procedure butCalledByClick(Sender: TObject);
    procedure butCallStackClick(Sender: TObject);
    procedure butClearClick(Sender: TObject);
    procedure butEnterMethodClick(Sender: TObject);
    procedure butErrorClick(Sender: TObject);
    procedure butExceptionClick(Sender: TObject);
    procedure butExitMethodClick(Sender: TObject);
    procedure butFloatClick(Sender: TObject);
    procedure ButGenericCheckPointClick(Sender: TObject);
    procedure butHeapInfoClick(Sender: TObject);
    procedure butInfoClick(Sender: TObject);
    procedure butIntegerClick(Sender: TObject);
    procedure ButOpenImageClick(Sender: TObject);
    procedure ButSendBitmapClick(Sender: TObject);
    procedure ButSendMemoryClick(Sender: TObject);
    procedure butStringClick(Sender: TObject);
    procedure butStringsClick(Sender: TObject);
    procedure butWarningClick(Sender: TObject);
    procedure butWatchIntegerClick(Sender: TObject);
    procedure butWatchStringClick(Sender: TObject);
    procedure ObjectClick(Sender: TObject);
    procedure SubLogClick(Sender: TObject);
    procedure TestLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Logger do
  begin
    Channels.Add(LogTreeView1.Channel);
    Channels.Add(TIPCChannel.Create);
    DefaultClasses := [lcDebug];
  end;
end;

procedure TForm1.TestLogClick(Sender: TObject);
var
  AList:TStringList;
begin
  with Logger do
  begin
    ActiveClasses:=lcAll;
    EnterMethod(Sender,'TestLogClick');
    AList:=TStringList.Create;
    with AList do
    begin
      Add('aaaaaaa');
      Add('bbbbbbb');
      Add('ccccccc');
    end;
    Send('A Text Message');
    Send('Another Text Message');
    Send('A StringList', AList);
    AList.Destroy;
    SendError('A Error Message');
    SubLogClick(butSubLog);
    DefaultClasses := [lcWarning];
    ActiveClasses:=[lcDebug,lcInfo];
    Send('This Text Should NOT be logged');
    Send([lcDebug],'This Text Should be logged');
    ActiveClasses:=[];
    Send([lcDebug],'But This Text Should NOT');
    //Exitmethod is called even if not active if there's a unpaired EnterMethod
    ExitMethod(Sender,'TestLogClick');
    ActiveClasses:=lcAll;
  end;
end;

procedure TForm1.butClearClick(Sender: TObject);
begin
  Logger.Clear;
end;

procedure TForm1.butEnterMethodClick(Sender: TObject);
begin
  with comboEnterMethod do
  begin
    if (Text <> '') and (Items.IndexOf(Text) =-1) then
    begin
      Items.Add(Text);
      editExitMethod.Text:=Text;
      Logger.EnterMethod(Text);
    end;
  end;
end;

procedure TForm1.butErrorClick(Sender: TObject);
begin
  if EditError.Text <> '' then
    Logger.SendError(EditError.Text);
end;

procedure TForm1.butExceptionClick(Sender: TObject);
begin
  try
    StrToInt('XXXXX');
  except
    On E: Exception do
      Logger.SendException('An Exception example',E);
  end
end;

procedure TForm1.butExitMethodClick(Sender: TObject);
var
  i: Integer;
begin
  with editExitMethod do
  begin
    if Text <> '' then
    begin
      Logger.ExitMethod(Text);
      i:=comboEnterMethod.Items.IndexOf(Text);
      if i <> -1 then
        comboEnterMethod.Items.Delete(i);
      Dec(i);
      if i <> -1 then
        Text:=comboEnterMethod.Items[i]
      else
        Text:='';
    end;
  end;
end;

procedure TForm1.butBooleanClick(Sender: TObject);
begin
  Logger.Send('A Boolean Variable',Boolean(comboBoolean.ItemIndex));
end;

procedure TForm1.ButAddNamedCheckPointClick(Sender: TObject);
begin
  if EditNamedCheckPoint.Text <> '' then
    Logger.AddCheckPoint(EditNamedCheckPoint.Text);
end;

procedure TForm1.butCalledByClick(Sender: TObject);
begin
  if editCalledBy.Text <> '' then
    with Logger do
      SendIf('Send only if Called By '+editCalledBy.Text,CalledBy(editCalledBy.Text));
end;

procedure TForm1.butCallStackClick(Sender: TObject);
begin
  Logger.SendCallStack('A CallStack Example');
end;

procedure TForm1.butFloatClick(Sender: TObject);
begin
  Logger.Send('A Float Variable',spinFloat.Value);
end;

procedure TForm1.ButGenericCheckPointClick(Sender: TObject);
begin
  Logger.AddCheckPoint;
end;

procedure TForm1.butHeapInfoClick(Sender: TObject);
begin
  Logger.SendHeapInfo('A Heap Info Example');
end;

procedure TForm1.butInfoClick(Sender: TObject);
begin
  if EditInfo.Text <> '' then
    Logger.Send(EditInfo.Text);
end;

procedure TForm1.butIntegerClick(Sender: TObject);
begin
  Logger.Send('A Integer Variable',spinInteger.Value);
end;

procedure TForm1.ButOpenImageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.ButSendBitmapClick(Sender: TObject);
begin
  Logger.SendBitmap('ABitmap',Image1.Picture.Bitmap);
end;

procedure TForm1.ButSendMemoryClick(Sender: TObject);
var
 AStr: String;
begin
  AStr:='dfhejhrgtjehrgthjergthjergthjergterbdfngm';
  Logger.SendMemory('The memory of a string',PChar(AStr),Length(AStr));
end;

procedure TForm1.butStringClick(Sender: TObject);
begin
  Logger.Send('A String Variable',editString.Text);
end;

procedure TForm1.butStringsClick(Sender: TObject);
begin
  if memoStrings.Lines.Count > 0 then
    Logger.Send('A TStrings',memoStrings.Lines);
end;

procedure TForm1.butWarningClick(Sender: TObject);
begin
  if EditWarning.Text <> '' then
    Logger.SendWarning(EditWarning.Text);
end;

procedure TForm1.butWatchIntegerClick(Sender: TObject);
begin
  Logger.Watch('X',spinWatchInteger.Value);
end;

procedure TForm1.butWatchStringClick(Sender: TObject);
begin
  Logger.Watch('StrVar',EditWatchString.Text);
end;

procedure TForm1.ObjectClick(Sender: TObject);
begin
  Logger.Send('An TObject Example',Sender)
end;

procedure TForm1.SubLogClick(Sender: TObject);
var
  OldClasses: set of TDebugClass;
begin
  with Logger do
  begin
    OldClasses:=ActiveClasses;
    ActiveClasses:=lcAll;
    EnterMethod(Sender,'SubLogClick');
    SendIf('Only show if called by TestLogClick',CalledBy('TestLogClick'));
    Send('AText inside DoIt');
    SendWarning('AWarning');
    SendCallStack('CallStack example');
    Send('A String','sadjfgadsfbmsandfb');
    Send('AInteger',4957);
    Send('A Boolean',True);
    ExitMethod(Sender,'SubLogClick');
    ActiveClasses:=OldClasses;
  end;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  Logger.Channels.Remove(LogTreeView1.Channel);
end;

initialization
  {$I unit1.lrs}

end.

