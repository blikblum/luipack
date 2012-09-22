unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, delphicompat, LMessages, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    SetTimer1Button: TButton;
    SetTimer2Button: TButton;
    SetTimer3Button: TButton;
    KillTimer1Button: TButton;
    KillTimer2Button: TButton;
    KillTimer3Button: TButton;
    SetTimer3bButton: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure SetTimer1ButtonClick(Sender: TObject);
    procedure SetTimer2ButtonClick(Sender: TObject);
    procedure SetTimer3ButtonClick(Sender: TObject);
    procedure KillTimer1ButtonClick(Sender: TObject);
    procedure KillTimer2ButtonClick(Sender: TObject);
    procedure KillTimer3ButtonClick(Sender: TObject);
    procedure SetTimer3bButtonClick(Sender: TObject);
  protected
    procedure WMTimer(var Message: TLMTimer); message LM_TIMER;
  private
    procedure TimerCallback(AId: LongWord);
    procedure TimerCallbackNew(AId: LongWord);
    procedure TimerCallbackOther(AId: LongWord);
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

const
  Timer1 = 1;
  Timer2 = 2;
  Timer3 = 3;

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TMainForm.SetTimer1ButtonClick(Sender: TObject);
begin
  SetTimer(Handle,Timer1,1000,nil);
end;

procedure TMainForm.SetTimer2ButtonClick(Sender: TObject);
begin
  SetTimer(Handle,Timer2,2000,nil);
end;

procedure TMainForm.SetTimer3ButtonClick(Sender: TObject);
begin
  SetTimer(Handle,Timer3,3000,@TimerCallbackNew);
end;

procedure TMainForm.KillTimer1ButtonClick(Sender: TObject);
begin
  KillTimer(Handle,Timer1);
end;

procedure TMainForm.KillTimer2ButtonClick(Sender: TObject);
begin
  KillTimer(Handle,Timer2);
end;

procedure TMainForm.KillTimer3ButtonClick(Sender: TObject);
begin
  KillTimer(Handle,Timer3);
end;

procedure TMainForm.SetTimer3bButtonClick(Sender: TObject);
begin
  SetTimer(Handle,Timer3,3000,@TimerCallbackOther);
end;

procedure TMainForm.WMTimer(var Message: TLMTimer);
var
  AStr: String;
begin
  case Message.TimerID of
    Timer1: AStr:='Timer1 called';
    Timer2: AStr:='Timer2 called';
    Timer3: AStr:='Timer3 called';
  else
    AStr:='Not Identified Timer: '+IntToStr(Message.TimerID);
  end;
  ListBox1.Items.Add('WMTimer - '+AStr);
end;

procedure TMainForm.TimerCallback(AId: LongWord);
var
  AStr: String;
begin
  case AId of
    Timer1: AStr:='Timer1 called';
    Timer2: AStr:='Timer2 called';
    Timer3: AStr:='Timer3 called';
  else
    AStr:='Not Identified Timer: '+IntToStr(AId);
  end;
  ListBox1.Items.Add('TimerCallback - '+AStr);
end;


procedure TMainForm.TimerCallbackNew(AId: LongWord);
var
  AStr: String;
begin
  AStr:='TimerCallbackNew called';
  ListBox1.Items.Add(AStr);
end;

procedure TMainForm.TimerCallbackOther(AId: LongWord);
var
  AStr: String;
begin
  AStr:='TimerCallbackOther called';
  ListBox1.Items.Add(AStr);
end;

initialization
  {$I Unit1.lrs}

end.

