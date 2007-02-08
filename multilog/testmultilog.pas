program testmultilog;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here },filechannel, ipcchannel, sharedlogger,multilog;
  
function GetCustomData (Sender: TLogger; Data: Pointer; var DoSend: Boolean): String;
begin
  Result:='CustomData'+LineEnding+'SecondLine';
  if Integer(Data) = 1 then
    DoSend:=False;
end;

var
  AList: TStrings;
  AStr: String;
begin
  AStr:='bdxnmczbkjsdhgfjs';
  AList:=TStringList.Create;
  with Logger do
  begin
    Channels.Add(TFileChannel.Create('test.log'));
    Channels.Add(TIPCChannel.Create);
    ActiveClasses:=[0,1];
    DefaultClass:=1;

    Send('An empty StringList',AList);
    Send('A Text Message');
    Send('Another Text Message');   
    with AList do
    begin
      Add('aaaaaa');
      Add('bbbbbb');
      Add('cccccc'); 
    end;
    Watch('Watch',78);
    SendError('A Error Message');
    SendMemory('The memory of a String',PChar(AStr),Length(AStr));
    SendCustomData('CustomData to be sent',nil,@GetCustomData);
    SendCustomData('CustomData NOT to be sent',Pointer(1),@GetCustomData);
    EnterMethod('DoIt');
    Send('AText inside DoIt');
    SendWarning('A Warning');
    Send('A String','sadjfgadsfbmsandfb');
    Send('AInteger',4957);
    Send('A Boolean',True);
    SendCallStack('A CallStack example');
    ExitMethod('DoIt');
    Send('A StringList',AList);
    DefaultClass:=2;
    Send('This Text Should NOT be logged');
    Send(1,'This Text Should be logged');
    ActiveClasses:=[0];
    Send(1,'But This Text Should NOT');
  end;
  AList.Destroy;
end.

