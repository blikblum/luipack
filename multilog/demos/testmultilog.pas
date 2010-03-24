program testmultilog;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

uses
  Classes, SysUtils,
  filechannel, ipcchannel, sharedlogger;

type

  { TMyObject }

  TMyObject = class(TComponent)
  private
    FId: Integer;
  published
    property Id: Integer read FId write FId;
  end;

var
  AList: TStrings;
  Obj: TMyObject;

begin
  AList:=TStringList.Create;
  with Logger do
  begin
    Channels.Add(TFileChannel.Create('test.log'));
    Channels.Add(TIPCChannel.Create);
    ActiveClasses:=[0,1];
    DefaultClasses:= [1];
    IncCounter('Counter1');
    Watch('AStrWatch','XXXX');
    Send('An empty StringList',AList);
    IncCounter('CounteR1');
    Watch('AIntWatch',123);
    Send('A Text Message');
    IncCounter('CounTER1');
    Send('Another Text Message');
    with AList do
    begin
      Add('aaaaaa');
      Add('bbbbbb');
      Add('cccccc'); 
    end;
    DecCounter('Counter1');
    Watch('AIntWatch',321);
    SendError('A Error Message');
    ResetCounter('Counter1');
    Watch('ASTRWatch','YYYY');

    Obj := TMyObject.Create(nil);
    Obj.Name := 'The_Obj_Name';
    Obj.Id := 2;
    Send('An Object/Component', Obj);
    Obj.Destroy;

    EnterMethod('DoIt');
    Send('AText inside DoIt');
    SendWarning('A Warning');
    Send('A String','sadjfgadsfbmsandfb');
    Send('AInteger',4957);
    Send('A Boolean',True);
    SendCallStack('A CallStack example');
    ExitMethod('DoIt');
    Send('A StringList',AList);
    DefaultClasses:=[2];
    Send('This Text Should NOT be logged');
    Send([1],'This Text Should be logged');
    ActiveClasses:=[0];
    Send([1],'But This Text Should NOT');
  end;
  AList.Destroy;
end.

