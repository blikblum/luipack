unit uniqueinstance;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, simpleipc;
  
type
  TOnOtherInstance = procedure (Sender : TObject; ParamCount: Integer; Parameters: array of String) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    FEnabled: Boolean;
    FIdentifier: String;
    FIPCServer: TSimpleIPCServer;
    FIPCClient: TSimpleIPCClient;
    FOnOtherInstance: TOnOtherInstance;
    function GetServerId: String;
    procedure ReceiveMessage(Sender: TObject);
    {$ifdef unix}
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    {$endif}
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Identifier: String read FIdentifier write FIdentifier;
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

implementation

uses
  StrUtils;

const
  BaseServerId = 'tuniqueinstance_';

{ TUniqueInstance }

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  TempArray: array of String;
  Count,i: Integer;

  procedure GetParams(const AStr: String);
  var
    pos1,pos2:Integer;
  begin
    //count num of params
    Count:=0;
    pos1:=1;
    pos2:=pos('|',AStr);
    while pos1 < pos2 do
    begin
      pos1:=pos2+1;
      pos2:=posex('|',AStr,pos1);
      Inc(Count);
    end;
    SetLength(TempArray,Count);
    //fill params
    Count:=0;
    pos1:=1;
    pos2:=pos('|',AStr);
    while pos1 < pos2 do
    begin
      TempArray[Count]:=Copy(AStr,pos1,pos2-pos1);
      pos1:=pos2+1;
      pos2:=posex('|',AStr,pos1);
      Inc(Count);
    end;
  end;

begin
  if Assigned(FOnOtherInstance) then
  begin
    GetParams(FIPCServer.StringMessage);
    FOnOtherInstance(Self,Count,TempArray);
    SetLength(TempArray,0);
  end;
end;

procedure TUniqueInstance.AppIdle(Sender: TObject; var Done: Boolean);
begin
  FIPCServer.PeekMessage(1, True);
end;

function TUniqueInstance.GetServerId: String;
begin
  if FIdentifier <> '' then
    Result:=BaseServerId+FIdentifier
  else
    Result:=BaseServerId+ExtractFileName(ParamStr(0));
end;


procedure TUniqueInstance.Loaded;
var
  TempStr:String;
  i:Integer;
begin
  if not (csDesigning in ComponentState) and FEnabled then
  begin
    FIPCClient.ServerId:=GetServerId;
    if FIPCClient.ServerRunning then
    begin
      //A instance is already running
      //Send a message and then exit
      TempStr:='';
      for i:= 1 to ParamCount do
        TempStr:=TempStr+ParamStr(i)+'|';
      FIPCClient.Active:=True;
      FIPCClient.SendStringMessage(TempStr);
      Application.ShowMainForm:=False;
      Application.Terminate;
    end
    else
    begin
      //It's the first instance. Init the server
      if FIPCServer = nil then
        FIPCServer:=TSimpleIPCServer.Create(Self);
      with FIPCServer do
      begin
        ServerID:=GetServerId;
        Global:=True;
        OnMessage:=@ReceiveMessage;
        StartServer;
      end;
      //there's no more need for FIPCClient
      FIPCClient.Free;
    end;
  end;//if
end;

constructor TUniqueInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIPCClient := TSimpleIPCClient.Create(Self);
  {$ifdef unix}
  Application.OnIdle := @AppIdle;
  {$endif}
end;


end.

