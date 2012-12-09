unit MainREST;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer;

type

  { TMainRESTModule }

  TMainRESTModule = class(TRESTServiceModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainRESTModule: TMainRESTModule;

implementation

uses
  RESTResources;

{ TMainRESTModule }

procedure TMainRESTModule.DataModuleCreate(Sender: TObject);
begin
  Resources.Register('people', TPeople, 0);
end;

{$R *.lfm}

initialization
  RegisterHTTPModule('TMainRESTModule', TMainRESTModule);
end.

