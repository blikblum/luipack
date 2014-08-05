unit MainModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttp, HTTPDefs, LuiRESTServer, sqldb, sqlite3slimconn,
  LuiRESTSqldb, CAMICUResources;

type

  { TCAMICUServiceModule }

  TCAMICUServiceModule = class(TRESTServiceModule)
    Connection: TSQLite3Connection;
    Transaction: TSQLTransaction;
    procedure ConnectionAfterConnect(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FFactory: TCAMICUResourceFactory;
  public
  end;

var
  CAMICUServiceModule: TCAMICUServiceModule;

implementation

{$R *.lfm}

{ TCAMICUServiceModule }

procedure TCAMICUServiceModule.ConnectionAfterConnect(Sender: TObject);
begin
  Connection.ExecuteDirect('PRAGMA foreign_keys = ON');
end;

procedure TCAMICUServiceModule.DataModuleCreate(Sender: TObject);
begin
  TSqldbJSONResource.DefaultConnection := Connection;
  FFactory := TCAMICUResourceFactory.Create(Self);
  Resources.Register('serviceinfo', TServiceInfoResource, 0);
  Resources.Register('patients', @FFactory.GetResource, RES_PATIENTS);
  Resources.Register('evaluations', @FFactory.GetResource, RES_EVALUATIONS);
end;

procedure TCAMICUServiceModule.DataModuleDestroy(Sender: TObject);
begin
  TSqldbJSONResource.DefaultConnection := nil;
end;

end.

