unit dmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, ChronoDataProvider,
  sqlite3ds, db;

type

  { TdmMain }

  TdmMain = class(TDataModule)
    srcResults: TDatasource;
    dsResults: TSqlite3Dataset;
    dsCustomViews: TSqlite3Dataset;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ChronoData: TChronoDataProvider;
  end; 

var
  dmMain: TdmMain;

implementation

{$R *.lfm}

{ TdmMain }

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  ChronoData := TChronoDataProvider.Create;
  ChronoData.Dataset := dsResults;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
  ChronoData.Destroy;
end;

end.

