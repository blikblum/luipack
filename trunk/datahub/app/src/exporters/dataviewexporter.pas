unit DataViewExporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataView;

type

  TDataViewExporter = class
  public
    class function Description: String; virtual; abstract;
    class procedure Execute(DataView: TDataView); virtual; abstract;
  end;

  TDataViewExporterClass = class of TDataViewExporter;

  { TExporterClassStore }

  TExporterClassStore = class(TPersistent)
  private
    FList: TFPList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TDataViewExporterClass;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Register(ExporterClass: TDataViewExporterClass);
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TDataViewExporterClass read GetItems; default;
  end;

  function ExporterClassStore: TExporterClassStore;

implementation

var
  _ExporterManager: TExporterClassStore;

function ExporterClassStore: TExporterClassStore;
begin
  Result := _ExporterManager;
end;

{ TExporterClassStore }

function TExporterClassStore.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TExporterClassStore.GetItems(Index: Integer): TDataViewExporterClass;
begin
  Result := TDataViewExporterClass(FList[Index]);
end;

procedure TExporterClassStore.AssignTo(Dest: TPersistent);
var
  i: Integer;
  Exporter: TDataViewExporterClass;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).Clear;
    for i := 0 to FList.Count -1 do
    begin
      Exporter := TDataViewExporterClass(FList[i]);
      TStrings(Dest).AddObject(Exporter.Description, TObject(Exporter));
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TExporterClassStore.Register(
  ExporterClass: TDataViewExporterClass);
begin
  Assert(ExporterClass <> nil, 'TDataViewExporterManager.RegisterExporter ExporterClass = nil');
  FList.Add(ExporterClass);
end;

constructor TExporterClassStore.Create;
begin
  FList := TFpList.Create;
end;

destructor TExporterClassStore.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

initialization
  _ExporterManager := TExporterClassStore.Create;

finalization
  _ExporterManager.Destroy;

end.

