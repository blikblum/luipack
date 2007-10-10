unit GnuPlotChart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDataSerie }

  TDataSerie = class
  private
    FName: String;
    FCount: Integer;
    FCapacity: Integer;
    FYValues: array of Double;
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create(const AName: String);
    procedure Add(YValue: Double);
    property Name: String read FName;
  end;
  
  { TDataSerieList }

  TDataSerieList = class
  private
    FList: TFpList;
    function GetCount: Integer;
    function GetItems(const SerieName: String): TDataSerie;
    function FindSerie(Index: Integer): TDataSerie;
    function FindSerie(const SerieName: String): TDataSerie;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function NewSerie(const SerieName: String): TDataSerie;
    property Items[SerieName: String]: TDataSerie read GetItems; default;
    property Count: Integer read GetCount;
  end;
  
  { TGnuPlotChart }

  TGnuPlotChart = class
  private
    FGnuPlotExe: String;
    FSeries: TDataSerieList;
    FXAxisLabels: TStrings;
    FXAxisTitle: String;
    FYAxisTitle: String;
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToFile(const FileName: String): Boolean;
    property GnuPlotExe: String read FGnuPlotExe write FGnuPlotExe;
    property Series: TDataSerieList read FSeries;
    property XAxisLabels: TStrings read FXAxisLabels;
    property XAxisTitle: String read FXAxisTitle write FXAxisTitle;
    property YAxisTitle: String read FYAxisTitle write FYAxisTitle;
  end;
  

implementation

uses
  strutils, process;

{ TDataSerieList }

function TDataSerieList.FindSerie(const SerieName: String): TDataSerie;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count -1 do
  begin
    if UpperCase(SerieName) = UpperCase(TDataSerie(FList[i]).Name) then
      Exit(TDataSerie(FList[i]));
  end;
end;

function TDataSerieList.GetItems(const SerieName: String): TDataSerie;
begin
  Result := FindSerie(SerieName);
  if Result = nil then
    Result := NewSerie(SerieName);
end;

function TDataSerieList.FindSerie(Index: Integer): TDataSerie;
begin
  Result := TDataSerie(FList[Index]);
end;

function TDataSerieList.GetCount: Integer;
begin
  Result := FList.Count;
end;

constructor TDataSerieList.Create;
begin
  FList := TFPList.Create;
end;

destructor TDataSerieList.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TDataSerieList.Clear;
begin

end;

function TDataSerieList.NewSerie(const SerieName: String): TDataSerie;
begin
  Result := TDataSerie.Create(SerieName);
  FList.Add(Result);
end;

{ TDataSerie }

procedure TDataSerie.SetCapacity(NewCapacity: Integer);
begin
  FCapacity := NewCapacity;
  SetLength(FYValues, FCapacity);
end;

constructor TDataSerie.Create(const AName: String);
begin
  FName := AName;
  SetCapacity(10);
end;

procedure TDataSerie.Add(YValue: Double);
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + (FCapacity div 4));
  FYValues[FCount] := YValue;
  Inc(FCount);
end;

{ TGnuPlotChart }

constructor TGnuPlotChart.Create;
begin
  FSeries := TDataSerieList.Create;
  FXAxisLabels := TStringList.Create;
end;

destructor TGnuPlotChart.Destroy;
begin
  FSeries.Destroy;
  FXAxisLabels.Destroy;
  inherited Destroy;
end;

function TGnuPlotChart.SaveToFile(const FileName: String): Boolean;
const
  plotlinetemplate = '''%s'' using 2:%d:xticlabels(1) with linespoints title "%s"';
var
  ScriptFile: Text;
  DataFile: Text;
  ScriptFilePath: String;
  DataFilePath: String;
  i,j: Integer;
  Process: TProcess;
begin
  if not FileExists(FGnuPlotExe) then
    Exit(False);
  if FileExists(FileName) then
    DeleteFile(FileName);
  //build data file
  DataFilePath := GetTempDir + 'gnuplotchart.data';
  Assign(DataFile, DataFilePath);
  Rewrite(DataFile);
  for j := 0 to FXAxisLabels.Count - 1 do
  begin
    Write(DataFile, '"',FXAxisLabels[j],'"');
    Write(DataFile,' ',j);
    for i := 0 to FSeries.Count - 1 do
      Write(DataFile, ' ',FloatToStr(FSeries.FindSerie(i).FYValues[j]));
    WriteLn(DataFile);
  end;
  System.Close(DataFile);
  //bild script file
  ScriptFilePath := GetTempDir + 'gnuplotchart.plot';
  Assign(ScriptFile, ScriptFilePath);
  Rewrite(ScriptFile);
  WriteLn(ScriptFile, 'set terminal png');
  WriteLn(ScriptFile, 'set output ''', ExpandFileName(FileName), '''');
  WriteLn(ScriptFile, 'set xtics');
  Write(ScriptFile, 'plot ');
  for i := 0 to FSeries.Count - 1 do
    WriteLn(ScriptFile, Format(plotlinetemplate, [DataFilePath, i+3, FSeries.FindSerie(i).Name]),
      IfThen(i < Pred(FSeries.Count), ',\'));
  Close(ScriptFile);
  //execute gnuplot
  with TProcess.Create(nil) do
  try
    Options := [poWaitOnExit];
    CommandLine := FGnuPlotExe + ' ' + ScriptFilePath;
    Execute;
  finally
    Destroy;
  end;
  Result := FileExists(ExpandFileName(FileName));
end;

end.

