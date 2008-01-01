unit GnuPlotChart;

{
  Generates charts using gnuplot utility

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TGnuPlotChartStyle = (gpsLine, gpsArea, gpsAreaStacked, gpsBar, gpsBarStacked);

  TGnuPlotChart = class;

  TDataSerieList = class;
  
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
  
  TDataSerieRange = record
    Max: Double;
    Min: Double;
  end;

  TDataSerieList = class
  private
    FList: TFpList;
    FStackMode: Boolean;
    FOwner: TGnuPlotChart;
    function GetCount: Integer; inline;
    function GetItems(const SerieName: String): TDataSerie;
    function FindSerie(Index: Integer): TDataSerie; inline;
    function FindSerie(const SerieName: String): TDataSerie;
    function GetYRange: TDataSerieRange;
  public
    constructor Create(AOwner: TGnuPlotChart);
    destructor Destroy; override;
    procedure Clear;
    function GetValue(SerieIndex, XIndex: Integer): Double;
    function NewSerie(const SerieName: String): TDataSerie;
    property Items[SerieName: String]: TDataSerie read GetItems; default;
    property Count: Integer read GetCount;
    property StackMode: Boolean read FStackMode write FStackMode;
    property YRange: TDataSerieRange read GetYRange;
  end;
  
  { TGnuPlotChart }

  TGnuPlotChart = class
  private
    FGnuPlotExe: String;
    FSeries: TDataSerieList;
    FStyle: TGnuPlotChartStyle;
    FXAxisLabels: TStrings;
    FXAxisTitle: String;
    FYAxisTitle: String;
    procedure BuildDataFile(const DataFilePath: String);
    procedure SetStyle(const AValue: TGnuPlotChartStyle);
    procedure WritePlotCommand(var Script: Text);
    procedure WriteStyleProperties(var Script: Text);
    procedure WriteYRange(var Script: Text; Range: TDataSerieRange);
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToFile(const FileName: String): Boolean;
    property GnuPlotExe: String read FGnuPlotExe write FGnuPlotExe;
    property Series: TDataSerieList read FSeries;
    property Style: TGnuPlotChartStyle read FStyle write SetStyle;
    property XAxisLabels: TStrings read FXAxisLabels;
    property XAxisTitle: String read FXAxisTitle write FXAxisTitle;
    property YAxisTitle: String read FYAxisTitle write FYAxisTitle;
  end;
  

implementation

uses
  strutils, process, math;

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

function TDataSerieList.GetYRange: TDataSerieRange;
var
  i,j: Integer;
  AValue: Double;
begin
  Result.Min := 0;
  Result.Max := 0;
  for i := 0 to FList.Count -1 do
  begin
    for j := 0 to FOwner.XAxisLabels.Count - 1 do
    begin
      AValue := GetValue(i, j);
      if AValue < Result.Min then
        Result.Min := AValue
      else
        if AValue > Result.Max then
          Result.Max := AValue;
    end
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

constructor TDataSerieList.Create(AOwner: TGnuPlotChart);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
end;

destructor TDataSerieList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TDataSerieList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TDataSerie(FList[i]).Destroy;
  FList.Clear;
end;

function TDataSerieList.GetValue(SerieIndex, XIndex: Integer): Double;
begin
  if (not FStackMode) or (SerieIndex = 0) then
    Result := FindSerie(SerieIndex).FYValues[XIndex]
  else
  begin
    Result := 0;
    while SerieIndex >= 0 do
    begin
      Result := Result + FindSerie(SerieIndex).FYValues[XIndex];
      Dec(SerieIndex);
    end;
  end;
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

procedure TGnuPlotChart.WriteStyleProperties(var Script: Text);
begin
  case FStyle of
    gpsLine:
      WriteLn(Script, 'set style data lines');
    gpsBar:
      begin
        WriteLn(Script, 'set style data histogram');
        WriteLn(Script, 'set style fill solid border -1');
      end;
    gpsArea, gpsAreaStacked:
      begin
        WriteLn(Script, 'set style data filledcurves x1');
        WriteLn(Script, 'set style fill solid border -1');
      end;
  end;
end;

procedure TGnuPlotChart.WriteYRange(var Script: Text; Range: TDataSerieRange);
begin
  Range.Max := Trunc(Range.Max + (Range.Max * 0.2));
  Range.Min := Max(0, Trunc(Range.Min - (Range.Min * 0.2)));
  WriteLn(Script, 'set yrange [', FloatToStr(Range.Min), ':',FloatToStr(Range.Max),']');
end;

procedure TGnuPlotChart.SetStyle(const AValue: TGnuPlotChartStyle);
begin
  FStyle := AValue;
  FSeries.StackMode := AValue in [gpsAreaStacked, gpsBarStacked];
end;

procedure TGnuPlotChart.WritePlotCommand(var Script: Text);
const
  PlotLineTemplate = '''gnuplotchart.data'' using 2:%d:xticlabels(1) title "%s"';
  PlotBarAndAreaTemplate =  '''gnuplotchart.data'' using %d:xticlabels(1) title "%s"';
  PlotAreaStackedTemplate = '''gnuplotchart.data'' using 2:%d:%d:xticlabels(1) title "%s"';
var
  Template: String;
  i: Integer;
begin
  Write(Script, 'plot ');
  case FStyle of
    gpsLine, gpsBar, gpsArea:
      begin
        if FStyle = gpsLine then
          Template := PlotLineTemplate
        else
          Template := PlotBarAndAreaTemplate;
        for i := 0 to FSeries.Count - 1 do
          WriteLn(Script, Format(Template, [i+3, FSeries.FindSerie(i).Name]),
            IfThen(i < Pred(FSeries.Count), ',\'));
      end;
    gpsAreaStacked:
      begin
        Template := PlotAreaStackedTemplate;
        WriteLn(Script, '''gnuplotchart.data'' using 3:xticlabels(1) with filledcurves x1 title "',
        FSeries.FindSerie(0).Name,'"',IfThen(FSeries.Count > 0, ',\'));
        for i := 1 to FSeries.Count - 1 do
          WriteLn(Script, Format(Template, [i+2, i+3, FSeries.FindSerie(i).Name]),
            IfThen(i < Pred(FSeries.Count), ',\'));
      end;
  end;
end;

procedure TGnuPlotChart.BuildDataFile(const DataFilePath: String);
var
  j: Integer;
  i: Integer;
  DataFile: Text;
begin
  Assign(DataFile, DataFilePath);
  Rewrite(DataFile);
  for j := 0 to FXAxisLabels.Count - 1 do
  begin
    Write(DataFile, '"', FXAxisLabels[j], '"');
    Write(DataFile, ' ', j);
    for i := 0 to FSeries.Count - 1 do
      Write(DataFile, ' ', FloatToStr(FSeries.GetValue(i,j)));
    WriteLn(DataFile);
  end;
  System.Close(DataFile);
end;

constructor TGnuPlotChart.Create;
begin
  FSeries := TDataSerieList.Create(Self);
  FXAxisLabels := TStringList.Create;
end;

destructor TGnuPlotChart.Destroy;
begin
  FSeries.Destroy;
  FXAxisLabels.Destroy;
  inherited Destroy;
end;

function TGnuPlotChart.SaveToFile(const FileName: String): Boolean;
var
  ScriptFile: Text;
  ScriptFilePath, OldDir: String;
begin
  if not FileExists(FGnuPlotExe) then
    Exit(False);
  if FSeries.Count = 0 then
    Exit(False);
  if FileExists(FileName) then
    DeleteFile(FileName);
  //build data file
  BuildDataFile(GetTempDir + 'gnuplotchart.data');
  //build script file
  ScriptFilePath := GetTempDir + 'gnuplotchart.plot';
  Assign(ScriptFile, ScriptFilePath);
  Rewrite(ScriptFile);
  WriteLn(ScriptFile, 'set terminal png');
  WriteLn(ScriptFile, 'set output ''', ExpandFileName(FileName), '''');
  WriteYRange(ScriptFile, FSeries.YRange);
  WriteLn(ScriptFile, 'set xtics');
  WriteStyleProperties(ScriptFile);
  WritePlotCommand(ScriptFile);
  Close(ScriptFile);
  //execute gnuplot
  OldDir := GetCurrentDir;
  SetCurrentDir(GetTempDir);
  with TProcess.Create(nil) do
  try
    Options := [poWaitOnExit, poNoConsole];
    CommandLine := FGnuPlotExe + ' ' + ScriptFilePath;
    Execute;
  finally
    SetCurrentDir(OldDir);
    Destroy;
  end;
  Result := FileExists(ExpandFileName(FileName));
end;

initialization
  DecimalSeparator := '.';

end.

