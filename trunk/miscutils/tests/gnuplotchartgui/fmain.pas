unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GnuPlotChart, IniFiles, StdCtrls, EditBtn;

type

  { TMainForm }

  TMainForm = class(TForm)
    BuildChartButton: TButton;
    ChartImage: TImage;
    GnuPlotExecutableEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    SelectStyleCombo: TComboBox;
    procedure BuildChartButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GnuPlotExecutableEditAcceptFileName(Sender: TObject;
      var Value: String);
  private
    { private declarations }
    FChart: TGnuPlotChart;
    function CheckExecutable(const Value: String): Boolean;
    procedure PopulateChart;
    function GetChartStyle: TGnuPlotChartStyle;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

const
  OutputFiles: array[TGnuPlotChartStyle] of String = ('linechart.png', 'areachart.png', 'areastackedchart.png',
    'barchart.png', 'barstackedchat.png');

{ TMainForm }

procedure TMainForm.BuildChartButtonClick(Sender: TObject);
begin
  with FChart do
  begin
    Style := GetChartStyle;
    SaveToFile(OutputFiles[Style]);
    Sleep(10);
    ChartImage.Picture.LoadFromFile(OutputFiles[Style]);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FChart := TGnuPlotChart.Create;
  with TIniFile.Create('gnuplot.ini') do
  begin
    GnuPlotExecutableEdit.FileName := ReadString('gnuplot', 'path', '');
    FChart.GnuPlotExe := GnuPlotExecutableEdit.FileName;
    CheckExecutable(FChart.GnuPlotExe);
    Destroy;
  end;
  BuildChartButton.Enabled := FileExists(FChart.GnuPlotExe);
  PopulateChart;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FChart.Destroy;
end;

procedure TMainForm.GnuPlotExecutableEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if not CheckExecutable(Value) then
  begin
    ShowMessage('File "' + FChart.GnuPlotExe + '" not found'#13#10
      +' Setup the gnuplot path in ini file');
    Exit;
  end;
  FChart.GnuPlotExe := Value;
  with TIniFile.Create('gnuplot.ini') do
  begin
    WriteString('gnuplot', 'path', Value);
    Destroy;
  end;
end;

procedure TMainForm.PopulateChart;
begin
  with FChart do
  begin
    XAxisLabels.Add('Cat1');
    XAxisLabels.Add('Cat2');
    XAxisLabels.Add('Cat3');

    with Series['Serie1'] do
    begin
      Add(1.5);
      Add(6.0);
      Add(4.0);
    end;

    with Series['Serie2'] do
    begin
      Add(4.0);
      Add(5.0);
      Add(3.5)
    end;
  end;
end;

function TMainForm.CheckExecutable(const Value: String): Boolean;
begin
  Result := FileExists(Value);
  BuildChartButton.Enabled := Result;
end;

function TMainForm.GetChartStyle: TGnuPlotChartStyle;
begin
  case SelectStyleCombo.ItemIndex of
    0: Result := gpsLine;
    1: Result := gpsArea;
    2: Result := gpsAreaStacked;
    3: Result := gpsBar;
    4: Result := gpsBarStacked;
  end;
end;

initialization
  {$I fmain.lrs}

end.

