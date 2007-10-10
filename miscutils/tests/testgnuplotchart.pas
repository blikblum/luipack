program testgnuplotchart;

{$Mode ObjFpc}
{$H+}

{ $define DEBUGHEAP}
uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
  sysutils,classes, gnuplotchart;
var	
  Chart: TGnuPlotChart;

begin //Main 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.log');
  {$endif}
  Chart := TGnuPlotChart.Create;
  with Chart do
  begin
    GnuPlotExe := 'C:\bin\utils\gnuplot\bin\pgnuplot.exe';
    with Series['serie1'] do
    begin
      Add(2.3);
      Add(5);
      Add(10);
    end;   
    with Series['serie2'] do
    begin
      Add(7);
      Add(6.8);
      Add(9);
    end;
    XAxisLabels.Add('cat1');
    XAxisLabels.Add('cat2');
    XAxisLabels.Add('cat3');
    SaveToFile('test.png');
    Destroy;
  end;
end.