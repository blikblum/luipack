unit fmain;

{ Copyright (C) 2005 Luiz Américo Pereira Câmara

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs, Menus, DB, inifiles, ExtCtrls, StdCtrls, DBGrids, Grids, CheckLst, Buttons;

type
  { TfrmMain }
  //todo(partial): isolate GUI code from logic
  TfrmMain = class(TForm)
    butShowView: TButton;
    butSaveView: TButton;
    butSaveToHtml: TButton;
    ButCreateChart: TButton;
    CheckRows: TCheckListBox;
    CheckDataSeries: TCheckListBox;
    CheckXAxis: TCheckListBox;
    CheckColumns: TCheckListBox;
    comboViews: TComboBox;
    comboScale: TComboBox;
    comboGroupBy: TComboBox;
    gridResults: TdbGrid;
    ImageChart: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    listSummary: TListBox;
    listResults: TListBox;
    MIOpenRecent: TMenuItem;
    nbMain: TNotebook;
    PageCharts: TPage;
    pageReports: TPage;
    pageSummary: TPage;
    pageCustomViews: TPage;
    pageResults: TPage;
    dlgSaveHtml: TSaveDialog;
    PanelTop: TPanel;
    MIOpen: TMenuItem;
    MIFile: TMenuItem;
    menuMain: TMainMenu;
    dlgOpenDatafile: TOpenDialog;
    gridCustomViews: TStringGrid;
    gridSummary: TStringGrid;
    //Events
    procedure ButCreateChartClick(Sender: TObject);
    procedure butSaveToHtmlClick(Sender: TObject);
    procedure butSaveViewClick(Sender: TObject);
    procedure comboViewsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure butShowViewClick(Sender: TObject);
    procedure comboScaleChange(Sender: TObject);
    procedure comboMainChange(Sender: TObject);
    procedure listSummarySelectionChange(Sender: TObject; User: boolean);
    procedure listResultsSelectionChange(Sender: TObject; User: boolean);
    procedure MIOpenClick(Sender: TObject);
    procedure MIOpenRecentClick(Sender: TObject);
  private
    RecentFileList:TStrings;
    procedure LoadRecentMenu;
    function GetChecked(CheckBox:TCheckListBox;List:TStrings):Boolean;
    procedure SetChecked(CheckBox:TCheckListBox;List:TStrings);
    procedure OpenFile(const AFileName: String; UpdateRecentList: Boolean);
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  fsaveview, DomUtils, dmodule, GnuPlotChart;

{ TfrmMain }

procedure TfrmMain.MIOpenClick(Sender: TObject);
begin
  //todo: Set InitialDir and check database
  with dlgOpenDatafile do
    if Execute then
    begin
      OpenFile(FileName,True);
    end;
end;

procedure TfrmMain.MIOpenRecentClick(Sender: TObject);
begin
  OpenFile(TMenuItem(Sender).Caption,True);
end;

procedure TfrmMain.listResultsSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  if listSummary.ItemIndex = -1 then
    Exit;
  with dmMain.ChronoData do
  begin
    ActiveGroup := listResults.Items[listResults.ItemIndex];
    RefreshDataset;
  end;
  with gridResults do
  begin
    for i:= 0 to Columns.Count - 1 do
      Columns[i].Width := 100;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  ini:TIniFile;
  i:Integer;
begin
  ini:=TIniFile.Create('chronoview.ini');
  with ini do
  begin
    EraseSection('recentfiles');
    for i:= 0 to RecentFileList.Count - 1 do
      WriteString('recentfiles',RecentFileList[i],IntToStr(i));
    Destroy;
  end;
  RecentFileList.Destroy;
end;


procedure TfrmMain.listSummarySelectionChange(Sender: TObject; User: boolean);
var
  i:Integer;
begin
  if listSummary.ItemIndex = -1 then
    Exit;
  with dmMain.ChronoData, gridSummary do
  begin
    ColCount := 6 ;
    //todo:  Columns are not set at runtime. Do it at design?
    Cells[1,0] := 'Count';
    Cells[2,0] := 'Min';
    Cells[3,0] := 'Max';
    Cells[4,0] := 'Median';
    Cells[5,0] := 'Average';

    ActiveGroup := listSummary.Items[listSummary.ItemIndex];
    RowCount := RowList.Count + 1;
    for i := 1 to RowList.Count do
    begin
      Cells[0,i] := RowList[i-1];
      Cells[1,i] := GetCount(i-1);
      Cells[2,i] := GetMin(i-1);
      Cells[3,i] := GetMax(i-1);
      Cells[4,i] := GetMedian(i-1);
      Cells[5,i] := GetAvg(i-1);
    end;
  end;
end;

procedure TfrmMain.butShowViewClick(Sender: TObject);
var
  i,j:Integer;
  RowList,ColList:TStringList;

  function GetTotal(RowNo:Integer):String;
  var
    k,x:Integer;
  begin
    x := 0;
    for k := 1 to ColList.Count do
      x := x+StrToInt(gridCustomViews.Cells[k,RowNo]);
    Result := IntToStr(x);
  end;
  
begin
  RowList := TStringList.Create;
  ColList := TStringList.Create;

  GetChecked(CheckRows, RowList);
  GetChecked(CheckColumns, ColList);

  with gridCustomViews do
  begin
    if (ColList.Count > 0) and (RowList.Count > 0) then
    begin
      ColCount := ColList.Count + 2;
      Cells[ColCount-1,0] := 'Total';
      for i := 1 to ColList.Count do
        Cells[i,0] := ColList[i - 1];

      RowCount := RowList.Count+1;
      for i:=1 to RowList.Count do
      begin
        Cells[0,i]:=RowList[i-1];
        for j:= 0 to ColList.Count - 1 do
        begin
          Cells[j+1,i] := dmMain.ChronoData.GetAvg(ColList[j], RowList[i-1]);
        end;
        Cells[ColList.Count+1,i] := GetTotal(i);
      end;
    end
    else
    begin
      ColCount := 1;
      RowCount := 2;
      Cells[0,1] := '';
    end;
  end;
  RowList.Destroy;
  ColList.Destroy;
end;

function TfrmMain.GetChecked(CheckBox:TCheckListBox;List: TStrings):Boolean;
var
  i:Integer;
begin
  with CheckBox do
    for i:= 0 to Items.Count - 1 do
    begin
      if Checked[i] then
        List.Add(Items[i]);
    end;
  Result:=List.Count > 0;
end;

procedure TfrmMain.SetChecked(CheckBox: TCheckListBox; List: TStrings);
var
  i:Integer;
begin
  with CheckBox do
  for i:= 0 to Items.Count - 1 do
    Checked[i]:= List.IndexOf(Items[i]) <> -1;
end;

procedure TfrmMain.LoadRecentMenu;
var
  i:Integer;
  TempItem: TMenuItem;
begin
  MIOpenRecent.Enabled:=True;
  MIOpenRecent.Clear;
  for i:= 0 to RecentFileList.Count - 1 do
  begin
    TempItem:=TMenuItem.Create(Self);
    TempItem.Caption:=RecentFileList[i];
    TempItem.OnClick:=@MIOpenRecentClick;
    MIOpenRecent.Add(TempItem);
  end;
end;

procedure TfrmMain.OpenFile(const AFileName: String; UpdateRecentList: Boolean);
var
  i:Integer;
begin
  if not FileExists(AFileName) then
  begin
    ShowMessage('File "'+AFileName+'" does not exists');
    Exit;
  end;
  with dmMain.dsResults do
  begin
    Close;
    FileName := AFileName;
  end;
  with dmMain.dsCustomViews do
  begin
    Close;
    FileName := AFileName;
    if not TableExists then
    begin
      FieldDefs.Clear;
      FieldDefs.Add('Code',ftAutoInc);
      FieldDefs.Add('Name',ftString);
      FieldDefs.Add('Description',ftString);
      FieldDefs.Add('ActionSession',ftString);
      CreateTable;
    end;
    comboViews.Items.Clear;
    //avoid exception when the user has no write access to db file
    if TableExists then
      QuickQuery('Select Name,Code from chrono_custom_views',
        comboViews.Items,True);
  end;
  comboGroupBy.Enabled := True;
  comboGroupBy.ItemIndex := 0;
  comboMainChange(nil);
  comboScale.Enabled := True;
  comboScale.ItemIndex := 1; //Miliseconds
  comboScaleChange(nil);
  Caption:='ChronoView - '+ AFileName;
  if UpdateRecentList then
  begin
    i := RecentFileList.IndexOf(AFileName);
    if i <> -1 then
      RecentFileList.Delete(i);
    RecentFileList.Insert(0,AFilename);
    if RecentFileList.Count > 10 then
      RecentFileList.Delete(RecentFileList.Count - 1);
    LoadRecentMenu;
  end;
end;

procedure TfrmMain.comboMainChange(Sender: TObject);
begin
  with dmMain.ChronoData do
  begin
    GroupIndex := comboGroupBy.ItemIndex;
    listResults.Items.Assign(GroupList);
    listSummary.Items.Assign(GroupList);
    
    CheckRows.Items.Assign(GroupList);
    SetChecked(CheckRows, GroupList);
    CheckColumns.Items.Assign(RowList);
    SetChecked(CheckColumns, RowList);
    
    CheckXAxis.Items.Assign(RowList);
    SetChecked(CheckXAxis, RowList);
    CheckDataSeries.Items.Assign(GroupList);
    SetChecked(CheckDataSeries, GroupList);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini:TIniFile;
begin
  gridResults.DataSource := dmMain.srcResults;
  RecentFileList := TStringList.Create;
  ini := TIniFile.Create('chronoview.ini');
  with ini do
  begin
    ReadSection('recentfiles',RecentFileList);
    if RecentFileList.Count > 0 then
    begin
      LoadRecentMenu;
      OpenFile(RecentFileList[0],False);
    end
    else
      MIOpenRecent.Enabled:=False;
    Destroy;
  end;
end;

procedure TfrmMain.butSaveViewClick(Sender: TObject);
var
  ActionList,SessionList:TStrings;
begin
  ActionList:=TStringList.Create;
  ActionList.Delimiter:=';';
  SessionList:=TStringList.Create;
  SessionList.Delimiter:=';';
  if GetChecked(CheckRows,ActionList) and
    GetChecked(CheckColumns,SessionList) then
  begin
    with TfrmSaveView.Create(nil) do
    try
      if ShowModal = mrOk then
      with dmMain do
      begin
        dsCustomViews.Open;
        dsCustomViews.Append;
        dsCustomViews.FieldByName('Name').AsString:=ViewName;
        dsCustomViews.FieldByName('Description').AsString:=Description;
        dsCustomViews.FieldByName('ActionSession').AsString:=
          ActionList.DelimitedText+'='+SessionList.DelimitedText;
        dsCustomViews.Post;
      end;
    finally
      Destroy;
    end;
  end
  else
    ShowMessage('It''s necessary to check at least one Session and one Action');
  ActionList.Destroy;
  SessionList.Destroy;
end;

procedure TfrmMain.butSaveToHtmlClick(Sender: TObject);
var
  ATree:THtmlTree;
  ATable:TTableTree;
  ADbTable:TDBTree;
  AGroupList,ARowList:TStrings;
  i,j,PosEqual,Acumulator: Integer;
  ActionSessionStr,AAvgStr:String;
begin
  if not dlgSaveHtml.Execute then
    Exit;
  ATree:=THtmlTree.Create;
  ATable:=TTableTree.Create(Atree);
  ADbTable:=TDBTree.Create(ATree);
  AGroupList:=TStringList.Create;
  ARowList:=TStringList.Create;

  AGroupList.Assign(dmMain.ChronoData.GroupList);
  ARowList.Assign(dmMain.ChronoData.RowList);

  with ATree do
  begin
    Title:='Chrono View Results';
    AddCssLink('chrono.css');
    AddJsLink('chrono.js');
    with SubTree do
    begin
      //Add Menu Items
      AddBase('div');
      Last['class']:='menu_views';
      AddListItem('All Results');
      Last['onclick']:='show_allresults()';
      AddListItem('Summary');
      Last['onclick']:='show_summary()';
      AddListItem('Custom Views');
      Last['onclick']:='show_customviews()';
      Attach;
      //Add Raw Results
      Add('div');
      AddBookMark(Last,'div_results');
      Last['id']:='allresults';
      Last['class']:='content';
      Attach;
    end;
    with ADbTable do
    begin
      Dataset := dmMain.ChronoData.Dataset;
      for i := 0 to AGroupList.Count - 1 do
      begin
        dmMain.ChronoData.ActiveGroup :=  AGroupList[i];
        dmMain.ChronoData.RefreshDataset;
        Title := AGroupList[i];
        BuildTable;
        AttachTo(GetBookmark('div_results'));
      end;
    end;

    //add Summary
    SubTree.Add('div');
    AddBookMark(SubTree.Last,'div_summary');
    SubTree.Last['id']:='summary';
    SubTree.Last['class']:='content';
    SubTree.Attach;
    with ATable, dmMain.ChronoData do
    begin
      for i:=0 to AGroupList.Count - 1 do
      begin
        ActiveGroup := AGroupList[i];
        //Add Table Title
        AddRow;
        Last['class']:='tabletitle';
        AddCol(AGroupList[i]);
        Last['colspan']:='6';

        //Add column titles
        AddRow;
        Last['class']:='columntitle';
        AddCol(' ');
        AddCol('Count');
        AddCol('Min');
        AddCol('Max');
        AddCol('Median');
        AddCol('Average');

        for j:=0 to ARowList.Count -1 do
        begin
          AddRow;
          AddCol(ARowList[j]);
          AddCol(GetCount(j));
          AddCol(GetMin(j));
          AddCol(GetMax(j));
          AddCol(GetMedian(j));
          AddCol(GetAvg(j));
        end;

        AttachTo(GetBookmark('div_summary'));
      end;
    end;

    //add Views

    SubTree.Add('div');
    AddBookMark(SubTree.Last,'div_customviews');
    SubTree.Last['id']:='customviews';
    SubTree.Last['class']:='content';
    SubTree.Attach;
    AGroupList.Delimiter:=';';
    ARowList.Delimiter:=';';
    with dmMain.dsCustomViews do
    begin
      Open;
      while not Eof do
      begin
        ActionSessionStr:=FieldByName('ActionSession').AsString;
        PosEqual:=Pos('=',ActionSessionStr);
        if comboGroupBy.ItemIndex = 0 then
        begin
          AGroupList.DelimitedText:=Copy(ActionSessionStr,1,PosEqual-1);
          ARowList.DelimitedText:=Copy(ActionSessionStr,PosEqual+1,length(ActionSessionStr));
        end
        else
        begin
          ARowList.DelimitedText:=Copy(ActionSessionStr,1,PosEqual-1);
          AGroupList.DelimitedText:=Copy(ActionSessionStr,PosEqual+1,length(ActionSessionStr));
        end;
        //Add Tabletitle
        ATable.AddRow;
        ATable.Last['class']:='tabletitle';
        ATable.AddCol(FieldByName('Description').AsString);
        ATable.Last['colspan']:=IntToStr(AGroupList.Count+2);

        //Add Columntitle
        ATable.AddRow;
        ATable.Last['class']:='columntitle';
        ATable.AddCol(' ');
        for i:=0 to AGroupList.Count - 1 do
          ATable.AddCol(AGroupList[i]);
        ATable.AddCol('Total');
        
        for i:=0 to ARowList.Count - 1 do
        begin
          ATable.AddRow;
          ATable.AddCol(ARowList[i]);
          Acumulator:=0;
          for j:= 0 to AGroupList.Count - 1 do
          begin
            AAvgStr := dmMain.ChronoData.GetAvg(AGroupList[j],ARowList[i]);
            Acumulator := Acumulator + StrToInt(AAvgStr);
            ATable.AddCol(AAvgStr);
          end;
          ATable.AddCol(IntToStr(Acumulator));
        end;
        ATable.AttachTo(ATree.GetBookmark('div_customviews'));
        Next;
      end;
      Close;
    end;

    SaveToFile(dlgSaveHtml.FileName);
    Destroy;
  end;
  ATable.Destroy;
  ADbTable.Destroy;
  AGroupList.Destroy;
  ARowList.Destroy;
end;

procedure TfrmMain.ButCreateChartClick(Sender: TObject);
var
  Chart: TGnuPlotChart;
  DataSeries: TStrings;
  i, j: Integer;
  ini: TIniFile;
begin
  DataSeries := TStringList.Create;
  Chart := TGnuPlotChart.Create;
  with Chart do
  begin
    ini := TIniFile.Create('chronoview.ini');
    GnuPlotExe := ini.ReadString('gnuplot','path','');
    ini.Destroy;
    GetChecked(CheckDataSeries, DataSeries);
    GetChecked(CheckXAxis, XAxisLabels);

    if (XAxisLabels.Count > 0) and (DataSeries.Count > 0) then
    begin
      for i := 0 to DataSeries.Count - 1 do
      begin
        with Series[DataSeries[i]] do
        begin
          for j := 0 to XAxisLabels.Count - 1 do
            Add(StrToFloat(dmMain.ChronoData.GetMedian(XAxisLabels[j], DataSeries[i])));
        end;
      end;
      if SaveToFile(GetTempDir + 'chronoviewchart.png') then
        ImageChart.Picture.LoadFromFile(GetTempDir + 'chronoviewchart.png')
      else
        ShowMessage('Error creating chart');
    end;
    Destroy;
  end;
  DataSeries.Destroy;
end;

procedure TfrmMain.comboViewsChange(Sender: TObject);
var
  ActionSessionStr: String;
  PosEqual: Integer;
  ActionList,SessionList: TStrings;
begin
  ActionList := TStringList.Create;
  ActionList.Delimiter := ';';
  SessionList := TStringList.Create;
  SessionList.Delimiter := ';';
  with dmMain do
  begin
    dsCustomViews.Open;
    with comboViews do
    if dsCustomViews.Locate('Code',Integer(Items.Objects[ItemIndex]),[]) then
    begin
      ActionSessionStr := dsCustomViews.FieldByName('ActionSession').AsString;
      PosEqual := Pos('=',ActionSessionStr);
      ActionList.DelimitedText := Copy(ActionSessionStr,1,PosEqual-1);
      SessionList.DelimitedText := Copy(ActionSessionStr,PosEqual+1,length(ActionSessionStr));
      SetChecked(CheckRows, ActionList);
      SetChecked(CheckColumns, SessionList);
      butShowViewClick(nil);
    end
    else
      ShowMessage('Error locating view');
    dsCustomViews.Close;
  end;
end;

procedure TfrmMain.comboScaleChange(Sender: TObject);
begin
  dmMain.ChronoData.ScaleIndex := comboScale.ItemIndex;
end;

initialization
  {$I fmain.lrs}

end.

