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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus, DB, inifiles, ExtCtrls, StdCtrls, DBGrids, Grids, CheckLst, Buttons,
  TAGraph, sqlite3ds;

type
  { TfrmMain }
  //todo: isolate GUI code from logic
  TfrmMain = class(TForm)
    butShowView: TButton;
    butSaveView: TButton;
    butSaveToHtml: TButton;
    Chart1: TChart;
    checkActions: TCheckListBox;
    checkSessions: TCheckListBox;
    comboViews: TComboBox;
    comboScale: TComboBox;
    comboGroupBy: TComboBox;
    gridResults: TdbGrid;
    Label1: TLabel;
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
    dsResults: TSqlite3Dataset;
    dsCustomViews: TSqlite3Dataset;
    PanelTop: TPanel;
    srcResults: TDatasource;
    MIOpen: TMenuItem;
    MIFile: TMenuItem;
    menuMain: TMainMenu;
    dlgOpenDatafile: TOpenDialog;
    gridCustomViews: TStringGrid;
    gridSummary: TStringGrid;
    //Events
    procedure butSaveToHtmlClick(Sender: TObject);
    procedure butSaveViewClick(Sender: TObject);
    procedure comboViewsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure butShowViewClick(Sender: TObject);
    procedure comboScaleChange(Sender: TObject);
    procedure comboMainChange(Sender: TObject);
    procedure listDetails2SelectionChange(Sender: TObject; User: boolean);
    procedure listMainSelectionChange(Sender: TObject; User: boolean);
    procedure MIOpenClick(Sender: TObject);
    procedure MIOpenRecentClick(Sender: TObject);
  private
    RecentFileList:TStrings;
    FGroupByFieldsStr: String;
    FGroupByWhereStr : String;
    FGroupByStr: String;
    FWhereStr: String;
    FScaleStr: String;
    FAverageStr:String;
    FMinStr:String;
    FMaxStr:String;
    FCountStr:String;
    procedure UpdateSqlTemplates;
    procedure LoadRecentMenu;
    procedure LoadSessions(List: TStrings);
    procedure LoadActions(List: TStrings);
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
  fsaveview, DomUtils;

const
  FAverageTemplate = 'Select ifnull(Cast (Round(Avg(Time)/%s) as Integer),0)';
  FMinTemplate = 'Select Cast(Round(Min(Time)/%s) as Integer)';
  FMaxTemplate = 'Select Cast(Round(Max(Time)/%s) as Integer)';
  FSessionFieldsTemplate = 'Select Action , Cast (Round(Time/%s) as Integer), Date, Comments from Results, Sessions';
  FSessionWhereTemplate = ' Where Sessions.Name = "%s" and Sessions.Code = Results.SessionId Order by Time;';
  FActionFieldsTemplate = 'Select Name , Cast(Round(Time/%s) as Integer), Date, Comments from Results, Sessions';
  FActionWhereTemplate = ' Where Results.Action = "%s" and Results.SessionId = Sessions.Code Order by Time;';
  FWhereTemplate = ' from results , sessions where  %s = "%s" and sessionid = sessions.code and %s = "%s";';

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

procedure TfrmMain.LoadSessions(List: TStrings);
begin
  List.Clear;
  dsResults.QuickQuery('Select Distinct Name from Sessions;',List);
end;

procedure TfrmMain.LoadActions(List: TStrings);
begin
  List.Clear;
  dsResults.QuickQuery('Select Distinct Action from Results;',List);
end;

procedure TfrmMain.listMainSelectionChange(Sender: TObject; User: boolean);
var
  i:Integer;
begin
  with dsResults do
  begin
    Sql:=Format(FGroupByStr,[listResults.Items[listResults.ItemIndex]]);
    //Writeln('SQL: ',sql);
    Close;
    Open;
  end;
  with gridResults do
  begin
    for i:= 0 to Columns.Count - 1 do
      Columns[i].Width:=100;
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

procedure TfrmMain.UpdateSqlTemplates;
begin
  FMinStr:=Format(FMinTemplate,[FScaleStr])+FWhereStr;
  FMaxStr:=Format(FMaxTemplate,[FScaleStr])+FWhereStr;
  FAverageStr:=Format(FAverageTemplate,[FScaleStr])+FWhereStr;
  FCountStr:='Select Count(*)'+FWhereStr;
  FGroupByStr:=Format(FGroupByFieldsStr,[FScaleStr])+FGroupByWhereStr;
  //todo: Update queries here
  {
  writeln('FMinStr: ',FMinStr);
  writeln('FMaxStr: ',FMaxStr);
  writeln('FAverageStr: ',FAverageStr);
  writeln('FGroupByStr: ',FGroupByStr);
  }
end;

procedure TfrmMain.listDetails2SelectionChange(Sender: TObject; User: boolean);
var
  RowList:TStrings;
  i:Integer;
begin
  RowList:=TStringList.Create;
  case comboGroupBy.ItemIndex of
  0:
  begin
    LoadActions(RowList);
  end;
  1:
  begin
    LoadSessions(RowList);
  end;
  end;
  
  with gridSummary do
  begin
    ColCount:=5 ;
    //todo:  Columns are not set at runtime. Do it at design?
    Cells[1,0]:='Count';
    Cells[2,0]:='Min';
    Cells[3,0]:='Max';
    Cells[4,0]:='Average';

    RowCount:=RowList.Count + 1;
    for i:=1 to RowList.Count do
    begin
      Cells[0,i]:=RowList[i-1];
      //writeln('Sql: ',Format(FMinStr,[RowList[i-1],
      // listSummary.Items[listSummary.ItemIndex]]));
      //writeln('Result: ',dsResults.QuickQuery(Format(FMinStr,[RowList[i-1],
      // listSummary.Items[listSummary.ItemIndex] ])));
      Cells[1,i]:=dsResults.QuickQuery(Format(FCountStr,[RowList[i-1],
       listSummary.Items[listSummary.ItemIndex] ]));
      Cells[2,i]:=dsResults.QuickQuery(Format(FMinStr,[RowList[i-1],
       listSummary.Items[listSummary.ItemIndex] ]));
      Cells[3,i]:=dsResults.QuickQuery(Format(FMaxStr,[RowList[i-1],
       listSummary.Items[listSummary.ItemIndex] ]));
      Cells[4,i]:=dsResults.QuickQuery(Format(FAverageStr,[RowList[i-1],
       listSummary.Items[listSummary.ItemIndex] ]));
    end;
  end;
  RowList.Destroy;
end;

procedure TfrmMain.butShowViewClick(Sender: TObject);
var
  i,j:Integer;
  RowList,ColList:TStringList;

  function GetTotal(RowNo:Integer):String;
  var
    k,x:Integer;
  begin
    x:=0;
    for k:=1 to ColList.Count do
      x:=x+StrToInt(gridCustomViews.Cells[k,RowNo]);
    Result:=IntToStr(x);
  end;
  
begin
  RowList:=TStringList.Create;
  ColList:=TStringList.Create;

  case comboGroupBy.ItemIndex of
  0:
  begin
    GetChecked(checkActions,ColList);
    GetChecked(checkSessions,RowList);
  end;
  1:
  begin
    GetChecked(checkActions,RowList);
    GetChecked(checkSessions,ColList);
  end;
  end;

  with gridCustomViews do
  begin
    if (ColList.Count > 0) and (RowList.Count > 0) then
    begin
      ColCount:=ColList.Count+2;
      Cells[ColCount-1,0]:='Total';
      for i:=1 to ColList.Count do
        Cells[i,0]:=ColList[i - 1];

      RowCount:=RowList.Count+1;
      for i:=1 to RowList.Count do
      begin
        Cells[0,i]:=RowList[i-1];
        for j:= 0 to ColList.Count - 1 do
        begin
          //writeln('SQL: ',Format(FAverageStr,[ColList[j],RowList[i-1]]));
          Cells[j+1,i]:=dsResults.QuickQuery(Format(FAverageStr,[ColList[j],RowList[i-1]]));
        end;
        Cells[ColList.Count+1,i]:=GetTotal(i);
      end;
    end
    else
    begin
      ColCount:=1;
      RowCount:=2;
      Cells[0,1]:='';
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
  with dsResults do
  begin
    Close;
    FileName:=AFileName;
  end;
  with dsCustomViews do
  begin
    Close;
    FileName:=AFileName;
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
    QuickQuery('Select Name,Code from chrono_custom_views',
      comboViews.Items,True);
  end;
  //todo: remove hack after LCL combo.ItemIndex bug is closed
  comboGroupBy.Enabled:=True;
  comboGroupBy.ItemIndex:=0;
  comboMainChange(nil);
  comboScale.Enabled:=True;
  comboScale.ItemIndex:=1; //Miliseconds
  comboScaleChange(nil);
  LoadSessions(checkSessions.Items);
  LoadActions(checkActions.Items);
  Caption:='ChronoView - '+ AFileName;
  if UpdateRecentList then
  begin
    i:=RecentFileList.IndexOf(AFileName);
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
  case comboGroupBy.ItemIndex of
  0:
  begin
    LoadSessions(listResults.Items);
    LoadSessions(listSummary.Items);
    FGroupByFieldsStr:=FSessionFieldsTemplate;
    FGroupByWhereStr:=FSessionWhereTemplate;
    FWhereStr:=Format(FWhereTemplate,['action','%s','name','%s']);
  end;
  1:
  begin
    LoadActions(listResults.Items);
    LoadActions(listSummary.Items);
    FGroupByFieldsStr:=FActionFieldsTemplate;
    FGroupByWhereStr:=FActionWhereTemplate;
    FWhereStr:=Format(FWhereTemplate,['name','%s','action','%s']);
  end;
  end;
  UpdateSqlTemplates;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini:TIniFile;
begin
  RecentFileList:=TStringList.Create;
  ini:=TIniFile.Create('chronoview.ini');
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
  ActionSessionStr:String;
begin
  ActionList:=TStringList.Create;
  ActionList.Delimiter:=';';
  SessionList:=TStringList.Create;
  SessionList.Delimiter:=';';
  if GetChecked(checkActions,ActionList) and
    GetChecked(checkSessions,SessionList) then
  begin
    with TfrmSaveView.Create(nil) do
    try
      if ShowModal = mrOk then
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
  if comboGroupBy.ItemIndex = 0 then
  begin
    LoadSessions(AGroupList);
    LoadActions(ARowList);
  end
  else
  begin
    LoadActions(AGroupList);
    LoadSessions(ARowList);
  end;
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
      Dataset:=dsResults;
      for i:=0 to AGroupList.Count - 1 do
      begin
        dsResults.Sql:=Format(FGroupByStr,[AGroupList[i]]);
        dsResults.Close;
        dsResults.Open;
        Title:=AGroupList[i];
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
    with ATable do
    begin
      for i:=0 to AGroupList.Count - 1 do
      begin
        //Add Table Title
        AddRow;
        Last['class']:='tabletitle';
        AddCol(AGroupList[i]);
        Last['colspan']:='5';

        //Add column titles
        AddRow;
        Last['class']:='columntitle';
        AddCol(' ');
        AddCol('Count');
        AddCol('Min');
        AddCol('Max');
        AddCol('Average');

        for j:=0 to ARowList.Count -1 do
        begin
          AddRow;
          AddCol(ARowList[j]);
          AddCol(dsResults.QuickQuery(Format(FCountStr,[ARowList[j],AGroupList[i]])));
          AddCol(dsResults.QuickQuery(Format(FMinStr,[ARowList[j],AGroupList[i]])));
          AddCol(dsResults.QuickQuery(Format(FMaxStr,[ARowList[j],AGroupList[i]])));
          AddCol(dsResults.QuickQuery(Format(FAverageStr,[ARowList[j],AGroupList[i]])));
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
    with dsCustomViews do
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
            AAvgStr:=dsResults.QuickQuery(Format(FAverageStr,[AGroupList[j],ARowList[i]]));
            Acumulator:=Acumulator+StrToInt(AAvgStr);
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

procedure TfrmMain.comboViewsChange(Sender: TObject);
var
  ActionSessionStr:String;
  PosEqual:Integer;
  ActionList,SessionList:TStrings;
begin
  ActionList:=TStringList.Create;
  ActionList.Delimiter:=';';
  SessionList:=TStringList.Create;
  SessionList.Delimiter:=';';
  dsCustomViews.Open;
  with comboViews do
  if dsCustomViews.Locate('Code',Integer(Items.Objects[ItemIndex]),[]) then
  begin
    ActionSessionStr:=dsCustomViews.FieldByName('ActionSession').AsString;
    PosEqual:=Pos('=',ActionSessionStr);
    ActionList.DelimitedText:=Copy(ActionSessionStr,1,PosEqual-1);
    SessionList.DelimitedText:=Copy(ActionSessionStr,PosEqual+1,length(ActionSessionStr));
    SetChecked(checkActions,ActionList);
    SetChecked(checkSessions,SessionList);
    butShowViewClick(nil);
  end
  else
    ShowMessage('Error locating view');
  dsCustomViews.Close;
end;

procedure TfrmMain.comboScaleChange(Sender: TObject);
begin
  case comboScale.ItemIndex of
    0:FScaleStr:='1';
    1:FScaleStr:='1000';
    2:FScaleStr:='1000000';
  end;
  UpdateSqlTemplates;
end;

initialization
  {$I fmain.lrs}

end.

