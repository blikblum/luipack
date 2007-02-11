unit fmain;

{ Viewer for Multilog messages

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

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
//todo: - Use only one StringGrid for Watches
//      - Optimize Watch update (Cache current values?)

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, multilog, VirtualStringTree,VirtualTrees, ComCtrls, Buttons, simpleipc, watchlist,
  Menus, ATBinHex;

type
  TMessageSet = 0..31;
  { TfrmMain }

  TfrmMain = class(TForm)
    BinHexViewer: TATBinHex;
    butFilter: TButton;
    butToggleFilter: TButton;
    butSelectAll: TButton;
    butUnSelectAll: TButton;
    checkHeapInfo: TCheckBox;
    checkInfo: TCheckBox;
    checkMemory: TCheckBox;
    checkCustomData: TCheckBox;
    checkBitmap: TCheckBox;
    checkWarning: TCheckBox;
    checkError: TCheckBox;
    checkValue: TCheckBox;
    checkConditional: TCheckBox;
    checkCheckPoint: TCheckBox;
    checkStrings: TCheckBox;
    checkObject: TCheckBox;
    checkException: TCheckBox;
    checkCallStack: TCheckBox;
    editTitleFilter: TEdit;
    ImgViewer: TImage;
    imgMessages: TImageList;
    Label1: TLabel;
    lbMemorySize: TLabel;
    memoViewer: TMemo;
    nbWatches: TNotebook;
    nbViewer: TNotebook;
    PageBitmap: TPage;
    PageHexViewer: TPage;
    pageNull: TPage;
    pageText: TPage;
    pageSelected: TPage;
    pageLastest: TPage;
    panelFilter: TPanel;
    panelMessages: TPanel;
    panelViewer: TPanel;
    panelLeft: TPanel;
    panelRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    gridCallStack: TStringGrid;
    gridLastestWatch: TStringGrid;
    gridSelectedWatch: TStringGrid;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    StringGridBitmap: TStringGrid;
    TabControl1: TTabControl;
    toolbarMain: TToolBar;
    tbutClear: TToolButton;
    vtreeMessages: TVirtualStringTree;
    procedure butFilterClick(Sender: TObject);
    procedure butSelectAllClick(Sender: TObject);
    procedure butToggleFilterClick(Sender: TObject);
    procedure butUnSelectAllClick(Sender: TObject);
    procedure ClearMessages(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure nbWatchesPageChanged(Sender: TObject);
    procedure QuitApplication(Sender: TObject);
    procedure vtreeMessagesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtreeMessagesFocusChanging(Sender: TBaseVirtualTree;
      OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtreeMessagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: LongInt);
    procedure vtreeMessagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vtreeMessagesInitNode(Sender: TBaseVirtualTree;
      ParentNode: PVirtualNode; Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
    FTitleFilter: String;
    FActiveMessages: set of TMessageSet;
    FExpandParent: Boolean;
    FMessageCount: LongWord;
    FActiveWatch: TStringGrid;
    FCurrentMsg: TLogMessage;
    FLastParent: PVirtualNode;
    FLastNode: PVirtualNode;
    FFilterVisible: Boolean;
    FIPCServer: TSimpleIPCServer;
    FWatches: TWatchList;
    procedure FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SetupFilters;
    procedure ToggleFilterSelected(CheckState:Boolean);
    procedure WatchCallback(const AVariable,AValue: String);
    procedure ReceiveMessage(Sender: TObject);
    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateWatches;
    procedure ShowBitmapInfo(ABitmap: TBitmap);
    {$ifdef unix}
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    {$endif}
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  StrUtils;

type
  TNodeData = record
    Title: String;
    MsgType: Integer;
    MsgData: TStream;
    MsgTime: TDateTime;
    Index: LongWord;
  end;
  PNodeData = ^TNodeData;

const
  PixelFormatNames: array [TPixelFormat] of String =
  (
    'pfDevice',
    'pf1bit',
    'pf4bit',
    'pf8bit',
    'pf15bit',
    'pf16bit',
    'pf24bit',
    'pf32bit',
    'pfCustom'
    );
  HandleTypeNames: array [TBitmapHandleType] of String =
  ('bmDIB',
  'bmDDB');
  
{ TfrmMain }

procedure TfrmMain.butToggleFilterClick(Sender: TObject);
begin
  if FFilterVisible then
  begin
    butToggleFilter.Caption:='More Options';
    panelFilter.Height:=editTitleFilter.Top+editTitleFilter.Height+4;
  end
  else
  begin
    butToggleFilter.Caption:='Less Options';
    panelFilter.Height:=checkValue.Top+checkValue.Height+4;
  end;
  FFilterVisible:=not FFilterVisible;
end;

procedure TfrmMain.butUnSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(False);
end;

procedure TfrmMain.ClearMessages(Sender: TObject);
begin
  vtreeMessages.Clear;
  gridCallStack.RowCount:=1;
  gridLastestWatch.RowCount:=1;
  gridSelectedWatch.RowCount:=1;
  //memoViewer.Lines.Clear;
  nbViewer.ActivePageComponent:=pageNull;
  FMessageCount:=0;
end;

procedure TfrmMain.butFilterClick(Sender: TObject);
begin
  SetupFilters;
  //Scans all tree nodes
  vtreeMessages.IterateSubtree(nil,@FilterCallback,nil);
end;

procedure TfrmMain.butSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(True);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef unix}
  Application.OnIdle:=@ApplicationIdle;
  {$endif}
  FFilterVisible:=True;
  vtreeMessages.NodeDataSize:=SizeOf(TNodeData);
  //TabControl1.BorderStyle:=bsNone;
  FWatches:=TWatchList.Create;
  FWatches.OnUpdate:=@WatchCallback;
  FIPCServer:=TSimpleIPCServer.Create(nil);
  with FIPCServer do
  begin
    ServerID:='ipc_log_server';
    Global:=True;
    OnMessage:=@ReceiveMessage;
    StartServer;
  end;
  SetupFilters;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FIPCServer.Destroy;
  FWatches.Destroy;
end;

procedure TfrmMain.nbWatchesPageChanged(Sender: TObject);
begin
  if vtreeMessages.FocusedNode <> nil then
    UpdateWatches;
end;

procedure TfrmMain.QuitApplication(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.vtreeMessagesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AStream:TStringStream;
begin
  UpdateWatches;
  with PNodeData(Sender.GetNodeData(Node))^do
  begin
    if MsgData = nil then
    begin
      nbViewer.ActivePageComponent:=pageNull;
      Exit;
    end;
    MsgData.Position:=0;
    case MsgType of
    ltStrings,ltCallStack,ltException,ltHeapInfo,ltCustomData:
    begin
      memoViewer.Lines.LoadFromStream(MsgData);
      nbViewer.ActivePageComponent:=pageText;
    end;
    ltObject:
    begin
      AStream:=TStringStream.Create('');
      ObjectBinaryToText(MsgData,AStream);
      memoViewer.Lines.Text:=AStream.DataString;
      nbViewer.ActivePageComponent:=pageText;
      AStream.Destroy;
    end;
    ltBitmap:
    begin
      ImgViewer.Picture.Bitmap.LoadFromStream(MsgData);
      nbViewer.ActivePageComponent:=PageBitmap;
      ShowBitmapInfo(ImgViewer.Picture.Bitmap);
    end;
    ltMemory:
    begin
      lbMemorySize.Caption:='Size: '+IntToStr(MsgData.Size);
      BinHexViewer.OpenStream(MsgData);
      nbViewer.ActivePageComponent:=PageHexViewer;
    end;
    end;
  end;
end;

procedure TfrmMain.vtreeMessagesFocusChanging(Sender: TBaseVirtualTree;
  OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed:=OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or (OldNode^.Parent<>NewNode^.Parent)) then
    UpdateCallStack(NewNode);
  //warning NewNode value is not more valid after here
end;

procedure TfrmMain.vtreeMessagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:='';
    if MsgData <> nil then
      MsgData.Destroy;
  end;
end;

procedure TfrmMain.vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: LongInt);
begin
  ImageIndex:=PNodeData(Sender.GetNodeData(Node))^.MsgType;
end;

procedure TfrmMain.vtreeMessagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  CellText:=PNodeData(Sender.GetNodeData(Node))^.Title;
end;

procedure TfrmMain.vtreeMessagesInitNode(Sender: TBaseVirtualTree;
  ParentNode: PVirtualNode; Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  //WriteLn('InitNode Called');
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:=FCurrentMsg.MsgText;
    MsgData:=FCurrentMsg.Data;
    MsgTime:=FCurrentMsg.MsgTime;
    MsgType:=FCurrentMsg.MsgType;
    //In fast computers two or more messages can have the same TimeStamp
    //This leads to conflicts when determining the Watches values
    //Uses an unique index instead
    Index:= FMessageCount;
    //Show only what matches filter criterias
    Sender.IsVisible[Node]:= (MsgType in [ltEnterMethod,ltExitMethod]) or
     ((MsgType in FActiveMessages) and IsWild(Title,FTitleFilter,True));
  end;
end;

procedure TfrmMain.FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
    Sender.IsVisible[Node]:= (MsgType in [ltEnterMethod,ltExitMethod]) or
     ((MsgType in FActiveMessages) and IsWild(Title,FTitleFilter,True));
end;

procedure TfrmMain.SetupFilters;
var
  AControl: TControl;
  i: Integer;
begin
  //Set Active Messages set used to filter
    FActiveMessages:=[4,5];//always show Enter/ExitMethod
    with panelFilter do
    for i:= 0 to ControlCount - 1 do
    begin
      AControl:=Controls[i];
      if (AControl is TCheckBox) and (TCheckBox(AControl)).Checked then
        Include(FActiveMessages, AControl.Tag);
    end;
    //Set Title Filter
    FTitleFilter:=Trim(editTitleFilter.Text)+'*';
    if Length(FTitleFilter) > 1 then //editFilter is not empty
      FTitleFilter:='*'+FTitleFilter;
    //writeln('FFilter:', FTitleFilter);
end;

procedure TfrmMain.ToggleFilterSelected(CheckState:Boolean);
var
  AControl: TControl;
  i: Integer;
begin
  with panelFilter do
   for i:= 0 to ControlCount - 1 do
   begin
     AControl:=Controls[i];
     if (AControl is TCheckBox) then
        TCheckBox(AControl).Checked := CheckState;
   end;
end;

procedure TfrmMain.WatchCallback(const AVariable, AValue: String);
begin
  with FActiveWatch do
  begin
    RowCount:=RowCount+1;
    Cells[0,RowCount-1]:=AVariable;
    Cells[1,RowCount-1]:=AValue;
  end;
end;

procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  TextSize,DataSize:Integer;
begin
  Inc(FMessageCount);
  with TSimpleIPCServer(Sender).MsgData do
  begin
    Seek(0,soFromBeginning);
    ReadBuffer(FCurrentMsg.MsgType,SizeOf(Integer));
    ReadBuffer(FCurrentMsg.MsgTime,SizeOf(TDateTime));
    ReadBuffer(TextSize,SizeOf(Integer));
    SetLength(FCurrentMsg.MsgText,TextSize);
    ReadBuffer(FCurrentMsg.MsgText[1],TextSize);
    ReadBuffer(DataSize,SizeOf(Integer));
    if DataSize > 0 then
    begin
      FCurrentMsg.Data:=TMemoryStream.Create;
      FCurrentMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,DataSize);
    end
    else
      FCurrentMsg.Data:=nil;
      
    case FCurrentMsg.MsgType of
    ltEnterMethod:
    begin
      FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
      if FExpandParent then
        vtreeMessages.Expanded[FLastParent]:=True
      else
        FExpandParent:=True;
      FLastParent:=FLastNode;
      vtreeMessages.ValidateNode(FLastNode,False);
    end;
    ltExitMethod:
    begin
      if (FLastParent = nil) or (FLastParent^.Parent = vtreeMessages.RootNode) then
      begin
        FLastNode:=vtreeMessages.AddChild(nil,nil);
        FLastParent:=nil;
      end
      else
      begin
        FLastNode:=vtreeMessages.AddChild(FLastParent^.Parent,nil);
        FLastParent:=FLastNode^.Parent;
      end;
      vtreeMessages.ValidateNode(FLastNode,False);
    end;
    ltWatch:
    begin
      FWatches.Add(FCurrentMsg.MsgText,FMessageCount);
      UpdateWatches;
    end;
    ltClear:
    begin
      ClearMessages(nil);
      FLastNode:=nil;
      FLastParent:=nil;
    end
    else
      FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
      vtreeMessages.ValidateNode(FLastNode,False);
      if FExpandParent then
      begin
        vtreeMessages.Expanded[FLastParent]:=True;
        FExpandParent:=False;
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateCallStack(var ANode: PVirtualNode);
var
  i:Integer;
begin
  //Writeln('UpdateCallstack');
  with vtreeMessages, gridCallStack do
  begin
    i:=GetNodeLevel(ANode);
    RowCount:=Succ(i);
    while i > 0 do
    begin
      Cells[0,i]:=PNodeData(GetNodeData(ANode^.Parent))^.Title;
      ANode:=ANode^.Parent;
      Dec(i);
    end;
  end;
end;

procedure TfrmMain.UpdateWatches;
var
  TempIndex: LongWord;
begin
  if nbWatches.ActivePageComponent = pageLastest then
  begin
    FActiveWatch:=gridLastestWatch;
    TempIndex:=FMessageCount;
  end
  else
  begin
    FActiveWatch:=gridSelectedWatch;
    if vtreeMessages.FocusedNode <> nil then
      TempIndex:=PNodeData(vtreeMessages.GetNodeData(vtreeMessages.FocusedNode))^.Index
    else
      TempIndex:=0;
  end;
  FActiveWatch.RowCount:=1;
  FWatches.Update(TempIndex);
end;

procedure TfrmMain.ShowBitmapInfo(ABitmap: TBitmap);
begin
  with StringGridBitmap, ABitmap do
  begin
    Cells[1,0]:=IntToStr(Height);
    Cells[1,1]:=IntToStr(Width);
    Cells[1,2]:=PixelFormatNames[PixelFormat];
    Cells[1,3]:=HandleTypeNames[HandleType];
    Cells[1,4]:='$'+IntToHex(TransparentColor,8);
  end;
end;

{$ifdef unix}
procedure TfrmMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FIPCServer.PeekMessage(1,True);
end;
{$endif}

initialization
  {$I fmain.lrs}

end.

