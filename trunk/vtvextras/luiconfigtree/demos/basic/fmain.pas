unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LuiConfig, LuiConfigTree, IniConfigProvider, VirtualTrees, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    IniProvider: TIniConfigProvider;
    ConfigTree: TLuiConfigTree;
    Config: TLuiConfig;
    IniFileMemo: TMemo;
    procedure ConfigTreeBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure ConfigTreeBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure ConfigTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: UTF8String);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateMemo;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.ConfigTreeBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if (Node^.Parent <> Sender.RootNode) and (Column = 0) then
  begin
    TargetCanvas.Brush.Color := $e0e0e0;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TMainForm.ConfigTreeBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  if Node^.Parent = Sender.RootNode then
  begin
    EraseAction := eaColor;
    ItemColor := $9f9f9f;
  end;
end;

procedure TMainForm.ConfigTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: UTF8String);
begin
  UpdateMemo;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateMemo;
end;

procedure TMainForm.UpdateMemo;
begin
  IniFileMemo.Lines.LoadFromFile('test.ini');
end;

initialization
  {$I fmain.lrs}

end.

