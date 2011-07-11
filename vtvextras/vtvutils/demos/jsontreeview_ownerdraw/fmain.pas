unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, VTJSON,
  fpjson, jsonparser, jsonscanner, VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm)
    JSONTreeView: TVirtualJSONTreeView;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JSONTreeViewBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure JSONTreeViewDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; var CellRect: TRect; var DefaultDraw: Boolean);
    procedure JSONTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: TJSONData; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure JSONTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure JSONTreeViewMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure JSONTreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure JSONTreeViewResize(Sender: TObject);
  private
    { private declarations }
    Data: TJSONData;
    FCellWidth: Integer;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  strutils, LCLIntf, LCLType, Math, Types;

const
  ITEM_SPACE = 3;

{$R *.lfm}

function NormalizeDateTime(ADate, ATime: TDateTime): String;
var
  DateStr: string;
begin
  DateTimeToString(DateStr, 'dd/mm/yyyy', ADate);
  Result := DateStr + ' ' + TimeToStr(ATime);
end;

procedure PaintDetails(Canvas: TCanvas; Details: TJSONArray; const CellRect: TRect);
const
  ColorMap: array[0..5] of TColor = (clPurple, clBlue, clGreen, clRed, clBlack, clFuchsia);
var
  i, x, y, TextWidth: Integer;
  Obj: TJSONObject;
  BackColor: TColor;
  ExameText: String;
begin
  x := CellRect.Left;
  y := CellRect.Top;
  SetBkMode(Canvas.Handle, OPAQUE);
  Canvas.Font.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  for i := 0 to Details.Count - 1 do
  begin
    Obj := Details.Objects[i];
    BackColor := ColorMap[Obj.Integers['status']];
    Canvas.Brush.Color := BackColor;
    ExameText := ' ' + Obj.Strings['exame'] + ' ';
    TextWidth := Canvas.TextWidth(ExameText);
    if (x + TextWidth) > CellRect.Right then
    begin
      x := CellRect.Left;
      Inc(y, Canvas.TextHeight(ExameText) + 4);
    end;
    Canvas.TextOut(x, y, ExameText);
    Inc(x, TextWidth + ITEM_SPACE);
  end;
end;

function CalcDetailsHeight(Canvas: TCanvas; Details: TJSONArray; MaxWidth: Integer): Integer;
var
  i, Width, MaxHeight: Integer;
  Obj: TJSONObject;
  ExameText: String;
  TextSize: TSize;
begin
  Canvas.Font.Style := [fsBold];
  Width := 0;
  MaxHeight := 16;
  Result := 1;
  for i := 0 to Details.Count -1 do
  begin
    Obj := Details.Objects[i];
    ExameText := ' ' + Obj.Strings['exame'] + ' ';
    TextSize := Canvas.TextExtent(ExameText);
    Inc(Width, TextSize.cx);
    MaxHeight := Max(MaxHeight, TextSize.cy + 4);
    if Width > MaxWidth then
    begin
      Width := TextSize.cx + ITEM_SPACE;
      Inc(Result);
    end
    else
      Inc(Width, ITEM_SPACE);
  end;
  Result := Result * MaxHeight;
end;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
const
  FileName = 'maptests.json';
var
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  Stream := nil;
  Parser := nil;
  Data := nil;
  try
    try
      Stream := TFileStream.Create(FileName, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      Data := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      ShowMessageFmt('Error opening "%s" : %s', [FileName, E.Message]);
    on E: EJSONScanner do
    begin
      ShowMessageFmt('Error parsing "%s" : %s', [FileName, E.Message]);
    end;
    on E: EScannerError do
    begin
      ShowMessageFmt('Error parsing "%s" : %s', [FileName, E.Message]);
    end;
  end;
  if Data <> nil then
  begin
    if Data.JSONType in [{jtObject,} jtArray] then
    begin
      JSONTreeView.Data := Data;
      JSONTreeView.FullExpand;
    end
    else
    begin
      ShowMessageFmt('Expecting a TJSONArray got "%s"', [Data.ClassName]);
    end;
  end;
end;

procedure TMainForm.JSONTreeViewBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    EraseAction := eaColor;
    ItemColor := clSilver;
  end;
end;

procedure TMainForm.JSONTreeViewDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; var CellRect: TRect; var DefaultDraw: Boolean);
var
  ObjData: TJSONObject;
  R: TRect;
begin
  DefaultDraw := Sender.GetNodeLevel(Node) = 0;
  if not DefaultDraw then
  begin
    ObjData := JSONTreeView.GetData(Node) as TJSONObject;
    R := CellRect;
    Inc(R.Top, 2);
    DrawText(TargetCanvas.Handle, PChar(CellText), Length(CellText), R, DT_LEFT or DT_TOP);
    Inc(R.Top, 16);
    PaintDetails(TargetCanvas, ObjData.Arrays['details'], R);
  end;
end;

procedure TMainForm.JSONTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: TJSONData; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  ObjData: TJSONObject absolute Data;
begin
  if (Sender.GetNodeLevel(Node) = 0) and (Data.JSONType = jtObject) then
  begin
    CellText := AddChar('0', ObjData.Strings['bednumber'], 2) +
      ' - ' + CellText;
  end
  else
    CellText := NormalizeDateTime(ObjData.Floats['date'], ObjData.Floats['time']) +
      ' - ' + CellText;
end;

procedure TMainForm.JSONTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if ParentNode = nil then
    Node^.CheckType := ctNone;
end;

procedure TMainForm.JSONTreeViewMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  ObjData: TJSONObject;
begin
  if Sender.GetNodeLevel(Node) = 0 then
    NodeHeight := 18
  else
  begin
    ObjData := JSONTreeView.GetData(Node) as TJSONObject;
    NodeHeight := 18 + CalcDetailsHeight(TargetCanvas, ObjData.Arrays['details'],
      FCellWidth);
  end;
end;

procedure TMainForm.JSONTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    TargetCanvas.Font.Style := [fsBold];
end;

procedure TMainForm.JSONTreeViewResize(Sender: TObject);
begin
  FCellWidth := JSONTreeView.ClientWidth;
  if JSONTreeView.GetTreeRect.Bottom > JSONTreeView.ClientHeight then
    Dec(FCellWidth, GetSystemMetrics(SM_CXVSCROLL));
  if toShowRoot in JSONTreeView.TreeOptions.PaintOptions then
    Dec(FCellWidth, 2 * JSONTreeView.Indent + JSONTreeView.Margin + (2 * JSONTreeView.TextMargin))
  else
    Dec(FCellWidth, JSONTreeView.Indent + JSONTreeView.Margin + (2 * JSONTreeView.TextMargin));
  JSONTreeView.InvalidateChildren(nil, True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

end.

