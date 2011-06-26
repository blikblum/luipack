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
    procedure JSONTreeViewBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure JSONTreeViewDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; var CellRect: TRect; var DefaultDraw: Boolean);
    procedure JSONTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: TJSONData; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure JSONTreeViewMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
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

{$R *.lfm}

function NormalizeDateTime(ADate, ATime: TDateTime): String;
var
  DateStr: string;
begin
  DateTimeToString(DateStr, 'dd/mm/yyyy', ADate);
  Result := DateStr + ' - ' + TimeToStr(ATime);
end;

procedure PaintDetails(Canvas: TCanvas; Details: TJSONArray; const CellRect: TRect);
const
  ColorMap: array[0..5] of TColor = (clPurple, clBlue, clGreen, clRed, clBlack, clFuchsia);
var
  i, x, y: Integer;
  Obj: TJSONObject;
  BackColor: TColor;
  ExameText: String;
begin
  x := CellRect.Left;
  y := CellRect.Top + 20;
  SetBkMode(Canvas.Handle, OPAQUE);
  Canvas.Font.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  for i := 0 to Details.Count -1 do
  begin
    Obj := Details.Items[i] as TJSONObject;
    BackColor := ColorMap[Obj.Integers['status']];
    Canvas.Brush.Color := BackColor;
    ExameText := ' ' + Obj.Strings['exame'] + ' ';
    Canvas.TextOut(x, y, ExameText);
    Inc(x, Canvas.TextWidth(ExameText) + 3);
    if x > (CellRect.Right - CellRect.Left) then
    begin
      x := CellRect.Left;
      Inc(y, Canvas.TextHeight(ExameText));
    end;
  end;
end;

function CalcDetailsHeight(Canvas: TCanvas; Details: TJSONArray; MaxWidth: Integer): Integer;
var
  i, Width, MaxHeight: Integer;
  Obj: TJSONObject;
  ExameText: String;
  TextSize: TSize;
begin
  Canvas.Font.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  Width := 0;
  MaxHeight := 0;
  Result := 0;
  for i := 0 to Details.Count -1 do
  begin
    Obj := Details.Items[i] as TJSONObject;
    ExameText := ' ' + Obj.Strings['exame'] + ' ';
    TextSize := Canvas.TextExtent(ExameText);
    Inc(Width, TextSize.cx + 2);
    MaxHeight := Max(MaxHeight, TextSize.cy + 2);
    if Width > MaxWidth then
    begin
      Width := TextSize.cx;
      Inc(Result, MaxHeight);
    end;
  end;
  Inc(Result, MaxHeight);
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

procedure TMainForm.JSONTreeViewBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  FCellWidth := JSONTreeView.ClientWidth;
  if Sender.GetTreeRect.Bottom > Sender.ClientHeight then
    Dec(FCellWidth, GetSystemMetrics(SM_CXVSCROLL));
  if toShowRoot in JSONTreeView.TreeOptions.PaintOptions then
    Dec(FCellWidth, 2 * JSONTreeView.Indent + JSONTreeView.Margin)
  else
    Dec(FCellWidth, JSONTreeView.Indent + JSONTreeView.Margin);
end;

procedure TMainForm.JSONTreeViewDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; var CellRect: TRect; var DefaultDraw: Boolean);
var
  ObjData: TJSONObject;
  DateTimeStr: String;
  R: TRect;
begin
  DefaultDraw := Sender.GetNodeLevel(Node) = 0;
  if not DefaultDraw then
  begin
    ObjData := JSONTreeView.GetData(Node) as TJSONObject;
    R := CellRect;
    DrawText(TargetCanvas.Handle, PChar(CellText), Length(CellText), R, DT_LEFT or DT_TOP);
    DateTimeStr := NormalizeDateTime(ObjData.Floats['date'], ObjData.Floats['time']);
    DrawText(TargetCanvas.Handle, PChar(DateTimeStr), Length(DateTimeStr), R, DT_RIGHT or DT_TOP);
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
      ' - ' + ObjData.Strings['name'];
  end;
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

end.

