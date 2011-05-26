unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  ExtCtrls, StdCtrls, VTJSON, fpjson, jsonparser, jsonscanner, VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm)
    LogMemo: TMemo;
    ShowCheckedButton: TButton;
    EnableCheckItemsCheckBox: TCheckBox;
    FileNameEdit: TFileNameEdit;
    JSONListView: TVirtualJSONListView;
    DefaultFieldNameEdit: TLabeledEdit;
    procedure DefaultFieldNameEditEditingDone(Sender: TObject);
    procedure EnableCheckItemsCheckBoxChange(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormDestroy(Sender: TObject);
    procedure ShowCheckedButtonClick(Sender: TObject);
  private
    { private declarations }
    JSONData: TJSONData;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Data: TJSONData;
begin
  Stream := nil;
  Parser := nil;
  Data := nil;
  try
    try
      Stream := TFileStream.Create(Value, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      Data := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      ShowMessageFmt('Error opening "%s" : %s', [Value, E.Message]);
    on E: EJSONScanner do
    begin
      ShowMessageFmt('Error parsing "%s" : %s', [Value, E.Message]);
    end;
    on E: EScannerError do
    begin
      ShowMessageFmt('Error parsing "%s" : %s', [Value, E.Message]);
    end;
  end;
  if Data <> nil then
  begin
    if Data.JSONType in [{jtObject,} jtArray] then
    begin
      JSONData.Free;
      JSONData := Data;
      JSONListView.Data := JSONData;
    end
    else
    begin
      ShowMessageFmt('Expecting a TJSONArray got "%s"', [Data.ClassName]);
      Data.Destroy;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JSONData.Free;
end;

procedure TMainForm.ShowCheckedButtonClick(Sender: TObject);
begin
  LogMemo.Lines.Text := JSONListView.CheckedData.FormatJSON([foSingleLineObject]);
end;

procedure TMainForm.DefaultFieldNameEditEditingDone(Sender: TObject);
begin
  JSONListView.DefaultFieldName := DefaultFieldNameEdit.Text;
  JSONListView.LoadData;
end;

procedure TMainForm.EnableCheckItemsCheckBoxChange(Sender: TObject);
begin
  if EnableCheckItemsCheckBox.Checked then
    JSONListView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions + [toCheckSupport]
  else
    JSONListView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions - [toCheckSupport];
end;

end.

