unit fMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, VTJSON, fpjson, jsonparser, jsonscanner;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExpandAllButton: TButton;
    CollapseAllButton: TButton;
    FileNameEdit: TFileNameEdit;
    JSONTreeView: TVirtualJSONTreeView;
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormDestroy(Sender: TObject);
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
      JSONTreeView.Data := JSONData;
    end
    else
    begin
      ShowMessageFmt('Expecting a TJSONArray got "%s"', [Data.ClassName]);
      Data.Destroy;
    end;
  end;
end;

procedure TMainForm.ExpandAllButtonClick(Sender: TObject);
begin
  JSONTreeView.FullExpand;
end;

procedure TMainForm.CollapseAllButtonClick(Sender: TObject);
begin
  JSONTreeView.FullCollapse;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JSONData.Free;
end;


end.

