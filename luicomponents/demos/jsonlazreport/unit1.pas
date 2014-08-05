unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Desgn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LuiJSONLazReport, LuiJSONUtils, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    frDesigner1: TfrDesigner;
    JSONDataMemo: TMemo;
    ShowReportButton: TButton;
    EditReportButton: TButton;
    procedure EditReportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowReportButtonClick(Sender: TObject);
  private
    { private declarations }
    FJSONData: TJSONObject;
    FReport: TfrJSONReport;
    function GetJSONData: TJSONObject;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  LR_Class;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ShowReportButtonClick(Sender: TObject);
var
  ReportData: TJSONObject;
begin
  ReportData := GetJSONData;
  if ReportData <> nil then
  begin
    FReport.LoadData(ReportData, True);
    FReport.ShowReport;
  end;
end;

function TMainForm.GetJSONData: TJSONObject;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  FreeAndNil(FJSONData);
  Stream := TMemoryStream.Create;
  try
    JSONDataMemo.Lines.SaveToStream(Stream);
    Stream.Position := 0;
    try
      Result := StreamToJSON(Stream) as TJSONObject;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    Stream.Destroy;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JSONDataMemo.Lines.LoadFromFile('test.json');
  FReport := TfrJSONReport.Create(Self);
  FReport.Options := [roIgnoreSymbolNotFound];
  FReport.LoadFromFile('design.lrf');
end;

procedure TMainForm.EditReportButtonClick(Sender: TObject);
begin
  FReport.DesignReport;
end;

end.

