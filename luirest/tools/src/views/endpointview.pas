unit EndPointView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls, ComCtrls, JSONFormMediator, fpjson;

type

  { TEndPointFrame }

  TEndPointFrame = class(TFrame)
    Label3: TLabel;
    ResponseCodeLabel: TLabel;
    RunButton: TButton;
    GenerateTestsButton: TButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    Label2: TLabel;
    TestsMemo: TMemo;
    PageControl1: TPageControl;
    RequestBodyMemo: TMemo;
    MethodLabel: TLabel;
    ResponseBodyMemo: TMemo;
    ResponseBodyTabSheet: TTabSheet;
    ResponseTestsTabSheet: TTabSheet;
    URLEdit: TEdit;
    NameEdit: TEdit;
    EndPointMediator: TJSONFormMediator;
    Label1: TLabel;
    procedure GenerateTestsButtonClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
  private
    FEndPointData: TJSONObject;
  public
    procedure SetEndPoint(Data: TJSONObject);
  end;

implementation

uses
  JSONSchemaBuilderView, LuiJSONUtils, Dialogs, RESTUtils;

{$R *.lfm}

{ TEndPointFrame }

procedure TEndPointFrame.GenerateTestsButtonClick(Sender: TObject);
var
  ResponseData: TJSONData;
  SchemaData: TJSONObject;
begin
  if not TryStrToJSON(ResponseBodyMemo.Text, ResponseData) then
  begin
    ShowMessage('Unable to parse response data - invalid JSON');
    Exit;
  end;
  try
    SchemaData := TJSONSchemaBuilderForm.CreateSchema(ResponseData);
    if SchemaData <> nil then
    begin
      //todo
      TestsMemo.Text := SchemaData.FormatJSON();
      SchemaData.Destroy;
    end;
  finally
    ResponseData.Destroy;
  end;
end;

procedure TEndPointFrame.RunButtonClick(Sender: TObject);
var
  ResponseText: String;
  ResponseCode: Integer;
begin
  if not DoRequest(FEndPointData, Trim(RequestBodyMemo.Text), ResponseText, ResponseCode) then
    ShowMessage('Unable to run request');
  ResponseCodeLabel.Caption := IntToStr(ResponseCode);
  ResponseBodyMemo.Text := ResponseText;
end;

procedure TEndPointFrame.SetEndPoint(Data: TJSONObject);
begin
  FEndPointData := Data;
  EndPointMediator.Data := Data;
  EndPointMediator.LoadData;
  //todo: load test
end;

end.

