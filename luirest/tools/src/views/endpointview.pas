unit EndPointView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls, ComCtrls, JSONFormMediator, fpjson;

type

  { TEndPointFrame }

  TEndPointFrame = class(TFrame)
    SaveButton: TButton;
    Label3: TLabel;
    ResponseCodeLabel: TLabel;
    SendButton: TButton;
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
    procedure SendButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FEndPointData: TJSONObject;
    procedure LoadTest;
    procedure SaveTest;
  public
    procedure SetEndPoint(Data: TJSONObject);
  end;

implementation

uses
  JSONSchemaBuilderView, LuiJSONUtils, LuiJSONHelpers, Dialogs, RESTUtils;

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
      TestsMemo.Text := FormatTest(MethodLabel.Caption, SchemaData.FormatJSON());
      SchemaData.Destroy;
    end;
  finally
    ResponseData.Destroy;
  end;
end;

procedure TEndPointFrame.SendButtonClick(Sender: TObject);
var
  ResponseText: String;
  ResponseCode: Integer;
begin
  if not DoRequest(FEndPointData, Trim(RequestBodyMemo.Text), ResponseText, ResponseCode) then
    ShowMessage('Unable to run request');
  ResponseCodeLabel.Caption := IntToStr(ResponseCode);
  ResponseBodyMemo.Text := ResponseText;
end;

procedure TEndPointFrame.SaveButtonClick(Sender: TObject);
begin
  FPONotifyObservers(Self, ooChange, FEndPointData);
  EndPointMediator.SaveData;
  SaveTest;
end;

procedure TEndPointFrame.LoadTest;
var
  EventsData: TJSONArray;
  TestData: TJSONObject;
  TestStr: String;
begin
  TestStr := '';
  if FEndPointData.Find('event', EventsData) and EventsData.Find(['listen', 'test'], TestData) then
    TestStr := TestData.GetPath('script.exec', '');
  TestsMemo.Text := TestStr;
end;

procedure TEndPointFrame.SaveTest;
var
  EventsData: TJSONArray;
  TestData: TJSONObject;
  TestStr: String;
begin
  TestStr := TestsMemo.Text;
  if not FEndPointData.Find('event', EventsData) then
  begin
    EventsData := TJSONArray.Create;
    FEndPointData.Arrays['event'] := EventsData;
  end;

  if not EventsData.Find(['listen', 'test'], TestData) then
  begin
    TestData := TJSONObject.Create([
      'listen', 'test',
      'script', TJSONObject.Create([
        'type', 'text/javascript'
      ])
    ]);
    EventsData.Add(TestData);
  end;
  SetJSONPath(TestData, 'script.exec', TJSONString.Create(TestStr));
end;

procedure TEndPointFrame.SetEndPoint(Data: TJSONObject);
begin
  FEndPointData := Data;
  EndPointMediator.Data := Data;
  EndPointMediator.LoadData;
  ResponseBodyMemo.Clear;
  LoadTest;
end;

end.

