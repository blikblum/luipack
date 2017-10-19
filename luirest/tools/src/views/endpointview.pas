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
    procedure DataChange(Sender: TObject);
    procedure Save;
    procedure SendButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FEndPointData: TJSONObject;
    FIsModified: Boolean;
    procedure LoadTest;
    procedure LoadURL;
    procedure SaveTest;
    procedure SaveURL;
  public
    procedure SetEndPoint(Data: TJSONObject);
  published
    property IsModified: Boolean read FIsModified;
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
      FIsModified := True;
      SchemaData.Destroy;
    end;
  finally
    ResponseData.Destroy;
  end;
end;

procedure TEndPointFrame.DataChange(Sender: TObject);
begin
  FIsModified := True;
end;

procedure TEndPointFrame.Save;
begin
  FPONotifyObservers(Self, ooChange, FEndPointData);
  EndPointMediator.SaveData;
  SaveTest;
  SaveURL;
  FIsModified := False;
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
  PageControl1.ActivePageIndex := 0;
end;

procedure TEndPointFrame.SaveButtonClick(Sender: TObject);
begin
  Save;
end;

procedure StringsToJSONArray(Strings: TStrings; JSONArray: TJSONArray);
var
  i: Integer;
begin
  for i := 0 to Strings.Count - 1 do
    JSONArray.Add(Strings[i]);
end;

procedure JSONArrayToStrings(JSONArray: TJSONArray; Strings: TStrings);
var
  i: Integer;
begin
  for i := 0 to JSONArray.Count - 1 do
    Strings.Add(JSONArray.Strings[i]);
end;

procedure TEndPointFrame.LoadTest;
var
  EventsData: TJSONArray;
  TestData: TJSONObject;
  ExecData: TJSONData;
begin
  TestsMemo.Lines.BeginUpdate;
  try
    TestsMemo.Clear;
    if FEndPointData.Find('event', EventsData) and EventsData.Find(['listen', 'test'], TestData) then
    begin
      if TestData.FindPath('script.exec', ExecData) then
      begin
        case ExecData.JSONType of
          jtString:
            TestsMemo.Text := ExecData.AsString;
          jtArray:
            JSONArrayToStrings(TJSONArray(ExecData), TestsMemo.Lines);
          else
            ShowMessage(Format('Unknow test exec type: %s', [JSONTypeName(ExecData.JSONType)]));
        end;
      end;
    end;
  finally
    TestsMemo.Lines.EndUpdate;
  end;
end;

procedure TEndPointFrame.LoadURL;
var
  URLData: TJSONData;
begin
  URLEdit.Text := '';
  if FEndPointData.FindPath('request.url', URLData) then
  begin
    case URLData.JSONType of
      jtString:
        URLEdit.Text := URLData.AsString;
      jtObject:
        URLEdit.Text := URLData.GetPath('raw', '');
      else
        ShowMessage(Format('Unknow url type: %s', [JSONTypeName(URLData.JSONType)]));
    end;
  end;
end;

procedure TEndPointFrame.SaveTest;
var
  EventsData: TJSONArray;
  TestData: TJSONObject;
  ExecData: TJSONArray;
begin
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
  ExecData := TJSONArray.Create;
  StringsToJSONArray(TestsMemo.Lines, ExecData);
  SetJSONPath(TestData, 'script.exec', ExecData);
end;

procedure TEndPointFrame.SaveURL;
var
  URLValue: String;
  URLData: TJSONObject;
begin
  URLValue := Trim(URLEdit.Text);
  if FEndPointData.FindPath('request.url', URLData) then
  begin
    if URLData.Get('raw') = URLValue then
      Exit;
  end;
  SetJSONPath(FEndPointData, 'request.url', TJSONString.Create(URLValue));
end;

procedure TEndPointFrame.SetEndPoint(Data: TJSONObject);
begin
  FEndPointData := Data;
  EndPointMediator.Data := Data;
  EndPointMediator.LoadData;
  ResponseBodyMemo.Clear;
  LoadTest;
  LoadURL;
  FIsModified := False;
end;

end.

