unit EndPointView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls, ComCtrls, JSONFormMediator, fpjson;

type

  { TEndPointFrame }

  TEndPointFrame = class(TFrame)
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
  private

  public
    procedure SetEndPoint(EndPointData: TJSONObject);
  end;

implementation

uses
  JSONSchemaBuilderView, LuiJSONUtils, Dialogs;

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

procedure TEndPointFrame.SetEndPoint(EndPointData: TJSONObject);
begin
  EndPointMediator.Data := EndPointData;
  EndPointMediator.LoadData;
  //todo: load test
end;

end.

