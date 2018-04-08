unit TestBuilderView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type

  { TTestBuilderForm }

  TTestBuilderForm = class(TForm)
    GenerateTestsButton: TButton;
    MethodGroupBox: TRadioGroup;
    PageControl1: TPageControl;
    ResponseBodyMemo: TMemo;
    ResponseBodyTabSheet: TTabSheet;
    ResponseTestsTabSheet: TTabSheet;
    TestsMemo: TMemo;
    procedure GenerateTestsButtonClick(Sender: TObject);
  private

  public

  end;

var
  TestBuilderForm: TTestBuilderForm;

implementation

uses
  fpjson, LuiJSONUtils,
  RESTUtils, JSONSchemaBuilderView;

{$R *.lfm}

{ TTestBuilderForm }

procedure TTestBuilderForm.GenerateTestsButtonClick(Sender: TObject);
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
      TestsMemo.Text := FormatTest(MethodGroupBox.Items[MethodGroupBox.ItemIndex], SchemaData.FormatJSON());
      SchemaData.Destroy;
    end
    else
    begin
      TestsMemo.Clear;
      ShowMessage('Unable to build schema');
    end;
  finally
    ResponseData.Destroy;
  end;
end;

end.

