unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, VTJSON, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplyPropertiesButton: TButton;
    JSONFileNameEdit: TFileNameEdit;
    JSONInspector: TVirtualJSONInspector;
    Label1: TLabel;
    Label2: TLabel;
    PropertiesMemo: TMemo;
    procedure ApplyPropertiesButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JSONFileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure JSONInspectorFormatValue(const PropName: String;
      PropData: TJSONData; var Result: String; var Handled: Boolean);
  private
    { private declarations }
    JObject: TJSONObject;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  strutils, jsonparser;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JObject := TJSONObject.Create(['name', 'Luiz Américo', 'age', 32, 'gender', 'M']);
  JSONInspector.JSONObject := JObject;
end;

procedure TMainForm.ApplyPropertiesButtonClick(Sender: TObject);
begin
  JSONInspector.Properties := PropertiesMemo.Lines;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JObject.Destroy;
end;

procedure TMainForm.JSONFileNameEditAcceptFileName(Sender: TObject;
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
    Stream := TFileStream.Create(Value, fmOpenRead);
    Parser := TJSONParser.Create(Stream);
    try
      Data := Parser.Parse;
    except
      ShowMessage(Format('Error parsing "%s"', [Value]));
    end;
  finally
    Parser.Free;
    Stream.Free;
  end;
  if Data <> nil then
  begin
    if Data is TJSONObject then
    begin
      JObject.Free;
      JObject := TJSONObject(Data);
      JSONInspector.JSONObject := JObject;
    end
    else
      ShowMessage(Format('Expecting a TJSONObject got "%s"', [Data.ClassName]));
  end;
end;

procedure TMainForm.JSONInspectorFormatValue(const PropName: String;
  PropData: TJSONData; var Result: String; var Handled: Boolean);
begin
  if PropName = 'gender' then
  begin
    Result := IfThen(PropData.AsString = 'M', 'Masculino', 'Feminino');
    Handled := True;
  end;
end;

end.

