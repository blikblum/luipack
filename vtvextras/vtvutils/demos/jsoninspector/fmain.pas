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
    JSONData: TJSONData;
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
  JSONData := TJSONObject.Create(['name', 'Luiz Américo', 'age', 32, 'gender', 'M']);
  JSONInspector.JSONData := JSONData;
end;

procedure TMainForm.ApplyPropertiesButtonClick(Sender: TObject);
begin
  JSONInspector.Properties := PropertiesMemo.Lines;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JSONData.Destroy;
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
    if Data.JSONType in [jtObject, jtArray] then
    begin
      JSONData.Free;
      JSONData := Data;
      JSONInspector.JSONData := JSONData;
      JSONInspector.FullExpand;
    end
    else
    begin
      ShowMessage(Format('Expecting a TJSONObject got "%s"', [Data.ClassName]));
      Data.Destroy;
    end;
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

