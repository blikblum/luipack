unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, VTJSON, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    UpdatePropertyDefsButton: TButton;
    SkipUnknownCheckBox: TCheckBox;
    SkipNullCheckBox: TCheckBox;
    JSONFileNameEdit: TFileNameEdit;
    JSONInspector: TVirtualJSONInspector;
    Label1: TLabel;
    Label2: TLabel;
    PropertiesMemo: TMemo;
    procedure SkipNullCheckBoxChange(Sender: TObject);
    procedure SkipUnknownCheckBoxChange(Sender: TObject);
    procedure UpdatePropertyDefsButtonClick(Sender: TObject);
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
  strutils, jsonparser, LuiStrUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JSONData := TJSONObject.Create(['name', 'Luiz Américo', 'age', 32, 'gender', 'M']);
  JSONInspector.RootData := JSONData;
end;

procedure TMainForm.UpdatePropertyDefsButtonClick(Sender: TObject);
var
  i: Integer;
  AName, DisplayName: String;
begin
  JSONInspector.PropertyDefs.Clear;
  for i := 0 to PropertiesMemo.Lines.Count - 1 do
  begin
    ExtractNameValue(Trim(PropertiesMemo.Lines[i]), AName, DisplayName);
    if Name <> '' then
      JSONInspector.PropertyDefs.Add(AName, DisplayName);
  end;
  JSONInspector.Reload;
end;

procedure TMainForm.SkipNullCheckBoxChange(Sender: TObject);
begin
  if SkipNullCheckBox.Checked then
    JSONInspector.TreeOptions.JSONOptions := JSONInspector.TreeOptions.JSONOptions + [jioSkipNullProperties]
  else
    JSONInspector.TreeOptions.JSONOptions := JSONInspector.TreeOptions.JSONOptions - [jioSkipNullProperties];
end;

procedure TMainForm.SkipUnknownCheckBoxChange(Sender: TObject);
begin
  if SkipUnknownCheckBox.Checked then
    JSONInspector.TreeOptions.JSONOptions := JSONInspector.TreeOptions.JSONOptions + [jioSkipUnknownProperties]
  else
    JSONInspector.TreeOptions.JSONOptions := JSONInspector.TreeOptions.JSONOptions - [jioSkipUnknownProperties];
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
      JSONInspector.RootData := JSONData;
      JSONInspector.FullExpand;
    end
    else
    begin
      ShowMessage(Format('Expecting a TJSONObject/Array got "%s"', [Data.ClassName]));
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

