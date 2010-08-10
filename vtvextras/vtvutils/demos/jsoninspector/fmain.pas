unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, VTJSON, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    FormatNameCheckBox: TCheckBox;
    UpdatePropertyDefsButton: TButton;
    SkipUnknownCheckBox: TCheckBox;
    SkipNullCheckBox: TCheckBox;
    JSONFileNameEdit: TFileNameEdit;
    JSONInspector: TVirtualJSONInspector;
    Label1: TLabel;
    Label2: TLabel;
    PropertiesMemo: TMemo;
    procedure FormatNameCheckBoxChange(Sender: TObject);
    procedure JSONInspectorFormatName(Sender: TVirtualJSONInspector;
      ParentData: TJSONData; ItemIndex: Integer; var DisplayName: String);
    procedure JSONInspectorFormatValue(Sender: TVirtualJSONInspector;
      const PropertyName: String; Data: TJSONData; var DisplayText: String);
    procedure SkipNullCheckBoxChange(Sender: TObject);
    procedure SkipUnknownCheckBoxChange(Sender: TObject);
    procedure UpdatePropertyDefsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JSONFileNameEditAcceptFileName(Sender: TObject; var Value: String);
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

procedure TMainForm.JSONInspectorFormatValue(Sender: TVirtualJSONInspector;
  const PropertyName: String; Data: TJSONData; var DisplayText: String);
begin
  if PropertyName = 'gender' then
  begin
    DisplayText := IfThen(Data.AsString = 'M', 'Masculino', 'Feminino');
  end;
end;

procedure TMainForm.JSONInspectorFormatName(Sender: TVirtualJSONInspector;
  ParentData: TJSONData; ItemIndex: Integer; var DisplayName: String);
var
  PropertyName: String;
begin
  if (DisplayName <> '') or not (FormatNameCheckBox.Checked) then
    Exit;
  case ParentData.JSONType of
    jtObject:
      begin
        PropertyName := TJSONObject(ParentData).Names[ItemIndex];
        DisplayName := '(' + AnsiProperCase(PropertyName, [' ']) + ')';
      end;
    jtArray:
      DisplayName := 'Item ' + IntToStr(ItemIndex);
  end;
end;

procedure TMainForm.FormatNameCheckBoxChange(Sender: TObject);
begin
  JSONInspector.Reload;
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
      ShowMessageFmt('Expecting a TJSONObject/Array got "%s"', [Data.ClassName]);
      Data.Destroy;
    end;
  end;
end;

end.

