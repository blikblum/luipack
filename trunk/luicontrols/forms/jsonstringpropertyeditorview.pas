unit JSONStringPropertyEditorView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterJScript, Forms,
  Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, fpjson;

type

  { TJSONStringPropertyEditorForm }

  TJSONStringPropertyEditorForm = class(TForm)
    BtnPanel: TButtonPanel;
    JSONStringEdit: TSynEdit;
    MessageLabel: TLabel;
    SynJScriptSyn1: TSynJScriptSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JSONStringEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FData: TJSONData;
    FJSONType: TJSONtype;
    function GetJSONString: TJSONStringType;
    function LoadJSONString(const Value: TJSONStringType): Boolean;
    procedure SetJSONString(const Value: TJSONStringType);
  public
    { public declarations }
    property JSONType: TJSONtype read FJSONType write FJSONType;
    property JSONString: TJSONStringType read GetJSONString write SetJSONString;
  end;

implementation

uses
  jsonparser;

{$R *.lfm}

{ TJSONStringPropertyEditorForm }

procedure TJSONStringPropertyEditorForm.OKButtonClick(Sender: TObject);
var
  Str: String;
begin
  Str := JSONStringEdit.Lines.Text;
  if (Str = '') or LoadJSONString(Str) then
    ModalResult := mrOK;
end;

function TJSONStringPropertyEditorForm.GetJSONString: TJSONStringType;
begin
  if FData <> nil then
    Result := FData.AsJSON
  else
    //todo: remove line endings
    Result := Trim(JSONStringEdit.Lines.Text);
end;

function TJSONStringPropertyEditorForm.LoadJSONString(
  const Value: TJSONStringType): Boolean;
var
  Parser: TJSONParser;
  ErrorMessage: String;
begin
  Result := False;
  ErrorMessage := '';
  FreeAndNil(FData);
  if Trim(Value) = '' then
    Exit;
  //validate
  Parser := TJSONParser.Create(Value, True);
  try
    //todo: see why parser does not raise an exception with Value = 'some string'
    FData := Parser.Parse;
  except
    on E: Exception do
    begin
      ErrorMessage := E.Message;
    end;
  end;
  if FData = nil then
  begin
    //sometimes FData = nil but exception is not raised
    if ErrorMessage = '' then
      ErrorMessage := 'Unable to convert property value to JSON';
  end
  else
  begin
    if (FJSONType <> jtUnknown) and (FData.JSONType <> FJSONType) then
      ErrorMessage := Format('Invalid JSON format. Expected "%s" got "%s"', [JSONTypeName(FJSONType), JSONTypeName(FData.JSONType)]);
  end;
  Result := ErrorMessage = '';
  if not Result then
  begin
    MessageLabel.Caption := ErrorMessage;
    MessageLabel.Visible := True;
  end;
end;

procedure TJSONStringPropertyEditorForm.SetJSONString(
  const Value: TJSONStringType);
begin
  if LoadJSONString(Value) then
    JSONStringEdit.Lines.Text := FData.FormatJSON
  else
    JSONStringEdit.Lines.Text := Value;
end;

procedure TJSONStringPropertyEditorForm.FormCreate(Sender: TObject);
begin
  FJSONType := jtUnknown;
  BtnPanel.OKButton.ModalResult := mrNone;
end;

procedure TJSONStringPropertyEditorForm.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

procedure TJSONStringPropertyEditorForm.JSONStringEditChange(Sender: TObject);
begin
  MessageLabel.Visible := False;
end;

end.

