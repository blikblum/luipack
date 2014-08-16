unit JSONStringPropertyEditorView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterJScript, SynMemo, Forms,
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
    function GetJSONString: TJSONStringType;
    function LoadJSONString(const Value: TJSONStringType): Boolean;
    procedure SetJSONString(const Value: TJSONStringType);
  public
    { public declarations }
    property JSONString: TJSONStringType read GetJSONString write SetJSONString;
  end;

var
  JSONStringPropertyEditorForm: TJSONStringPropertyEditorForm;

implementation

uses
  jsonparser;

{$R *.lfm}

{ TJSONStringPropertyEditorForm }

procedure TJSONStringPropertyEditorForm.OKButtonClick(Sender: TObject);
begin
  if LoadJSONString(JSONStringEdit.Lines.Text) then
    ModalResult := mrOK;
end;

function TJSONStringPropertyEditorForm.GetJSONString: TJSONStringType;
begin
  if FData <> nil then
    Result := FData.AsJSON
  else
    Result := JSONStringEdit.Lines.Text;
end;

function TJSONStringPropertyEditorForm.LoadJSONString(
  const Value: TJSONStringType): Boolean;
var
  Parser: TJSONParser;
begin
  Result := True;
  //validate
  Parser := TJSONParser.Create(Value, True);
  try
    FreeAndNil(FData);
    //todo: see why parser does not raise an exception with Value = 'ddd'
    FData := Parser.Parse;
  except
    on E: Exception do
    begin
      Result := False;
      MessageLabel.Visible := True;
      MessageLabel.Caption := E.Message;
    end;
  end;
end;

procedure TJSONStringPropertyEditorForm.SetJSONString(
  const Value: TJSONStringType);
begin
  if LoadJSONString(Value) and (FData <> nil) then
    JSONStringEdit.Lines.Text := FData.FormatJSON
  else
    JSONStringEdit.Lines.Text := Value;
end;

procedure TJSONStringPropertyEditorForm.FormCreate(Sender: TObject);
begin
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

