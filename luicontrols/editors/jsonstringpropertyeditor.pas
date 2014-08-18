unit JSONStringPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, PropEdits, Controls;

type

  { TJSONDataStringPropertyEditor }

  TJSONDataStringPropertyEditor = class(TStringPropertyEditor)
  protected
    function GetJSONType: TJSONtype; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TJSONObjectStringPropertyEditor }

  TJSONObjectStringPropertyEditor = class(TJSONDataStringPropertyEditor)
  protected
    function GetJSONType: TJSONtype; override;
  end;

  { TJSONArrayStringPropertyEditor }

  TJSONArrayStringPropertyEditor = class(TJSONDataStringPropertyEditor)
  protected
    function GetJSONType: TJSONtype; override;
  end;

implementation

uses
  JSONStringPropertyEditorView;

{ TJSONObjectStringPropertyEditor }

function TJSONObjectStringPropertyEditor.GetJSONType: TJSONtype;
begin
  Result := jtObject;
end;

{ TJSONArrayStringPropertyEditor }

function TJSONArrayStringPropertyEditor.GetJSONType: TJSONtype;
begin
  Result := jtArray;
end;

{ TJSONDataStringPropertyEditor }

function TJSONDataStringPropertyEditor.GetJSONType: TJSONtype;
begin
  Result := jtUnknown;
end;

procedure TJSONDataStringPropertyEditor.Edit;
var
  Form: TJSONStringPropertyEditorForm;
begin
  Form := TJSONStringPropertyEditorForm.Create(nil);
  try
    Form.JSONType := GetJSONType;
    Form.JSONString := GetStrValue;
    if Form.ShowModal = mrOK then
      SetStrValue(Form.JSONString);
  finally
    Form.Free;
  end;
end;

function TJSONDataStringPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paAutoUpdate];
end;

end.

