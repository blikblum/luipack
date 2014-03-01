unit JSONFormMediatorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComponentEditors, LazarusPackageIntf, LazIdeIntf, JSONFormMediator;

type

  { TJSONFormMediatorEditor }

  TJSONFormMediatorEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  JSONFormMediatorEditorView;

procedure Register;
begin
   RegisterComponentEditor(TJSONFormMediator, TJSONFormMediatorEditor);
end;

{ TJSONFormMediatorEditor }

function TJSONFormMediatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TJSONFormMediatorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Mediator...';
  end;
end;

procedure TJSONFormMediatorEditor.ExecuteVerb(Index: Integer);
var
  Form: TJSONFormMediatorEditorViewForm;
begin
  case Index of
    0:
      begin
        Form := TJSONFormMediatorEditorViewForm.Create(nil);
        try
          Form.Mediator := Component as TJSONFormMediator;
          Form.ShowModal;
        finally
          Form.Destroy;
        end;
      end;
  end;
end;

end.

