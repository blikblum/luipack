{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jsonmediatorseditor_package;

interface

uses
  JSONFormMediatorEditor, JSONFormMediatorEditorView, 
  JSONFormMediatorImportControlsView, JSONFormMediatorImportModelsView, 
  JSONElementUpdateMethodView, JSONStringPropertyEditorView, 
  jsonstringpropertyeditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JSONFormMediatorEditor', @JSONFormMediatorEditor.Register);
end;

initialization
  RegisterPackage('jsonmediatorseditor_package', @Register);
end.
