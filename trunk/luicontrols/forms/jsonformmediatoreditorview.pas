unit JSONFormMediatorEditorView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, AdvancedLabel, VirtualTrees, JSONFormMediator;

type

  { TJSONFormMediatorEditorViewForm }

  TJSONFormMediatorEditorViewForm = class(TForm)
    AddElementButton: TBitBtn;
    ImportFromControlsLabel: TAdvancedLabel;
    DeleteElementLabel: TAdvancedLabel;
    ControlComboBox: TComboBox;
    ImportFromModelsLabel: TAdvancedLabel;
    Label1: TLabel;
    PropertyNameEdit: TLabeledEdit;
    PageControl1: TPageControl;
    ElementsTabSheet: TTabSheet;
    ElementListView: TVirtualStringTree;
    CaptionEdit: TLabeledEdit;
    NameEdit: TLabeledEdit;
    procedure DeleteElementLabelClick(Sender: TObject);
    procedure ImportFromControlsLabelClick(Sender: TObject);
    procedure ImportFromModelsLabelClick(Sender: TObject);
  private
    FMediator: TJSONFormMediator;
    { private declarations }
  public
    { public declarations }
    property Mediator: TJSONFormMediator read FMediator write FMediator;
  end;

var
  JSONFormMediatorEditorViewForm: TJSONFormMediatorEditorViewForm;

implementation

uses
  JSONFormMediatorImportControlsView;

{$R *.lfm}

{ TJSONFormMediatorEditorViewForm }

procedure TJSONFormMediatorEditorViewForm.ImportFromModelsLabelClick(
  Sender: TObject);
begin
end;

procedure TJSONFormMediatorEditorViewForm.DeleteElementLabelClick(
  Sender: TObject);
begin

end;

procedure TJSONFormMediatorEditorViewForm.ImportFromControlsLabelClick(
  Sender: TObject);
var
  ImportForm: TJSONFormMediatorImportControlsViewForm;
begin
  ImportForm := TJSONFormMediatorImportControlsViewForm.Create(Self);
  try
    ImportForm.Mediator := Mediator;
    ImportForm.TopLevelControl := Mediator.Owner as TWinControl;
    ImportForm.ShowModal;
  finally
    ImportForm.Destroy;
  end;
end;

end.

