unit ModelFieldsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Menus, VirtualTrees,
  DataModel, DataView, DataHubProject, MenuButton, VTCollectionController;

type

  { TModelFieldsViewFrame }

  TModelFieldsViewFrame = class(TFrame)
    AddModelFieldButton: TButton;
    SelectAllButton: TButton;
    DeselectAllButton: TButton;
    Label1: TLabel;
    FieldsTreeView: TVirtualStringTree;
    ExportModelButton: TMenuButton;
    ExportMenu: TPopupMenu;
    procedure AddModelFieldButtonClick(Sender: TObject);
    procedure DeselectAllButtonClick(Sender: TObject);
    procedure FieldsTreeViewDblClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
  private
    FFields: TDataModelFields;
    FFieldsController: TCollectionVirtualTreeController;
    FProject: TDataHubProject;
    procedure CreateFieldsCollection(var NewCollection: TCollection);
    procedure SetFields(Value: TDataModelFields);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateFields;
    property Fields: TDataModelFields read FFields write SetFields;
    property Project: TDataHubProject read FProject write FProject;
  end;

implementation

uses
  ModelFieldEditor, DataViewExporter, DataHubLCLUtils;


{$R *.lfm}

{ TModelFieldsViewFrame }

procedure TModelFieldsViewFrame.FieldsTreeViewDblClick(Sender: TObject);
var
  Field: TDataModelField;
  Node: PVirtualNode;
begin
  Node := FieldsTreeView.HotNode;
  Field := TDataModelField(FFieldsController.GetCollectionItem(Node));
  if Field <> nil then
  begin
    if TModelFieldEditorForm.EditField(Field) then
      FieldsTreeView.InvalidateNode(Node);
  end;
end;

procedure TModelFieldsViewFrame.ExportMenuItemClick(Sender: TObject);
var
  ExporterClass: TDataViewExporterClass;
  View: TDataView;
  MenuItem: TMenuItem absolute Sender;
begin
  if not (Sender is TMenuItem) then
    Exit;
  ExporterClass := TDataViewExporterClass(MenuItem.Tag);
  View := TDataView.Create(Project as IFieldResolver);
  try
    View.Fields.Assign(FFieldsController.CheckedCollection);
    ExporterClass.Execute(View);
  finally
    View.Destroy;
  end;
end;

procedure VTCheckAll(Tree: TBaseVirtualTree; Checked: Boolean);
const
  BooleanCheckMap: array[Boolean] of TCheckState = (csUncheckedNormal, csCheckedNormal);
var
  Node: PVirtualNode;
begin
  Node := Tree.GetFirst;
  while Node <> nil do
  begin
    Tree.CheckState[Node] := BooleanCheckMap[Checked];
    Node := Tree.GetNext(Node);
  end;
end;

procedure TModelFieldsViewFrame.SelectAllButtonClick(Sender: TObject);
begin
  VTCheckAll(FieldsTreeView, True);
end;

procedure TModelFieldsViewFrame.CreateFieldsCollection(
  var NewCollection: TCollection);
begin
  NewCollection := TDataModelFields.Create(nil);
end;

procedure TModelFieldsViewFrame.AddModelFieldButtonClick(Sender: TObject);
begin
  if (Fields <> nil) and TModelFieldEditorForm.AddField(Fields) then
    FFieldsController.Load;
end;

procedure TModelFieldsViewFrame.DeselectAllButtonClick(Sender: TObject);
begin
  VTCheckAll(FieldsTreeView, False);
end;

procedure TModelFieldsViewFrame.SetFields(Value: TDataModelFields);
begin
  if FFields = Value then Exit;
  FFields := Value;
  FFieldsController.Collection := Value;
  FFieldsController.Load;
end;

constructor TModelFieldsViewFrame.Create(TheOwner: TComponent);
const
  Properties: array[0..2] of String = ('FieldName', 'DisplayLabel', 'FieldTypeName');
begin
  inherited Create(TheOwner);
  FFieldsController := TCollectionVirtualTreeController.Create(Self);
  FFieldsController.Tree := FieldsTreeView;
  FFieldsController.PropertyNames := Properties;
  FFieldsController.OnCreateCheckedCollection := @CreateFieldsCollection;
  BuildExporterMenu(ExportMenu, @ExportMenuItemClick);
end;

procedure TModelFieldsViewFrame.UpdateFields;
begin
  FFieldsController.Load;
end;

end.

