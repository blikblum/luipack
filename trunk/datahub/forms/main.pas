unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, DataModel, DataHubProject, VirtualTrees,
  VTCollectionController, ModelFieldsView;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionsImageList: TImageList;
    ModelsTreeView: TVirtualStringTree;
    MainToolBar: TToolBar;
    OpenDialog1: TOpenDialog;
    OpenProjectFileButton: TToolButton;
    NewProjectlButton: TToolButton;
    ImportPopupMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveProjectButton: TToolButton;
    ToolButton1: TToolButton;
    AddModelButton: TToolButton;
    ImportModelButton: TToolButton;
    ToolButton2: TToolButton;
    AddViewButton: TToolButton;
    ViewsTreeView: TVirtualStringTree;
    procedure AddModelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportModelButtonClick(Sender: TObject);
    procedure ModelsTreeViewDblClick(Sender: TObject);
    procedure ModelsTreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure NewProjectlButtonClick(Sender: TObject);
    procedure OpenProjectFileButtonClick(Sender: TObject);
    procedure SaveProjectButtonClick(Sender: TObject);
  private
    FProject: TDataHubProject;
    FModelsController: TCollectionVirtualTreeController;
    FModelFieldsView: TModelFieldsViewFrame;
    FViewsController: TCollectionVirtualTreeController;
    procedure ImportMenuItemClick(Sender: TObject);
    procedure LoadProjectFromFile(const FileName: String);
    procedure SaveProject;
    procedure SaveProjectToFile(const FileName: String);
    procedure UpdateModelsTreeView;
    procedure UpdateViewsTreeView;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  ModelEditor, DataHubLCLUtils, DataModelImporter;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FProject := TDataHubProject.Create(Self);
  FModelsController := TCollectionVirtualTreeController.Create(Self);
  FModelsController.Collection := FProject.Models;
  FModelsController.Tree := ModelsTreeView;
  FModelsController.DefaultProperty := 'Name';
  FModelFieldsView := TModelFieldsViewFrame.Create(Self);
  FModelFieldsView.Parent := Self;
  FModelFieldsView.AnchorToNeighbour(akLeft, 2,ModelsTreeView);
  FModelFieldsView.AnchorParallel(akTop, -10, ModelsTreeView);
  FModelFieldsView.Project := FProject;
  FViewsController := TCollectionVirtualTreeController.Create(Self);
  FViewsController.Tree := ViewsTreeView;
  FViewsController.DefaultProperty := 'Name';
  BuildImporterMenu(ImportPopupMenu, @ImportMenuItemClick);
  //FViewsController.Collection := FProject;
end;

procedure TMainForm.ImportModelButtonClick(Sender: TObject);
begin
  //todo: make a wizard interface
end;

procedure TMainForm.ModelsTreeViewDblClick(Sender: TObject);
var
  Item: TDataModel;
begin
  Item := FModelsController.GetCollectionItem(ModelsTreeView.FocusedNode) as TDataModel;
  if Item <> nil then
  begin
    Item.Name := InputBox('Rename Model', 'New Name', Item.Name);
    FModelsController.Load;
  end;
end;

procedure TMainForm.AddModelButtonClick(Sender: TObject);
begin
  if TModelEditorForm.AddModel(FProject.Models) then
    UpdateModelsTreeView;
end;

procedure TMainForm.ModelsTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Item: TDataModel;
begin
  Item := FModelsController.GetCollectionItem(Node) as TDataModel;
  if Item <> nil then
    FModelFieldsView.Fields := Item.Fields;
end;

procedure TMainForm.NewProjectlButtonClick(Sender: TObject);
var
  NewProject: TDataHubProject;
begin
  //todo: check need to save
  NewProject := TDataHubProject.Create(nil);
  try
    FProject.Assign(NewProject);
  finally
    NewProject.Destroy;
  end;
  UpdateModelsTreeView;
end;

procedure TMainForm.OpenProjectFileButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadProjectFromFile(OpenDialog1.FileName);
end;

procedure TMainForm.SaveProjectButtonClick(Sender: TObject);
begin
  if FProject.FileName = '' then
  begin
    if SaveDialog1.Execute then
    begin
      FProject.FileName := SaveDialog1.FileName;
      FProject.Save;
    end;
  end
  else
    FProject.Save;
end;

procedure TMainForm.ImportMenuItemClick(Sender: TObject);
var
  ImporterClass: TDataModelImporterClass;
  MenuItem: TMenuItem absolute Sender;
begin
  if not (Sender is TMenuItem) then
    Exit;
  ImporterClass := TDataModelImporterClass(MenuItem.Tag);
  ImporterClass.Execute(FProject.Models);
  UpdateModelsTreeView;
end;

procedure TMainForm.LoadProjectFromFile(const FileName: String);
var
  SavedProject: TDataHubProject;
begin
  SavedProject := TDataHubProject.Create(nil);
  try
    SavedProject.Assign(FProject);
    try
      FProject.FileName := FileName;
      FProject.Load;
    except
      on E: Exception do
      begin
        ShowMessage('Error Loading Project: ' + E.Message);
        FProject.Assign(SavedProject);
      end;
    end;
  finally
    SavedProject.Destroy;
  end;
  UpdateModelsTreeView;
end;

procedure TMainForm.SaveProject;
begin
  if FProject.FileName <> '' then
  begin
    FProject.Save;
  end
  else
  begin

  end;
end;

procedure TMainForm.SaveProjectToFile(const FileName: String);
begin
  if SaveDialog1.Execute then
  begin
    //todo
  end;
end;

procedure TMainForm.UpdateModelsTreeView;
begin
  FModelsController.Load;
end;

procedure TMainForm.UpdateViewsTreeView;
begin
  FViewsController.Load;
end;

end.

