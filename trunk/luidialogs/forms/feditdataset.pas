unit fEditDataSet;

{$mode objfpc}{$H+}
//todo: add proper internacionalization code
{$define USE_PORTUGUESE}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, VirtualDBGrid, db, LuiDBDialogs, VirtualTrees;

type

  { TEditDataSourceForm }

  TEditDataSourceForm = class(TForm)
    AddButton: TBitBtn;
    DataSource1: TDatasource;
    DeleteButton: TBitBtn;
    CloseButton: TBitBtn;
    EditButton: TBitBtn;
    Grid: TVirtualDBGrid;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  private
    FAppendEditorClass: TFormClass;
    FAppendEditorForm: TForm;
    FModifications: TDataModifications;
    FEditorClass: TFormClass;
    FEditorForm: TForm;
    procedure SetAppendEditorClassName(const Value: String);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetEditorClassName(const Value: String);
  public
    property AppendEditorClassName: String write SetAppendEditorClassName;
    property DataSet: TDataSet write SetDataSet;
    property EditorClassName: String write SetEditorClassName;
    property Modifications: TDataModifications read FModifications;
  end;

implementation

uses
  LuiRTTIUtils;

{$R *.lfm}

var
  strAddRecord: String = 'Add';
  strEditRecord: String = 'Edit';
  strDeleteRecord: String = 'Remove';
  strCloseDialog: String = 'Close';


procedure LoadPortugueseStrings;
begin
  strAddRecord := 'Adicionar';
  strEditRecord := 'Editar';
  strDeleteRecord := 'Excluir';
  strCloseDialog := 'Fechar';
end;

{ TEditDataSourceForm }

procedure TEditDataSourceForm.AddButtonClick(Sender: TObject);
var
  LastNode: PVirtualNode;
begin
  with DataSource1.DataSet do
  begin
    Append;
    Post;
    if (FAppendEditorForm = nil) and (FAppendEditorClass <> nil) then
    begin
      FAppendEditorForm := FAppendEditorClass.Create(Self);
      SetObjectProperties(FAppendEditorForm, ['DataSource', DataSource1, 'Dataset', DataSource1.DataSet]);
    end;
    if FAppendEditorForm <> nil then
    begin
      if (FAppendEditorForm.ShowModal in [mrOK, mrYes]) then
        FModifications := FModifications + [dmAdd]
      else
      begin
        Delete;
      end;
    end
    else
    begin
      FModifications := FModifications + [dmAdd];
      LastNode := Grid.GetLast;
      if LastNode <> nil then
        Grid.EditNode(LastNode, 0);
    end;
  end;
end;

procedure TEditDataSourceForm.DeleteButtonClick(Sender: TObject);
begin
  with DataSource1.DataSet do
  begin
    if not IsEmpty then
    begin
      Delete;
      FModifications := FModifications + [dmDelete];
    end;
  end;
end;

procedure TEditDataSourceForm.EditButtonClick(Sender: TObject);
begin
  if FEditorForm = nil then
  begin
    FEditorForm := FEditorClass.Create(Self);
    SetObjectProperties(FEditorForm, ['DataSource', DataSource1, 'Dataset', DataSource1.DataSet]);
  end;
  FEditorForm.ShowModal;
end;

procedure TEditDataSourceForm.FormCreate(Sender: TObject);
begin
  AddButton.Caption := strAddRecord;
  DeleteButton.Caption := strDeleteRecord;
  EditButton.Caption := strEditRecord;
  CloseButton.Caption := strCloseDialog;
end;

procedure TEditDataSourceForm.GridEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FModifications := FModifications + [dmUpdate];
end;

procedure TEditDataSourceForm.SetDataSet(const Value: TDataSet);
begin
  DataSource1.DataSet := Value;
end;

procedure TEditDataSourceForm.SetAppendEditorClassName(const Value: String);
var
  AClass: TClass;
begin
  if Value <> '' then
  begin
    AClass := FindClass(Value);
    if AClass.InheritsFrom(TForm) then
    begin
      FAppendEditorClass := TFormClass(AClass);
    end;
  end;
end;

procedure TEditDataSourceForm.SetEditorClassName(const Value: String);
var
  AClass: TClass;
begin
  if Value <> '' then
  begin
    AClass := FindClass(Value);
    if AClass.InheritsFrom(TForm) then
    begin
      FEditorClass := TFormClass(AClass);
      EditButton.Visible := True;
    end;
  end;
end;

initialization
  {$ifdef USE_PORTUGUESE}
  LoadPortugueseStrings;
  {$endif}

end.

