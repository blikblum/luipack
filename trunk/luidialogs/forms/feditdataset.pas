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
    Grid: TVirtualDBGrid;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  private
    FModifications: TDataModifications;
    procedure SetDataSet(const Value: TDataSet);
    { private declarations }
  public
    { public declarations }
    property DataSet: TDataSet write SetDataSet;
    property Modifications: TDataModifications read FModifications;
  end;

implementation

{$R *.lfm}

var
  strAddRecord: String = 'Add';
  strDeleteRecord: String = 'Remove';
  strCloseDialog: String = 'Close';


procedure LoadPortugueseStrings;
begin
  strAddRecord := 'Adicionar';
  strDeleteRecord := 'Excluir';
  strCloseDialog := 'Fechar';
end;

{ TEditDataSourceForm }

procedure TEditDataSourceForm.AddButtonClick(Sender: TObject);
begin
  with DataSource1.DataSet do
  begin
    FModifications := FModifications + [dmAdd];
    Append;
    Post;
    with Grid do
    begin
      if GetLast <> nil then
        EditNode(GetLast, 0);
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

procedure TEditDataSourceForm.FormCreate(Sender: TObject);
begin
  AddButton.Caption := strAddRecord;
  DeleteButton.Caption := strDeleteRecord;
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

initialization
  {$ifdef USE_PORTUGUESE}
  LoadPortugueseStrings;
  {$endif}

end.

