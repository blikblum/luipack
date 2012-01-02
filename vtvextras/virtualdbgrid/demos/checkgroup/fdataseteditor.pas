unit fDatasetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Buttons, StdCtrls,
  VirtualDBGrid, DB;

type

  { TDatasetEditorFrame }

  TDatasetEditorFrame = class(TFrame)
    AddButton: TButton;
    DeleteButton: TButton;
    Grid: TVirtualDBGrid;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
  private
    FDataset: TDataset;
    function CheckDataset: Boolean;
  public
  end; 

implementation

{$R *.lfm}

{ TDatasetEditorFrame }

procedure TDatasetEditorFrame.AddButtonClick(Sender: TObject);
begin
  if not CheckDataset then
    Exit;
  FDataset.Append;
  FDataset.FieldByName('Name').AsString := 'New Value';
  FDataset.Post;
  Grid.EditNode(Grid.FocusedNode, 1);
end;

procedure TDatasetEditorFrame.DeleteButtonClick(Sender: TObject);
begin
  if not CheckDataset or FDataset.IsEmpty then
    Exit;
  FDataset.Delete;
end;

function TDatasetEditorFrame.CheckDataset: Boolean;
begin
  FDataset := nil;
  if Grid.DBOptions.DataSource <> nil then
    FDataset := Grid.DBOptions.DataSource.DataSet;
  Result := FDataset <> nil;
end;

end.

