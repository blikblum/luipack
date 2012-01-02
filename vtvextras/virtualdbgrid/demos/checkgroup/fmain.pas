unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Sqlite3DS, db, VirtualDBCheckGroup, fDatasetEditor;

type

  { TMainForm }

  TMainForm = class(TForm)
    EditTagsButton: TButton;
    DatasetEditorFrame1: TDatasetEditorFrame;
    ItemTagsDatasetId: TAutoIncField;
    ItemTagsDatasetItem: TLongintField;
    ItemTagsDatasetTag: TLongintField;
    NameEdit: TDBEdit;
    Label2: TLabel;
    TagsLabel: TLabel;
    DetailsGroupBox: TGroupBox;
    ItemsDatasource: TDatasource;
    Label1: TLabel;
    TagsDatasource: TDatasource;
    ItemTagsDatasource: TDatasource;
    ItemsDataset: TSqlite3Dataset;
    ItemsDatasetId: TAutoIncField;
    ItemsDatasetName: TStringField;
    TagsDataset: TSqlite3Dataset;
    ItemTagsDataset: TSqlite3Dataset;
    TagsDatasetId: TAutoIncField;
    TagsDatasetName: TStringField;
    procedure EditTagsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDBCheckGroup: TVirtualDBCheckGroup;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  fTagsEditor;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDBCheckGroup := TVirtualDBCheckGroup.Create(Self);
  FDBCheckGroup.Parent := DetailsGroupBox;
  FDBCheckGroup.AnchorToNeighbour(akTop, 2, TagsLabel);
  FDBCheckGroup.AnchorParallel(akLeft, 0, TagsLabel);
  FDBCheckGroup.AddDBColumn('Name', 'Name', 180);
  FDBCheckGroup.DBOptions.DataSource := TagsDatasource;
  FDBCheckGroup.CheckSource := ItemTagsDatasource;
  FDBCheckGroup.KeyField := 'Id';
  FDBCheckGroup.CheckField := 'Tag';
  TagsDataset.Open;
  ItemsDataset.Open;
  ItemTagsDataset.Open;
end;

procedure TMainForm.EditTagsButtonClick(Sender: TObject);
begin
  with TTagsEditorForm.Create(nil) do
  try
    ShowModal;
    TagsDataset.ApplyUpdates;
  finally
    Destroy;
  end;
end;

end.

