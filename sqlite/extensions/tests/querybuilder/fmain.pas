unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DbCtrls, StdCtrls, ExtCtrls, SqliteUtils, Sqlite3DS, db;

type

  { TMainForm }

  TMainForm = class(TForm)
    ReloadButton: TButton;
    ButtonSaveChanges: TButton;
    CompoundKeyCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    MainExtraDatasource: TDatasource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    ExtraDatasource: TDatasource;
    MainDatasource: TDatasource;
    MainDataset: TSqlite3Dataset;
    ExtraDataset: TSqlite3Dataset;
    MainExtraDataset: TSqlite3Dataset;
    SQLMemo: TMemo;
    QueryBuilder: TSqliteQueryBuilder;
    TablesRadioGroup: TRadioGroup;
    procedure ButtonSaveChangesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
  private
    procedure ReloadData;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainDataset.Open;
  ExtraDataset.Open;
  MainExtraDataset.SQL := 'Select Main.Id as PersonId, Extra.Id as ExtraId, Age, Name from Main, Extra where Main.Id = Extra.MainId';
  MainExtraDataset.Open;
end;

procedure TMainForm.ReloadButtonClick(Sender: TObject);
begin
  ReloadData;
end;

procedure TMainForm.ReloadData;
begin
  MainDataset.Close;
  MainDataset.Open;

  ExtraDataset.Close;
  ExtraDataset.Open;

  MainExtraDataset.Close;
  MainExtraDataset.Open
end;

procedure TMainForm.ButtonSaveChangesClick(Sender: TObject);
begin
  if CompoundKeyCheckBox.Checked then
    QueryBuilder.TableDefs[0].PrimaryKey := 'PersonId=Id,Name'
  else
    QueryBuilder.TableDefs[0].PrimaryKey := 'PersonId=Id';
  case TablesRadioGroup.ItemIndex of
    0: QueryBuilder.BuildSQL('Main');
    1: QueryBuilder.BuildSQL('Extra');
    2: QueryBuilder.BuildSQL;
  end;
  SQLMemo.Lines.Assign(QueryBuilder.SQL);
  try
    MainExtraDataset.ExecSQL(QueryBuilder.SQL);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  MainExtraDataset.ClearUpdates;
  ReloadData;
end;

end.

