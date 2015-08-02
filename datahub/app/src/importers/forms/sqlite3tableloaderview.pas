unit Sqlite3TableLoaderView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, Sqlite3DS, DataModel, DataModelImporter;

type

  { TSqlite3TableLoaderViewForm }

  TSqlite3TableLoaderViewForm = class(TForm)
    FileNameEdit: TFileNameEdit;
    Label2: TLabel;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    Label1: TLabel;
    Sqlite3Dataset: TSqlite3Dataset;
    TablesListBox: TListBox;
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure OkButtonClick(Sender: TObject);
    procedure TablesListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FModel: TDataModel;
    { private declarations }
  public
    { public declarations }
  published
    property Model: TDataModel write FModel;
  end; 


implementation

{$R *.lfm}

{ TSqlite3TableLoaderViewForm }

procedure TSqlite3TableLoaderViewForm.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  TablesListBox.Clear;
  OkButton.Enabled := False;
  Sqlite3Dataset.Close;
  Sqlite3Dataset.FileName := Value;
  try
    Sqlite3Dataset.QuickQuery('SELECT name FROM SQLITE_MASTER WHERE type = ''table''', TablesListBox.Items);
  except
    ShowMessage(Format('Unable to not open "%s"', [Value]));
  end;
end;

procedure TSqlite3TableLoaderViewForm.OkButtonClick(Sender: TObject);
begin
  Sqlite3Dataset.Close;
  Sqlite3Dataset.Sql := 'Select * from ' + TablesListBox.Items[TablesListBox.ItemIndex] + ' Where 1 = 0';
  Sqlite3Dataset.Open;
  DatasetToDataModel(Sqlite3Dataset, FModel);
end;

procedure TSqlite3TableLoaderViewForm.TablesListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  OkButton.Enabled := (TablesListBox.ItemIndex <> -1);
end;

end.

