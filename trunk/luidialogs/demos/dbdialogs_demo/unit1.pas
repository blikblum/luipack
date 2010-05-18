unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, db, Sqlite3DS;

type

  { TMainForm }

  TMainForm = class(TForm)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    EditDatasetButton: TButton;
    Sqlite3Dataset1: TSqlite3Dataset;
    procedure EditDatasetButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  LuiDBDialogs;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if not Sqlite3Dataset1.TableExists then
  begin
    Sqlite3Dataset1.FieldDefs.Add('Id', ftAutoInc);
    Sqlite3Dataset1.FieldDefs.Add('Name', ftString);
    Sqlite3Dataset1.FieldDefs.Add('Phone', ftString);
    Sqlite3Dataset1.CreateTable;
  end;
  Sqlite3Dataset1.Open;
end;

procedure TMainForm.EditDatasetButtonClick(Sender: TObject);
const
  Info: TDataDialogInfo = (
    FieldNames: 'Name';
    FieldWidths: nil;
    Title: 'Edit Test Dataset'
    );
begin
  EditDataSet(Sqlite3Dataset1, Self, Info);
end;

end.

