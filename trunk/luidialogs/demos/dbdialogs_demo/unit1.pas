unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, ExtCtrls, db, Sqlite3DS;

type

  { TMainForm }

  TMainForm = class(TForm)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    EditDatasetButton: TButton;
    OptionsRadioGroup: TRadioGroup;
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

const
  OnlyName = '"Name"';
  NameCustomTitle = '{"title": "Edit Test Dataset", "fields": {"name": "Name", "title": "Person"}}';
  IdName = '["Id", "Name"]';
  IdNameCustomWidth = '{"title": "Edit Test Dataset",'+
    '"fields": [{"name": "Id", "width": 100}, {"name": "Name", "width": 200}]}';
  AllCustomMixed = '{"title": "Edit Test Dataset",'+
    '"fields": ["Id", {"name": "Name", "title": "Person"}, { "name": "Phone", "width": 100}]}';

procedure TMainForm.EditDatasetButtonClick(Sender: TObject);
var
  DialogInfo: String;
begin
  case OptionsRadioGroup.ItemIndex of
    0: DialogInfo := OnlyName;
    1: DialogInfo := NameCustomTitle;
    2: DialogInfo := IdName;
    3: DialogInfo := IdNameCustomWidth;
    4: DialogInfo := AllCustomMixed;
  end;
  EditDataSet(Sqlite3Dataset1, Self, DialogInfo);
end;

end.

