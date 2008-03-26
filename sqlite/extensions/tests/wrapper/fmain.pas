unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Sqlite3Wrapper,
  StdCtrls, Grids, Buttons, EditBtn;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButExecSql: TButton;
    EditSQL: TEdit;
    EditFileName: TFileNameEdit;
    Label1: TLabel;
    LabelRecordCount: TLabel;
    ListTables: TListBox;
    GridTableData: TStringGrid;
    procedure ButExecSqlClick(Sender: TObject);
    procedure EditFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure ListTablesSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FDatabase: TSqlite3Database;
    procedure OpenDatabase(const AFileName: String);
    procedure ShowData(const Sql: String);
    procedure ExecuteSql(const Sql: String);
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.ButExecSqlClick(Sender: TObject);
var
  ASql: String;
begin
  ASql := Trim(EditSQL.Text);
  if Pos('SELECT ', UpperCase(ASql)) = 1 then
    ShowData(ASql)
  else
    ExecuteSql(ASql);
end;

procedure TFormMain.EditFileNameAcceptFileName(Sender: TObject; var Value: String);
begin
  if FileExists(Value) then
    OpenDatabase(Value);
end;

procedure TFormMain.ListTablesSelectionChange(Sender: TObject; User: boolean);
begin
  with ListTables do
    if ItemIndex <> -1 then
      ShowData('Select * from ' + Items[ItemIndex]);
end;

procedure TFormMain.OpenDatabase(const AFileName: String);
{
var
  TableListReader: TSqlite3DataReader;
}
begin
  //Create the database
  if FDatabase = nil then
    FDatabase := TSqlite3Database.Create(Self);
  with FDatabase do
  begin
    Close;
    FileName := AFileName;
    try
      Open;
    except
      on E: Exception do
      begin
        ButExecSql.Enabled := False;
        ShowMessage(E.Message);
        Exit;
      end;
    end;
    ButExecSql.Enabled := True;
    //Get all tables in database using Query.GetList function
    ListTables.Clear;
    Query('Select Name from sqlite_master where type = "table"').GetList(ListTables.Items);
    //below is the way of getting using a datareader
    {
    TableListReader := TSqlite3DataReader.Create;
    try
      Prepare('Select Name from sqlite_master where type = "table"',
        TableListReader);
      with TableListReader do
      begin
        while Step do
          ListTables.Items.Add(GetString(0));
        //finalize is optional if the reader is used only once since destroy calls it
        //Finalize;
      end;
    finally
      TableListReader.Destroy;
    end;
    }
  end;
end;

procedure TFormMain.ShowData(const Sql: String);
var
  TableDataReader: TSqlite3DataReader;
  i, j: Integer;
begin
  try
    TableDataReader := TSqlite3DataReader.Create;
    try
      //prepare the datareader
      FDatabase.Prepare(Sql, TableDataReader);
      with GridTableData, TableDataReader do
      begin
        //reset the grid
        RowCount := 1;
        ColCount := FieldCount;
        //get the field names
        for i := 0 to FieldCount - 1 do
          Cells[i, 0] := FieldNames[i];
        //browse the data
        while Step do
        begin
          //add a row
          j := RowCount;
          RowCount := Succ(j);
          //fill the row with the values
          for i:= 0 to FieldCount - 1 do
            Cells[i, j] := GetString(i);
        end;
        //finalize the reader (optional)
        Finalize;
        //Set the record count
        LabelRecordCount.Caption := 'Record Count: ' + IntToStr(RowCount - 1);
      end;
    finally
      TableDataReader.Destroy;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TFormMain.ExecuteSql(const Sql: String);
begin
  try
    FDatabase.ExecSql(Sql);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

initialization
  {$I fmain.lrs}

end.

