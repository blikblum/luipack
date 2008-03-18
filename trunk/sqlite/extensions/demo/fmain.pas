unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, sqlite3wrapper,
  StdCtrls, Grids, Buttons, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    FConnection: TSqlite3Connection;
    procedure OpenDatabase(const AFileName: String);
    procedure ShowData(const Sql: String);
    procedure ExecuteSql(const Sql: String);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ButExecSqlClick(Sender: TObject);
var
  ASql: String;
begin
  ASql := EditSQL.Text;
  if Pos('SELECT ', UpperCase(ASql)) > 0 then
    ShowData(ASql)
  else
    ExecuteSql(ASql);
end;

procedure TForm1.EditFileNameAcceptFileName(Sender: TObject; var Value: String);
begin
  if FileExists(Value) then
    OpenDatabase(Value);
end;

procedure TForm1.ListTablesSelectionChange(Sender: TObject; User: boolean);
begin
  with ListTables do
    if ItemIndex <> -1 then
      ShowData('Select * from ' + Items[ItemIndex]);
end;

procedure TForm1.OpenDatabase(const AFileName: String);
var
  TableListReader: TSqlite3DataReader;
begin
  //Create the connection
  if FConnection = nil then
    FConnection := TSqlite3Connection.Create(Self);
  with FConnection do
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
    //Get all tables in database
    TableListReader := TSqlite3DataReader.Create;
    try
      FConnection.Prepare('Select Name from sqlite_master where type = "table"',
        TableListReader);
      ListTables.Clear;
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
  end;
end;

procedure TForm1.ShowData(const Sql: String);
var
  TableDataReader: TSqlite3DataReader;
  i, j: Integer;
begin
  try
    TableDataReader := TSqlite3DataReader.Create;
    try
      //prepare the datareader
      FConnection.Prepare(Sql, TableDataReader);
      with GridTableData, TableDataReader do
      begin
        //reset the grid
        RowCount := 1;
        ColCount := FieldCount;
        //get the field names
        for i:= 0 to FieldCount - 1 do
          Cells[i,0] := FieldNames[i];
        //browse the data
        while Step do
        begin
          //add a row
          j := RowCount;
          RowCount := Succ(j);
          //fill the row with the values
          for i:= 0 to FieldCount - 1 do
            Cells[i,j] := GetString(i);
        end;
        //finalize the reader - optional
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

procedure TForm1.ExecuteSql(const Sql: String);
begin
  try
    FConnection.ExecSql(Sql);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

initialization
  {$I fmain.lrs}

end.

