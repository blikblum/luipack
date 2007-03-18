unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, sqlite3wrapper,
  StdCtrls, Grids, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButOpenFile: TButton;
    LabelRecordCount: TLabel;
    ListTables: TListBox;
    GridTableData: TStringGrid;
    OpenDialog1: TOpenDialog;
    procedure ButOpenFileClick(Sender: TObject);
    procedure ListTablesSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FConnection: TSqlite3Connection;
    procedure OpenDatabase(const AFileName: String);
    procedure ShowTableData(const TableName: String);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ButOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenDatabase(OpenDialog1.FileName);
end;

procedure TForm1.ListTablesSelectionChange(Sender: TObject; User: boolean);
begin
  with ListTables do
    if ItemIndex <> -1 then
      ShowTableData(Items[ItemIndex]);
end;

procedure TForm1.OpenDatabase(const AFileName: String);
var
  TableListReader: TSqlite3DataReader;
begin
  //Create the connection
  if FConnection = nil then
    FConnection:= TSqlite3Connection.Create(Self);
  with FConnection do
  begin
    FileName:=AFileName;
    if not Open then
    begin
      ShowMessage('Error opening "'+AFileName+'" '+ ReturnString);
      Exit;
    end;
    //Get all tables in database
    TableListReader:=TSqlite3DataReader.Create;
    try
      FConnection.Prepare('Select Name from sqlite_master where type = "table"',
        TableListReader);
      ListTables.Clear;
      with TableListReader do
      begin
        while Step do
          ListTables.Items.Add(GetString(0));
        //finalize is optional if only used once since destroy calls it
        //Finalize;
      end;
    finally
      TableListReader.Destroy;
    end;
  end;
end;

procedure TForm1.ShowTableData(const TableName: String);
var
  TableDataReader: TSqlite3DataReader;
  i,j: Integer;
begin
  try
    //get the record count
    LabelRecordCount.Caption:='Record Count: ' +
      FConnection.Query('Select Count(*) from '+TableName).GetString;
    TableDataReader:=TSqlite3DataReader.Create;
    try
      //prepare the datareader
      FConnection.Prepare('Select * from '+TableName,TableDataReader);
      with GridTableData, TableDataReader do
      begin
        //reset the grid
        RowCount:=1;
        ColCount:=FieldCount;
        //get the field names
        for i:= 0 to FieldCount - 1 do
          Cells[i,0]:=FieldNames[i];
        //browse the data
        while Step do
        begin
          //add a row
          j:=RowCount;
          RowCount:=Succ(j);
          //fill the row with the values
          for i:= 0 to FieldCount - 1 do
            Cells[i,j]:=GetString(i);
        end;
        //finalize the reader - optional
        Finalize;
      end;
    finally
      TableDataReader.Destroy;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

initialization
  {$I fmain.lrs}

end.

