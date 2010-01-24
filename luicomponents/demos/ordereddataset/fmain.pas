unit fMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LuiOrderedDataset, db, DBGrids, StdCtrls, Spin;

type

  { TMainForm }

  TMainForm = class(TForm)
    AppendButton: TButton;
    ChangeOrderButton: TButton;
    DeleteButton: TButton;
    PostButton: TButton;
    InsertButton: TButton;
    Grid: TDBGrid;
    OrderedDatasource: TDatasource;
    OrderedDataset: TLuiOrderedDataset;
    OrderOffsetEdit: TSpinEdit;
    procedure AppendButtonClick(Sender: TObject);
    procedure ChangeOrderButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  sqlite3, LCLProc;


procedure TraceSqlite(user: pointer; s: pchar); cdecl;
begin
  DebugLn(S);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  with OrderedDataset do
  begin
    FileName := ExtractFilePath(ParamStrUTF8(0)) + 'data.db';
    if not TableExists then
    begin
      FieldDefs.Clear;
      FieldDefs.Add('Id', ftAutoInc);
      FieldDefs.Add('OrderNo', ftInteger);
      FieldDefs.Add('Str', ftString);
      CreateTable;
    end;
    Open;
    sqlite3_trace(SqliteHandle, @TraceSqlite, nil);
  end;
end;

procedure TMainForm.InsertButtonClick(Sender: TObject);
var
  CurRecNo: Integer;
begin
  CurRecNo := OrderedDataset.RecNo;
  OrderedDataset.Insert;
  OrderedDataset.FieldByName('Str').AsString := 'String_' +
    IntToStr(CurRecNo);
end;

procedure TMainForm.PostButtonClick(Sender: TObject);
begin
  OrderedDataset.Post;
end;

procedure TMainForm.AppendButtonClick(Sender: TObject);
begin
  OrderedDataset.Append;
  OrderedDataset.FieldByName('Str').AsString := 'String_' +
    IntToStr(OrderedDataset.RecordCount + 1);
end;

procedure TMainForm.ChangeOrderButtonClick(Sender: TObject);
begin
  OrderedDataset.ChangeOrder(OrderOffsetEdit.Value);
end;

procedure TMainForm.DeleteButtonClick(Sender: TObject);
begin
  if not OrderedDataset.IsEmpty then
    OrderedDataset.Delete;
end;

initialization
  {$I fmain.lrs}

end.

