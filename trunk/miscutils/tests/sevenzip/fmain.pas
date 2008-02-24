unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, Menus, SevenZipWrapper;

type

  { TFormMain }

  TFormMain = class(TForm)
    EditFileName: TFileNameEdit;
    Label1: TLabel;
    ListViewFiles: TListView;
    MainMenu1: TMainMenu;
    MIOptions: TMenuItem;
    procedure EditFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIOptionsClick(Sender: TObject);
  private
    procedure UpdateExecutable;
    { private declarations }
  public
    { public declarations }
    Reader: TSevenZipReader;
  end; 

var
  FormMain: TFormMain;

implementation

uses
  fEditExecutable, IniFiles;
  
{ TFormMain }

procedure TFormMain.MIOptionsClick(Sender: TObject);
begin
  with TFormEditExecutable.Create(Self) do
  try
    ShowModal;
    if ModalResult = mrYes then
      UpdateExecutable;
  finally
    Destroy;
  end;
end;

procedure TFormMain.UpdateExecutable;
begin
  with TIniFile.Create('sevenzip.ini') do
  try
    Reader.Executable := ReadString('options', 'exepath', '');
  finally
    Destroy;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Reader := TSevenZipReader.Create;
  UpdateExecutable;
end;

procedure TFormMain.EditFileNameAcceptFileName(Sender: TObject;
  var Value: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  Reader.FileName := Value;
  Reader.Load;
  with ListViewFiles do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Reader.PackedFiles.Count - 1do
    begin
      ListItem := Items.Add;
      ListItem.Caption := Reader.PackedFiles[i].Path;
      ListItem.SubItems.Add(IntToStr(Reader.PackedFiles[i].Size));
      ListItem.SubItems.Add(IntToStr(Reader.PackedFiles[i].PackedSize));
      ListItem.SubItems.Add(Reader.PackedFiles[i].CRC);
    end;
    EndUpdate;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Reader.Destroy;
end;

initialization
  {$I fmain.lrs}

end.

