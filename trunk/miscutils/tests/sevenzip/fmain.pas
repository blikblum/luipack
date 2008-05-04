unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, Menus, SevenZipWrapper, IniFiles;

type

  { TFormMain }

  TFormMain = class(TForm)
    CheckGetDetails: TCheckBox;
    EditFileName: TFileNameEdit;
    Label1: TLabel;
    ListViewFiles: TListView;
    MainMenu1: TMainMenu;
    MIExtract: TMenuItem;
    MIOptions: TMenuItem;
    PopupFiles: TPopupMenu;
    SelectDirectory: TSelectDirectoryDialog;
    procedure CheckGetDetailsChange(Sender: TObject);
    procedure EditFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIExtractClick(Sender: TObject);
    procedure MIOptionsClick(Sender: TObject);
    procedure PopupFilesPopup(Sender: TObject);
  private
    procedure UpdateExecutable;
    { private declarations }
  public
    { public declarations }
    Reader: TSevenZipReader;
    Ini: TIniFile;
  end; 

var
  FormMain: TFormMain;

implementation

uses
  fEditExecutable;
  
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

procedure TFormMain.PopupFilesPopup(Sender: TObject);
begin
  MIExtract.Enabled := ListViewFiles.SelCount > 0;
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
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'sevenzip.ini');
  EditFileName.InitialDir := Ini.ReadString('options', 'lastfiledir', '');
  UpdateExecutable;
end;

procedure TFormMain.EditFileNameAcceptFileName(Sender: TObject;
  var Value: String);
var
  i: Integer;
  ListItem: TListItem;
begin
  Ini.WriteString('options', 'lastfiledir', ExtractFileDir(Value));
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

procedure TFormMain.CheckGetDetailsChange(Sender: TObject);
begin
  if CheckGetDetails.Checked then
    Reader.Options := Reader.Options + [szoGetDetails]
  else
    Reader.Options := Reader.Options - [szoGetDetails];
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Reader.Destroy;
  Ini.Destroy;
end;

procedure TFormMain.MIExtractClick(Sender: TObject);
begin
  if SelectDirectory.Execute then
  begin
    Reader.Extract(ListViewFiles.Selected.Index, SelectDirectory.FileName);
  end;
end;

initialization
  {$I fmain.lrs}

end.

