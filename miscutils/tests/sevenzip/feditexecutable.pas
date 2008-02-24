unit fEditExecutable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, IniFiles, Buttons;

type

  { TFormEditExecutable }

  TFormEditExecutable = class(TForm)
    ButtonSave: TBitBtn;
    EditExecutablePath: TFileNameEdit;
    Label1: TLabel;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FIni: TIniFile;
  public
    { public declarations }
  end; 

implementation

{ TFormEditExecutable }

procedure TFormEditExecutable.ButtonSaveClick(Sender: TObject);
begin
  if EditExecutablePath.Text <> '' then
    FIni.WriteString('options', 'exepath', EditExecutablePath.Text);
end;

procedure TFormEditExecutable.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.Create('sevenzip.ini');
  EditExecutablePath.Text := FIni.ReadString('options', 'exepath', '');
end;

procedure TFormEditExecutable.FormDestroy(Sender: TObject);
begin
  FIni.Destroy;
end;

initialization
  {$I feditexecutable.lrs}

end.

