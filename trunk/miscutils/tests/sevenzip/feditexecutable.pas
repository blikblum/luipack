unit fEditExecutable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons;

type

  { TFormEditExecutable }

  TFormEditExecutable = class(TForm)
    ButtonSave: TBitBtn;
    EditExecutablePath: TFileNameEdit;
    Label1: TLabel;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses
  fMain;

{ TFormEditExecutable }

procedure TFormEditExecutable.ButtonSaveClick(Sender: TObject);
begin
  if EditExecutablePath.Text <> '' then
    FormMain.Ini.WriteString('options', 'exepath', EditExecutablePath.Text);
end;

procedure TFormEditExecutable.FormCreate(Sender: TObject);
begin
  EditExecutablePath.Text := FormMain.Ini.ReadString('options', 'exepath', '');
end;

initialization
  {$I feditexecutable.lrs}

end.

