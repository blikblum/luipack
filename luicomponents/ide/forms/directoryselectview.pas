unit DirectorySelectView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons;

type

  { TDirectorySelectForm }

  TDirectorySelectForm = class(TForm)
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    DirectoryEdit: TDirectoryEdit;
    DirLabel: TLabel;
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
  private
    FDirectory: String;
    procedure SetDirectory(const AValue: String);
    { private declarations }
    procedure ValidateDirectory;
  public
    { public declarations }
    property Directory: String read FDirectory write SetDirectory;
  end;

implementation

{$R *.lfm}

{ TDirectorySelectForm }

procedure TDirectorySelectForm.FormShow(Sender: TObject);
begin
  ValidateDirectory;
end;

procedure TDirectorySelectForm.DirectoryEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  FDirectory := Value;
  ValidateDirectory;
end;

procedure TDirectorySelectForm.SetDirectory(const AValue: String);
begin
  FDirectory := AValue;
  DirectoryEdit.Text := FDirectory;
end;

procedure TDirectorySelectForm.ValidateDirectory;
begin
  SaveButton.Enabled := DirectoryExistsUTF8(FDirectory);
end;

end.

