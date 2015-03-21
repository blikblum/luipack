unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CompareButton: TButton;
    ResultLabel: TLabel;
    Str1Edit: TEdit;
    Str2Edit: TEdit;
    procedure CompareButtonClick(Sender: TObject);
    procedure Str2EditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

Uses
  LuiStrUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CompareButtonClick(Sender: TObject);
begin
  ResultLabel.Caption := Format('Result: %d', [CompareNatural(Str1Edit.Text, Str2Edit.Text)]);
end;

procedure TMainForm.Str2EditChange(Sender: TObject);
begin
  ResultLabel.Caption := 'Result: --';
end;

end.

