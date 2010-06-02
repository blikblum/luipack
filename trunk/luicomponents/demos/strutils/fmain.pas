unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CapitalizeButton: TButton;
    CapitalizeTextEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainPageControl: TPageControl;
    CapitalizeTabSheet: TTabSheet;
    CapitalizeResultsMemo: TMemo;
    CapitalizeExcludeMemo: TMemo;
    procedure CapitalizeButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  LuiStrUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CapitalizeButtonClick(Sender: TObject);
var
  ExcludeWords: array of UnicodeString;
  i: Integer;
begin
  if Trim(CapitalizeTextEdit.Text) = '' then
    Exit;
  SetLength(ExcludeWords, CapitalizeExcludeMemo.Lines.Count);
  for i := 0 to CapitalizeExcludeMemo.Lines.Count - 1 do
    ExcludeWords[i] := CapitalizeExcludeMemo.Lines[i];
  CapitalizeResultsMemo.Lines.Add(Capitalize(CapitalizeTextEdit.Text, ExcludeWords));
end;

end.

