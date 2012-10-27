unit fMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, DropDownManager, DropDownButton;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    DropDownButton1: TDropDownButton;
    DropDownWindow1: TDropDownManager;
    Edit1: TEdit;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    StandaloneTab: TTabSheet;
    SpecializedTab: TTabSheet;
    ToggleBox1: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure DropDownWindow1Hide(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DropDownWindow1Hide(Sender: TObject);
begin
 ToggleBox1.Checked := False;
end;

procedure TMainForm.RadioGroup1Click(Sender: TObject);
begin
  DropDownButton1.Caption := RadioGroup1.Items[RadioGroup1.ItemIndex];
  DropDownButton1.DropDown.Visible := False;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  DropDownWindow1.Visible := False;
end;

procedure TMainForm.ToggleBox1Change(Sender: TObject);
begin
  DropDownWindow1.Visible := ToggleBox1.Checked;
end;

end.

