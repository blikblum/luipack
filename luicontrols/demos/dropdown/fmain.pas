unit fMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, DropDownManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DropDownWindow1: TDropDownManager;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    ToggleBox1: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure DropDownWindow1Hide(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DropDownWindow1Hide(Sender: TObject);
begin
 ToggleBox1.Checked := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DropDownWindow1.Visible := False;
end;

procedure TForm1.ToggleBox1Change(Sender: TObject);
begin
  DropDownWindow1.Visible := ToggleBox1.Checked;
end;

end.

