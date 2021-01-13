unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, StdCtrls, UniqueInstance;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    UniqueInstance1: TUniqueInstance;
    procedure FormCreate(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
    procedure UniqueInstance1QueryBeforeCheck(Sender: TObject; var TerminatePrevious: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text:=TimeToStr(Time);
end;

procedure TForm1.UniqueInstance1QueryBeforeCheck(Sender: TObject; var TerminatePrevious: Boolean);
begin
  TerminatePrevious:=MessageDlg(Application.Title, 'Should it close the previous instance of the app?',
    mtConfirmation, mbYesNo, EmptyStr)=mrYes;
end;

end.

