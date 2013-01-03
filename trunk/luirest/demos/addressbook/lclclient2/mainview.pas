unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TMainViewForm }

  TMainViewForm = class(TForm)
    MainPageControl: TPageControl;
    JSONTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainViewForm: TMainViewForm;

implementation

uses
  JSONResourceView;

{ TMainViewForm }

procedure TMainViewForm.FormCreate(Sender: TObject);
begin
  with TJSONResourceViewFrame.Create(Self) do
  begin
    Parent := JSONTabSheet;
  end;
end;

{$R *.lfm}

end.

