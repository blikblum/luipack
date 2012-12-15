unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainPageControl: TPageControl;
    JSONTabSheet: TTabSheet;
    XMLTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  JSONServiceView, XMLServiceView;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  with TJSONServiceViewFrame.Create(Self) do
  begin
    Parent := JSONTabSheet;
  end;
  with TXMLServiceViewFrame.Create(Self) do
  begin
    Parent := XMLTabSheet;
  end;
end;

end.

