unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AppServices, MyService;

type

  { TForm1 }

  TForm1 = class(TForm)
    TestMyServiceButton: TButton;
    TestMySecondServiceButton: TButton;
    procedure TestMySecondServiceButtonClick(Sender: TObject);
    procedure TestMyServiceButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TestMyServiceButtonClick(Sender: TObject);
var
  MyService: IMyService;
begin
  Services.Resolve(IMyService, MyService);
  ShowMessage(MyService.GetDescription);
end;

procedure TForm1.TestMySecondServiceButtonClick(Sender: TObject);
var
  MySecondService: IMySecondService;
begin
  Services.Resolve(IMySecondService, MySecondService);
  ShowMessage(MySecondService.GetDescription);
end;

end.

