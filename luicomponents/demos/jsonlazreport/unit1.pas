unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LuiJSONLazReport,
  LuiJSONUtils, fpjson;

type

  { TForm1 }

  TForm1 = class(TForm)
    ShowReportButton: TButton;
    procedure ShowReportButtonClick(Sender: TObject);
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

procedure TForm1.ShowReportButtonClick(Sender: TObject);
var
  Obj: TJSONObject;
  Rep: TfrJSONReport;
begin
  Obj := TJSONFile.Load('test.json') as TJSONObject;
  Rep := TfrJSONReport.Create(Self);
  try
    Rep.LoadFromFile('design.lrf');
    Rep.RegisterDataSource('InfoData', 'info');
    Rep.RegisterDataSource('ItemsData', 'items');
    Rep.JSONObject := Obj;
    Rep.ShowReport;
  finally
    Rep.Destroy;
    Obj.Free;
  end;

end;

end.

