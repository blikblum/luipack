unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LuiJSONLCLViews, fpjson;

type

  { TMainForm }

  TMainForm = class(TForm)
    DumpButton: TButton;
    DumpMemo: TMemo;
    SaveButton: TButton;
    LoadButton: TButton;
    ShortStrLabel: TLabel;
    ShortStrEdit: TEdit;
    JSONObjectViewManager: TJSONObjectViewManager;
    MultiLineStrMemo: TMemo;
    procedure DumpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    { private declarations }
    FObject: TJSONObject;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FObject := TJSONObject.Create(['shortstr', 'short', 'multilinestr',
    'long' + LineEnding + 'long' + LineEnding + 'long', 'int', 13]);
  JSONObjectViewManager.JSONObject := FObject;
end;

procedure TMainForm.DumpButtonClick(Sender: TObject);
var
  i: Integer;
begin
  DumpMemo.Clear;
  for i := 0 to FObject.Count - 1 do
    DumpMemo.Lines.Add(FObject.Items[i].AsString);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FObject.Destroy;
end;

procedure TMainForm.LoadButtonClick(Sender: TObject);
begin
  JSONObjectViewManager.Load;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  JSONObjectViewManager.Save;
end;

end.

