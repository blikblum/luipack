unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ChronoLog,
  StdCtrls, ExtCtrls;

type

  TTestProc = procedure (Log: TChronoLog);

  { TFormMain }

  TFormMain = class(TForm)
    CheckClearPrevious: TCheckBox;
    Label1: TLabel;
    ListTests: TListBox;
    MemoOutput: TMemo;
    PanelLeft: TPanel;
    SplitterVertical: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ListTestsDblClick(Sender: TObject);
  private
    { private declarations }
    procedure RegisterTest(const AName: String; Proc: TTestProc);
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  uCancatenateString;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  RegisterTest('Concatenate String', @TestConcatenateString);
end;

procedure TFormMain.ListTestsDblClick(Sender: TObject);
var
  Log: TChronoLog;
  TestProc: TTestProc;
  TempFile: String;
begin
  if ListTests.ItemIndex = - 1 then
    Exit;
  Log := TChronoLog.Create;
  TestProc := TTestProc(ListTests.Items.Objects[ListTests.ItemIndex]);
  TestProc(Log);
  TempFile := GetTempDir + 'chronolog_demo_temp.log';
  Log.SaveToText(TempFile, CheckClearPrevious.Checked);
  MemoOutput.Lines.LoadFromFile(TempFile);
  Log.Destroy;
end;

procedure TFormMain.RegisterTest(const AName: String; Proc: TTestProc);
begin
  ListTests.Items.AddObject(AName, TObject(Proc));
end;

initialization
  {$I fmain.lrs}

end.

