unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  VTJSONEx, fpjson, VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm)
    AnswerMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    QuestionTreeView: TJSONQuestionTreeView;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure QuestionTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { private declarations }
    FQuestionData: TJSONData;
    FAnswerData: TJSONObject;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  LuiJSONUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FAnswerData := TJSONFile.Load('answers1.json') as TJSONObject;
  FQuestionData := TJSONFile.Load('questions1.json');
  QuestionTreeView.Data := FQuestionData;
  QuestionTreeView.LoadData;
  QuestionTreeView.LoadAnswerData(FAnswerData, True);
  QuestionTreeView.FullExpand;
  AnswerMemo.Text := FAnswerData.FormatJSON;
end;

procedure TMainForm.QuestionTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  AnswerMemo.Text := FAnswerData.FormatJSON;
end;

end.

