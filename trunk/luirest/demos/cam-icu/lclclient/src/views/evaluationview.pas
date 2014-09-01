unit EvaluationView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, JSONFormMediator, ZVDateTimePicker, EvaluationPresenter;

type

  { TEvaluationForm }

  TEvaluationForm = class(TForm)
    CancelButton: TBitBtn;
    SaveButton: TBitBtn;
    SedationCheckGroup: TCheckGroup;
    ICDSCCheckGroup: TCheckGroup;
    EvaluationMediator: TJSONFormMediator;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ShiftComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    DatePicker: TZVDateTimePicker;
    RASSComboBox: TComboBox;
    DeliriumComboBox: TComboBox;
    VentilationComboBox: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SedationCheckGroupItemClick(Sender: TObject; Index: integer);
  private
    FPresenter: TEvaluationPresenter;
  public
  published
    property Presenter: TEvaluationPresenter read FPresenter write FPresenter;
  end;

var
  EvaluationForm: TEvaluationForm;

implementation

{$R *.lfm}

{ TEvaluationForm }

procedure TEvaluationForm.FormShow(Sender: TObject);
begin
  EvaluationMediator.Data := FPresenter.Evaluation.Data;
  EvaluationMediator.LoadData;
end;

procedure TEvaluationForm.SaveButtonClick(Sender: TObject);
begin
  EvaluationMediator.SaveData;
  FPresenter.SaveEvaluation;
end;

procedure TEvaluationForm.SedationCheckGroupItemClick(Sender: TObject;
  Index: integer);
var
  i: Integer;
begin
  if (Index = 0) and (SedationCheckGroup.Checked[0]) then
  begin
    for i := 1 to SedationCheckGroup.Items.Count - 1 do
    begin
      SedationCheckGroup.Checked[i] := False;
    end;
  end
  else
  begin
    SedationCheckGroup.Checked[0] := False;
  end;
end;

end.

