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

end.

