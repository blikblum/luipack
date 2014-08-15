unit EvaluationView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ZVDateTimePicker;

type

  { TEvaluationForm }

  TEvaluationForm = class(TForm)
    CheckGroup1: TCheckGroup;
    ICDSCCheckGroup: TCheckGroup;
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
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  EvaluationForm: TEvaluationForm;

implementation

{$R *.lfm}

end.

