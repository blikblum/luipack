unit JSONBooleanRadioButtonView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, JSONBooleanGroupView;

type

  { TJSONBooleanRadioButtonViewFrame }

  TJSONBooleanRadioButtonViewFrame = class(TFrame)
    CaptionLabel: TLabel;
    FalseRadioButton: TRadioButton;
    UndefinedRadioButton: TRadioButton;
    TrueRadioButton: TRadioButton;
  private
    FPropertyName: String;
    procedure SetPropertyCaption(const AValue: String);
    procedure SetPropertyName(const AValue: String);
    procedure SetTrueText(const Value: String);
    procedure SetFalseText(const Value: String);
    procedure SetUndefinedText(const Value: String);
    procedure SetInitialValue(Value: TBooleanValue);
  public
    property PropertyName: String read FPropertyName write SetPropertyName;
    property PropertyCaption: String write SetPropertyCaption;
    property TrueText: String write SetTrueText;
    property FalseText: String write SetFalseText;
    property UndefinedText: String write SetUndefinedText;
    property InitialValue: TBooleanValue write SetInitialValue;
  end;

implementation

{$R *.lfm}

{ TJSONBooleanRadioButtonViewFrame }

procedure TJSONBooleanRadioButtonViewFrame.SetPropertyName(const AValue: String);
begin
  FPropertyName := AValue;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetTrueText(const Value: String);
begin
  TrueRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetFalseText(const Value: String);
begin
  FalseRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetUndefinedText(const Value: String);
begin
  if Value = '' then
    UndefinedRadioButton.Visible := False
  else
    UndefinedRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetInitialValue(Value: TBooleanValue);
begin
  case Value of
    bvTrue: TrueRadioButton.Checked := True;
    bvFalse: FalseRadioButton.Checked := True;
    bvUndefined: UndefinedRadioButton.Checked := True;
  end;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetPropertyCaption(const AValue: String);
begin
  CaptionLabel.Caption := AValue;
end;

end.

