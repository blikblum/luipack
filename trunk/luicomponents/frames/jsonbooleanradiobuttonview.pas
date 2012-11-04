unit JSONBooleanRadioButtonView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, JSONBooleanGroupView, DropDownManager;

type

  { TJSONBooleanRadioButtonViewFrame }

  TJSONBooleanRadioButtonViewFrame = class(TFrame)
    CaptionLabel: TLabel;
    FalseRadioButton: TRadioButton;
    IndeterminateRadioButton: TRadioButton;
    TrueRadioButton: TRadioButton;
  private
    procedure SetPropertyCaption(const AValue: String);
    procedure SetTrueCaption(const Value: String);
    procedure SetFalseCaption(const Value: String);
    procedure SetIndeterminateCaption(const Value: String);
    procedure SetInitialValue(Value: TBooleanValue);
  public
    property PropertyCaption: String write SetPropertyCaption;
    property TrueCaption: String write SetTrueCaption;
    property FalseCaption: String write SetFalseCaption;
    property IndeterminateCaption: String write SetIndeterminateCaption;
    property InitialValue: TBooleanValue write SetInitialValue;
  end;

implementation

{$R *.lfm}

{ TJSONBooleanRadioButtonViewFrame }

procedure TJSONBooleanRadioButtonViewFrame.SetTrueCaption(const Value: String);
begin
  TrueRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetFalseCaption(const Value: String);
begin
  FalseRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetIndeterminateCaption(const Value: String);
begin
  if Value = '' then
    IndeterminateRadioButton.Visible := False
  else
    IndeterminateRadioButton.Caption := Value;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetInitialValue(Value: TBooleanValue);
begin
  case Value of
    bvTrue: TrueRadioButton.Checked := True;
    bvFalse: FalseRadioButton.Checked := True;
    bvIndeterminate: IndeterminateRadioButton.Checked := True;
  end;
end;

procedure TJSONBooleanRadioButtonViewFrame.SetPropertyCaption(const AValue: String);
begin
  CaptionLabel.Caption := AValue;
end;

end.

