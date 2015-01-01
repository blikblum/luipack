unit PresentationOptionsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ValEdit, Buttons, PresentationDescriptors;

type

  { TPresentationOptionsForm }

  TPresentationOptionsForm = class(TForm)
    CancelButton: TBitBtn;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    PresenterPropertiesCheckBox: TCheckBox;
    OkButton: TBitBtn;
    PresenterClassNameEdit: TLabeledEdit;
    PresenterPropertiesEditor: TValueListEditor;
    UnitNameChangeTimer: TTimer;
    ViewUnitNameEdit: TLabeledEdit;
    ViewClassNameEdit: TLabeledEdit;
    PresenterUnitNameEdit: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PresenterPropertiesCheckBoxChange(Sender: TObject);
    procedure UnitNameChangeTimerTimer(Sender: TObject);
    procedure ViewUnitNameEditChange(Sender: TObject);
  private
    FDescriptor: TPresenterViewDescriptor;
    FCurrentPrefix: String;
    procedure UpdatePrefixes;
    { private declarations }
  public
    { public declarations }
    property Descriptor: TPresenterViewDescriptor read FDescriptor write FDescriptor;
  end;

var
  PresentationOptionsForm: TPresentationOptionsForm;

implementation

{$R *.lfm}

{ TPresentationOptionsForm }

procedure TPresentationOptionsForm.FormShow(Sender: TObject);
var
  ViewUnitName: String;
begin
  ViewUnitName := FDescriptor.DefaultSourceName;
  ViewUnitNameEdit.Text := ViewUnitName;
  ViewClassNameEdit.Text := FDescriptor.ViewClassName;
  PresenterUnitNameEdit.Text := FDescriptor.PresenterUnitName;
  PresenterClassNameEdit.Text := FDescriptor.PresenterClassName;
  FCurrentPrefix := Copy(ViewUnitName, 1, Pos('view', LowerCase(ViewUnitName)) - 1);
  ViewUnitNameEdit.OnChange := @ViewUnitNameEditChange;
end;

procedure TPresentationOptionsForm.OkButtonClick(Sender: TObject);
begin
  FDescriptor.DefaultSourceName := ViewUnitNameEdit.Text;
  FDescriptor.ViewClassName := ViewClassNameEdit.Text;
  FDescriptor.PresenterUnitName := PresenterUnitNameEdit.Text;
  FDescriptor.PresenterClassName := PresenterClassNameEdit.Text;
  if PresenterPropertiesCheckBox.Checked then
    FDescriptor.PresenterProperties.Assign(PresenterPropertiesEditor.Strings);
end;

procedure TPresentationOptionsForm.PresenterPropertiesCheckBoxChange(
  Sender: TObject);
begin
  PresenterPropertiesEditor.Enabled := PresenterPropertiesCheckBox.Checked;
end;

procedure TPresentationOptionsForm.UnitNameChangeTimerTimer(Sender: TObject);
begin
  UpdatePrefixes;
  UnitNameChangeTimer.Enabled := False;
end;

procedure TPresentationOptionsForm.ViewUnitNameEditChange(Sender: TObject);
begin
  UnitNameChangeTimer.Enabled := False;
  UnitNameChangeTimer.Enabled := True;
end;

procedure TPresentationOptionsForm.UpdatePrefixes;
var
  NewPrefix, ViewUnitName: String;
  SuffixPos: Integer;
begin
  ViewUnitName := ViewUnitNameEdit.Text;
  SuffixPos := Pos('view', LowerCase(ViewUnitName));
  NewPrefix := Copy(ViewUnitNameEdit.Text, 1, SuffixPos - 1);
  if SameText(Trim(ViewClassNameEdit.Text), Format('T%sForm', [FCurrentPrefix])) then
    ViewClassNameEdit.Text := Format('T%sForm', [NewPrefix]);
  if SameText(Trim(PresenterUnitNameEdit.Text), Format('%sPresenter', [FCurrentPrefix])) then
    PresenterUnitNameEdit.Text := Format('%sPresenter', [NewPrefix]);
  if SameText(Trim(PresenterClassNameEdit.Text), Format('T%sPresenter', [FCurrentPrefix])) then
    PresenterClassNameEdit.Text := Format('T%sPresenter', [NewPrefix]);
  FCurrentPrefix := NewPrefix;
end;

end.

