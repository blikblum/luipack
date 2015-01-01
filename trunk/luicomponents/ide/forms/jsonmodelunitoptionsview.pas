unit JSONModelUnitOptionsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, JSONModelDescriptors;

type

  { TJSONModelUnitOptionsForm }

  TJSONModelUnitOptionsForm = class(TForm)
    CancelButton: TBitBtn;
    ResourceNameEdit: TLabeledEdit;
    OkButton: TBitBtn;
    CollectionNameEdit: TLabeledEdit;
    UnitNameChangeTimer: TTimer;
    UnitNameEdit: TLabeledEdit;
    ModelNameEdit: TLabeledEdit;
    procedure OkButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UnitNameChangeTimerTimer(Sender: TObject);
    procedure UnitNameEditChange(Sender: TObject);
  private
    FCurrentPrefix: String;
    FDescriptor: TJSONModelUnitDescriptor;
    procedure UpdatePrefixes;
  public
    property Descriptor: TJSONModelUnitDescriptor read FDescriptor write FDescriptor;
  end;

var
  JSONModelUnitOptionsForm: TJSONModelUnitOptionsForm;

implementation

{$R *.lfm}

{ TJSONModelUnitOptionsForm }

procedure TJSONModelUnitOptionsForm.FormShow(Sender: TObject);
var
  AUnitName: String;
begin
  CollectionNameEdit.Text := Descriptor.CollectionName;
  ModelNameEdit.Text := Descriptor.ModelName;
  ResourceNameEdit.Text := Descriptor.ResourceName;
  AUnitName := Descriptor.DefaultSourceName;
  UnitNameEdit.Text := AUnitName;
  FCurrentPrefix := Copy(AUnitName, 1, Pos('model', LowerCase(AUnitName)) - 1);
  UnitNameEdit.OnChange := @UnitNameEditChange;
end;

procedure TJSONModelUnitOptionsForm.UnitNameChangeTimerTimer(Sender: TObject);
begin
  UpdatePrefixes;
  UnitNameChangeTimer.Enabled := False;
end;

procedure TJSONModelUnitOptionsForm.UnitNameEditChange(Sender: TObject);
begin
  UnitNameChangeTimer.Enabled := False;
  UnitNameChangeTimer.Enabled := True;
end;

procedure TJSONModelUnitOptionsForm.UpdatePrefixes;
var
  NewPrefix, AUnitName: String;
  SuffixPos: Integer;
begin
  AUnitName := UnitNameEdit.Text;
  SuffixPos := Pos('model', LowerCase(AUnitName));
  NewPrefix := Copy(AUnitName, 1, SuffixPos - 1);
  if SameText(ModelNameEdit.Text, Format('T%sModel', [FCurrentPrefix])) then
    ModelNameEdit.Text := Format('T%sModel', [NewPrefix]);
  if SameText(CollectionNameEdit.Text, Format('T%sCollection', [FCurrentPrefix])) then
    CollectionNameEdit.Text := Format('T%sCollection', [NewPrefix]);
  if SameText(ResourceNameEdit.Text, FCurrentPrefix) then
    ResourceNameEdit.Text := LowerCase(NewPrefix);
  FCurrentPrefix := NewPrefix;
end;

procedure TJSONModelUnitOptionsForm.OkButtonClick(Sender: TObject);
begin
  Descriptor.CollectionName := CollectionNameEdit.Text;
  Descriptor.ModelName := ModelNameEdit.Text;
  Descriptor.ResourceName := ResourceNameEdit.Text;
  Descriptor.DefaultSourceName := UnitNameEdit.Text;
end;

end.

