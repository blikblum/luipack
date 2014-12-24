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
    UnitNameEdit: TLabeledEdit;
    ModelNameEdit: TLabeledEdit;
    procedure OkButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDescriptor: TJSONModelUnitDescriptor;
  public
    property Descriptor: TJSONModelUnitDescriptor read FDescriptor write FDescriptor;
  end;

var
  JSONModelUnitOptionsForm: TJSONModelUnitOptionsForm;

implementation

{$R *.lfm}

{ TJSONModelUnitOptionsForm }

procedure TJSONModelUnitOptionsForm.FormShow(Sender: TObject);
begin
  CollectionNameEdit.Text := Descriptor.CollectionName;
  ModelNameEdit.Text := Descriptor.ModelName;
  ResourceNameEdit.Text := Descriptor.ResourceName;
  UnitNameEdit.Text := Descriptor.DefaultSourceName;
end;

procedure TJSONModelUnitOptionsForm.OkButtonClick(Sender: TObject);
begin
  Descriptor.CollectionName := CollectionNameEdit.Text;
  Descriptor.ModelName := ModelNameEdit.Text;
  Descriptor.ResourceName := ResourceNameEdit.Text;
  Descriptor.DefaultSourceName := UnitNameEdit.Text;
end;

end.

