unit PresentationOptionsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ValEdit, Buttons;

type

  { TPresentationOptionsForm }

  TPresentationOptionsForm = class(TForm)
    CancelButton: TBitBtn;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    Label1: TCheckBox;
    OkButton: TBitBtn;
    PresenterClassNameEdit: TLabeledEdit;
    ValueListEditor1: TValueListEditor;
    ViewUnitNameEdit: TLabeledEdit;
    ViewClassNameEdit: TLabeledEdit;
    PresenterUnitNameEdit: TLabeledEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PresentationOptionsForm: TPresentationOptionsForm;

implementation

{$R *.lfm}

end.

