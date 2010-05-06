unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, DbCtrls, DBGrids, ExtCtrls, StdCtrls, ValidateEdit, db, dbf;

type

  { TMainForm }

  TMainForm = class(TForm)
    ValidateEditErrorMessageEdit: TEdit;
    ValidateEditNullAsErrorBox: TCheckBox;
    DateEditRecoverModeComboBox: TComboBox;
    DateEditNullAsErrorBox: TCheckBox;
    Datasource1: TDatasource;
    ValidateEditRecoverModeComboBox: TComboBox;
    DBDateMaskEdit1: TDBDateMaskEdit;
    Dbf1: TDbf;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DateEditErrorMessageEdit: TEdit;
    DBValidateEdit1: TDBValidateEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainPageControl: TPageControl;
    DBDateMaskEditTab: TTabSheet;
    Splitter1: TSplitter;
    DBValidateEditTab: TTabSheet;
    procedure DateEditErrorMessageEditEditingDone(Sender: TObject);
    procedure DateEditNullAsErrorBoxChange(Sender: TObject);
    procedure DateEditRecoverModeComboBoxChange(Sender: TObject);
    procedure DBValidateEdit1ValidateData(Sender: TObject; var NewData: String;
      var IsValid: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ValidateEditErrorMessageEditChange(Sender: TObject);
    procedure ValidateEditNullAsErrorBoxChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure ChangeNullAsError(ValidateEdit: TCustomDBValidateEdit; NullAsError: Boolean);
begin
  if NullAsError then
    ValidateEdit.Options := ValidateEdit.Options + [voNullValueAsError]
  else
    ValidateEdit.Options := ValidateEdit.Options - [voNullValueAsError];
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //ShortDateFormat := 'yyyy-mm-d';
  //DateSeparator := '-';
  with Dbf1 do
  begin
    if not FileExistsUTF8(TableName) then
    begin
      FieldDefs.Add('DateField', ftDate);
      FieldDefs.Add('StrField', ftString, 50);
      FieldDefs.Add('IntField', ftInteger);
      FieldDefs.Add('FloatField', ftFloat);
      CreateTable;
    end;
    Open;
  end;
  DateEditErrorMessageEdit.Text := DBDateMaskEdit1.InvalidValueMessage;
  ValidateEditErrorMessageEdit.Text := DBValidateEdit1.InvalidValueMessage;
end;

procedure TMainForm.ValidateEditErrorMessageEditChange(Sender: TObject);
begin
  DBValidateEdit1.InvalidValueMessage := ValidateEditErrorMessageEdit.Text;
end;

procedure TMainForm.ValidateEditNullAsErrorBoxChange(Sender: TObject);
begin
  ChangeNullAsError(DBValidateEdit1, ValidateEditNullAsErrorBox.Checked);
end;

procedure TMainForm.DateEditNullAsErrorBoxChange(Sender: TObject);
begin
  ChangeNullAsError(DBDateMaskEdit1, DateEditNullAsErrorBox.Checked);
end;

procedure TMainForm.DateEditErrorMessageEditEditingDone(Sender: TObject);
begin
  DBDateMaskEdit1.InvalidValueMessage := DateEditErrorMessageEdit.Text;
end;

procedure TMainForm.DateEditRecoverModeComboBoxChange(Sender: TObject);
begin
  {
  case DateEditRecoverModeComboBox.ItemIndex of
    0: DBDateMaskEdit1.RecoverMode := rmNone;
    1: DBDateMaskEdit1.RecoverMode := rmClear;
    2: DBDateMaskEdit1.RecoverMode := rmRestore;
  end;
  }
end;

procedure TMainForm.DBValidateEdit1ValidateData(Sender: TObject;
  var NewData: String; var IsValid: Boolean);
begin
  IsValid := (Length(NewData) < 10);
end;

initialization
  {$I fmain.lrs}

end.

