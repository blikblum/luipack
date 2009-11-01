unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, DbCtrls, DBGrids, ExtCtrls, StdCtrls, DateEdit, db, dbf;

type

  { TMainForm }

  TMainForm = class(TForm)
    DateEditRecoverModeComboBox: TComboBox;
    DateEditNullAsErrorBox: TCheckBox;
    Datasource1: TDatasource;
    DBDateMaskEdit1: TDBDateMaskEdit;
    Dbf1: TDbf;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DateEditErrorMessageEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainPageControl: TPageControl;
    DBDateMaskEditTab: TTabSheet;
    Splitter1: TSplitter;
    procedure DateEditErrorMessageEditEditingDone(Sender: TObject);
    procedure DateEditNullAsErrorBoxChange(Sender: TObject);
    procedure DateEditRecoverModeComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ShortDateFormat := 'yyyy-mm-d';
  DateSeparator := '-';
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
  DateEditErrorMessageEdit.Text := DBDateMaskEdit1.ErrorMessage;
end;

procedure TMainForm.DateEditNullAsErrorBoxChange(Sender: TObject);
begin
  if DateEditNullAsErrorBox.Checked then
    DBDateMaskEdit1.Options := DBDateMaskEdit1.Options + [deoNullDateAsError]
  else
    DBDateMaskEdit1.Options := DBDateMaskEdit1.Options - [deoNullDateAsError];
end;

procedure TMainForm.DateEditErrorMessageEditEditingDone(Sender: TObject);
begin
  DBDateMaskEdit1.ErrorMessage := DateEditErrorMessageEdit.Text;
end;

procedure TMainForm.DateEditRecoverModeComboBoxChange(Sender: TObject);
begin
  case DateEditRecoverModeComboBox.ItemIndex of
    0: DBDateMaskEdit1.RecoverMode := rmNone;
    1: DBDateMaskEdit1.RecoverMode := rmClear;
    2: DBDateMaskEdit1.RecoverMode := rmRestore;
  end;
end;

initialization
  {$I fmain.lrs}

end.

