unit fExportDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, Buttons, CheckLst, ControlSwitcher, LuiBar, db;

type

  { TExportDatasetForm }

  TExportDatasetForm = class(TForm)
    Bevel1: TBevel;
    CancelButton: TBitBtn;
    SaveFileButton: TBitBtn;
    ImageList1: TImageList;
    SelectAllButton: TButton;
    UnselectAllButton: TButton;
    FieldsCheckListBox: TCheckListBox;
    ControlSwitcher: TControlSwitcher;
    SelectFieldsLabel: TLabel;
    SelectFieldsPanel: TPanel;
    DirectoryEdit: TDirectoryEdit;
    FileNameEdit: TEdit;
    FileSettingsPanel: TPanel;
    FileTypeGroup: TRadioGroup;
    Label1: TLabel;
    FileNameLabel: TLabel;
    procedure ControlSwitcherGetImageInfo(Sender: TLuiBar; Cell: TLuiBarCell;
      var ImageInfo: TLuiBarImageInfo);
    procedure DirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure FileNameEditChange(Sender: TObject);
    procedure FileNameEditEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlSwitcherSelecting(Sender: TLuiBar; OldCell,
      NewCell: Integer; var Allowed: Boolean);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure UnselectAllButtonClick(Sender: TObject);
    procedure UpdateFieldSelectionStatus(Sender: TObject);
    procedure UpdateFileSettingsStatus(DirExists: Boolean);
  private
    FDataset: TDataset;
    FFileSettingsIsValid: Boolean;
    FFieldSelectionIsValid: Boolean;
    procedure UpdateSaveFileButton;
    { private declarations }
    property FileSettingsIsValid: Boolean read FFileSettingsIsValid;
    property FieldsSelectionIsValid: Boolean read FFieldSelectionIsValid;
  public
    { public declarations }
    class function Execute(AOwner: TWinControl; Dataset: TDataSet): Boolean;
    procedure GetCheckedFields(FieldList: TStrings);
    property Dataset: TDataset read FDataset write FDataset;
  end;

implementation

{$R *.lfm}

uses
  GraphType, LazUTF8, LazFileUtils;

procedure SetCheckListState(List: TCheckListBox; CheckState: Boolean);
var
  i: Integer;
begin
  for i := 0 to List.Items.Count - 1 do
    List.Checked[i] := CheckState;
end;

function GetCheckListCheckedString(List: TCheckListBox): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Items.Count - 1 do
    if List.Checked[i] then
      Result := Result + List.Items[i] + ';';
end;

procedure GetCheckListCheckedObjects(List: TCheckListBox; Objects: TFPList);
var
  i: Integer;
begin
  for i := 0 to List.Items.Count - 1 do
    if List.Checked[i] then
      Objects.Add(List.Items.Objects[i]);
end;


procedure GetCheckListCheckedItems(List: TCheckListBox; Items: TStrings);
var
  i: Integer;
begin
  for i := 0 to List.Items.Count - 1 do
    if List.Checked[i] then
      Items.AddObject(List.Items[i], List.Items.Objects[i]);
end;

function GetCheckListHasChecked(List: TCheckListBox): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to List.Items.Count - 1 do
    if List.Checked[i] then
      Exit(True);
end;

{ TExportDatasetForm }

procedure TExportDatasetForm.ControlSwitcherSelecting(Sender: TLuiBar; OldCell,
  NewCell: Integer; var Allowed: Boolean);
begin
  Allowed := (NewCell <> 1) or FileSettingsIsValid;
end;

procedure TExportDatasetForm.SelectAllButtonClick(Sender: TObject);
begin
  SetCheckListState(FieldsCheckListBox, True);
  UpdateFieldSelectionStatus(nil);
end;

procedure TExportDatasetForm.UnselectAllButtonClick(Sender: TObject);
begin
  SetCheckListState(FieldsCheckListBox, False);
  UpdateFieldSelectionStatus(nil);
end;

procedure TExportDatasetForm.UpdateSaveFileButton;
begin
  SaveFileButton.Enabled := FileSettingsIsValid and FieldsSelectionIsValid;
end;

procedure TExportDatasetForm.UpdateFieldSelectionStatus(Sender: TObject);
begin
  if FFieldSelectionIsValid <> GetCheckListHasChecked(FieldsCheckListBox) then
  begin
    FFieldSelectionIsValid := not FFieldSelectionIsValid;
    ControlSwitcher.Redraw;
    UpdateSaveFileButton;
  end;
end;

procedure TExportDatasetForm.UpdateFileSettingsStatus(DirExists: Boolean);
var
  ColorMap: array[Boolean] of TColor = (clRed, clWindow);
begin
  DirectoryEdit.Color := ColorMap[DirExists];
  if FFileSettingsIsValid <> (DirExists and (FileNameEdit.Text <> '')) then
  begin
    FFileSettingsIsValid := not FFileSettingsIsValid;
    ControlSwitcher.Redraw;
    UpdateSaveFileButton;
  end;
end;

procedure TExportDatasetForm.ControlSwitcherGetImageInfo(Sender: TLuiBar;
  Cell: TLuiBarCell; var ImageInfo: TLuiBarImageInfo);
var
  ImageMap: array[Boolean] of Integer = (0, 1);
  EffectMap: array[Boolean] of TGraphicsDrawEffect = (gdeDisabled, gdeNormal);
begin
  case Cell.Index of
    0: ImageInfo.Index := ImageMap[FileSettingsIsValid];
    1:
      begin
        ImageInfo.Index := ImageMap[FieldsSelectionIsValid];
        ImageInfo.Effect := EffectMap[(Sender.SelectedIndex = 1) or FileSettingsIsValid];
      end;
  end;
end;

procedure TExportDatasetForm.DirectoryEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  UpdateFileSettingsStatus(DirectoryExistsUTF8(Value));
end;

procedure TExportDatasetForm.FileNameEditChange(Sender: TObject);
var
  ColorMap: array[Boolean] of TColor = (clRed, clWindow);
begin
  FileNameEdit.Color := ColorMap[FileNameEdit.Text <> ''];
end;

procedure TExportDatasetForm.FileNameEditEditingDone(Sender: TObject);
begin
  UpdateFileSettingsStatus(DirectoryExistsUTF8(DirectoryEdit.Directory));
end;

procedure TExportDatasetForm.FormShow(Sender: TObject);
begin
  UpdateFileSettingsStatus(DirectoryExistsUTF8(DirectoryEdit.Directory));
end;

class function TExportDatasetForm.Execute(AOwner: TWinControl; Dataset: TDataSet): Boolean;
var
  i: Integer;
begin
  Result := False;
  with TExportDatasetForm.Create(AOwner) do
  try
    FDataset := Dataset;
    for i := 0 to FDataset.Fields.Count - 1 do
      FieldsCheckListBox.Items.AddObject(FDataset.Fields[i].DisplayLabel, FDataset.Fields[i]);
    DirectoryEdit.Directory := ExtractFileDir(ParamStrUTF8(0));
    Result := ShowModal = mrOK;
  finally
    Destroy;
  end;
end;

procedure TExportDatasetForm.GetCheckedFields(FieldList: TStrings);
begin
  GetCheckListCheckedItems(FieldsCheckListBox, FieldList);
end;

initialization

end.

