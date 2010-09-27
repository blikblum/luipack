unit Umegademo;

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, VirtualDBGrid, ExtCtrls, VirtualTrees, Buttons,
  StdCtrls, DBCtrls, LResources, dbf, Variants;

type

  { TMainForm }

  TMainForm = class(TForm)
    Dbf1: TDbf;
    DBNavigator2: TDBNavigator;
    Page1: TPage;
    Page2: TPage;
    DataSource1: TDataSource;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lbOddRowColor: TLabel;
    lbEvenRowColor: TLabel;
    chStrippedRows: TCheckBox;
    IndicatorImages: TImageList;
    ColorD: TColorDialog;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    clrIndicatorBgColor: TShape;
    chAutoInsertIndicator: TCheckBox;
    cmbIndicatorAlign: TComboBox;
    cmbIndicatorVAlign: TComboBox;
    cmbIndicatorImage: TComboBox;
    Label6: TLabel;
    chShowHorzLines: TCheckBox;
    chShowVertLines: TCheckBox;
    chFullRowSelect: TCheckBox;
    chMultiSelect: TCheckBox;
    GroupBox4: TGroupBox;
    chEditable: TCheckBox;
    chAllowSorting: TCheckBox;
    chSortDBFieldColumns: TCheckBox;
    chSortCalculatedColumns: TCheckBox;
    chActiveDatabase: TCheckBox;
    chScrollIntoView: TCheckBox;
    chHourGlassCursor: TCheckBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label7: TLabel;
    DBEdit2: TDBEdit;
    Label8: TLabel;
    DBEdit3: TDBEdit;
    Label9: TLabel;
    DBEdit4: TDBEdit;
    Label10: TLabel;
    DBEdit5: TDBEdit;
    DBNavigator1: TDBNavigator;
    Label11: TLabel;
    DBEdit6: TDBEdit;
    Label12: TLabel;
    DBEdit7: TDBEdit;
    cmbSortingType: TComboBox;
    GroupBox6: TGroupBox;
    SelRecInfoBtn: TButton;
    NTB: TNotebook;
    DBGrid: TVirtualDBGrid;
    Memo1: TMemo;
    VDBGridBtn: TButton;
    chEditDBFieldColumns: TCheckBox;
    chEditCalculatedColumns: TCheckBox;
    Label13: TLabel;
    cmbSortColumn: TComboBox;
    Label14: TLabel;
    cmbSortDirection: TComboBox;
    SetSortBtn: TSpeedButton;
    procedure DBGridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure FormCreate(Sender: TObject);
    procedure SetSettings(Sender: TObject);
    procedure chActiveDatabaseClick(Sender: TObject);
    procedure lbOddRowColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmbIndicatorAlignChange(Sender: TObject);
    procedure cmbIndicatorVAlignChange(Sender: TObject);
    procedure cmbIndicatorImageChange(Sender: TObject);
    procedure clrIndicatorBgColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cmbSortingTypeChange(Sender: TObject);
    procedure DBGridCustomSort(Sender: TObject; Column: TColumnIndex;
      ColumnType: TColumnType; SortBy: String;
      SortDirection: TSortDirection; var RefreshGrid: Boolean);
    procedure DBGridFormatFieldValue(Sender: TObject; Column: TColumnIndex;
      RecordData: TRecordData; RowIndex: Cardinal; Field: TField;
      var FieldValue: Variant);
    procedure SelRecInfoBtnClick(Sender: TObject);
    procedure VDBGridBtnClick(Sender: TObject);
    procedure DBGridPostChanges(Sender: TObject; FieldNameOrIDText: String;
      Column: TColumnIndex; ColumnType: TColumnType;
      RecordData: TRecordData; RowIndex: Cardinal; var NewValue: UTF8String;
      var DoDefault: Boolean);
    procedure DBGridChangeSort(Sender: TObject; SortColumn: TColumnIndex;
      SortDirection: TSortDirection);
    procedure SetSortBtnClick(Sender: TObject);
    procedure DBGridPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure DBGridCalculateValue(Sender: TObject; IDText: String;
      Column: TColumnIndex; RecordData: TRecordData; RowIndex: Cardinal; 
      var CalculatedValue: String; var CalculatedValueType: TFieldType);
  private
    function GetIndicatorColor: TColor;
    procedure OpenDataset;
    procedure CloseDataset;
    procedure SetupGrid;
    procedure UpdateButtons;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

  OrderByStr,
  SortDirStr  : string;


implementation


procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetupGrid;

  Dbf1.FilePathFull := ExtractFilePath(paramstr(0));
  Dbf1.TableName := 'StreetsBig.DBF';

  OrderByStr:= 'CODE_ID';
  SortDirStr:= 'ASC';

{ --- Get Settings from DBGrid to settings controls --- }

 { -- Row options -- }
  { Show stripped rows }
  chStrippedRows.Checked:= (aoStrippedRows in DBGrid.DBOptions.AdvOptions);
  { Odd row color }
  lbOddRowColor.Color:= DBGrid.DBOptions.OddRowColor;
  { Even row color }
  lbEvenRowColor.Color:= DBGrid.DBOptions.EvenRowColor;
                         { Show horizontal lines }
  chShowHorzLines.Checked:= (aoShowHorzLines in DBGrid.DBOptions.AdvOptions);
  { Show vertical lines }
  chShowVertLines.Checked:= (aoShowVertLines in DBGrid.DBOptions.AdvOptions);
  { Full row select }
  chFullRowSelect.Checked:= (aoFullRowSelect in DBGrid.DBOptions.AdvOptions);
  { Multi select }
  chMultiSelect.Checked:= (aoMultiSelect in DBGrid.DBOptions.AdvOptions);

 { -- Row indicator -- }
  { Auto insert indicator }
    { Insert automatically indicator column, when opening dataset, }
    { if there are no columns defined                              }
  chAutoInsertIndicator.Checked:= (aoAutoInsertIndicator in DBGrid.DBOptions.AdvOptions);
  { Indicator align }
  cmbIndicatorAlign.ItemIndex:= Integer(DBGrid.DBOptions.IndicatorAlign);
  { Indicator vertical align }
  cmbIndicatorVAlign.ItemIndex:= Integer(DBGrid.DBOptions.IndicatorVAlign);
  { Indicator image }
  cmbIndicatorImage.ItemIndex:= Integer(DBGrid.DBOptions.IndicatorImageIndex) + 1;
  { Indicator bg color }
  clrIndicatorBgColor.Brush.Color:= GetIndicatorColor;

 { -- Others -- }
  { Cell values editable (and post changes to database) }
  chEditable.Checked:= (aoEditable in DBGrid.DBOptions.AdvOptions);
  { Allow sorting }
    { if is set then click on header will begin sorting procedure }
    { depending of SortType in DBOptions                          }
  chAllowSorting.Checked:= (aoAllowSorting in DBGrid.DBOptions.AdvOptions);
  cmbSortingType.ItemIndex:= Integer(DBGrid.DBOptions.SortingType);

  { Sort database field columns }
  chSortDBFieldColumns.Checked:= (aoSortDBFieldColumns in DBGrid.DBOptions.AdvOptions);
  { Sort calculated columns }
  chSortCalculatedColumns.Checked:= (aoSortCalculatedColumns in DBGrid.DBOptions.AdvOptions);
  { Center scroll into view }
  chScrollIntoView.Checked:= (aoCenterScrollIntoView in DBGrid.DBOptions.AdvOptions);
  { Show HourGlass cursor on sort action }
  chHourGlassCursor.Checked:= (aoHourGlassCursor in DBGrid.DBOptions.AdvOptions);
  { Edit DB field columns }
  chEditDBFieldColumns.Checked:= (aoEditDBFieldColumns in DBGrid.DBOptions.AdvOptions);
  { Edit calculated columns }
  chEditCalculatedColumns.Checked:= (aoEditCalculatedColumns in DBGrid.DBOptions.AdvOptions);

  NTB.PageIndex:= 0;
  UpdateButtons;

  chActiveDatabase.Checked:= true;
end;

procedure TMainForm.OpenDataset;
var
   I: Integer;
begin
  // open dataset, this automatically load up data to virtualdbgrid
  Dbf1.Active := True;

  cmbSortColumn.Items.Clear;
  for I:= 0 to DBGrid.Header.Columns.Count-1 do
  begin
    if (TVirtualDBTreeColumn(DBGrid.Header.Columns[I]).ColumnType <> ctIndicator)
       then cmbSortColumn.Items.Add(DBGrid.Header.Columns[I].Text);
  end;
  cmbSortColumn.ItemIndex:= -1;
  cmbSortDirection.ItemIndex:= -1;
end;

function TMainForm.GetIndicatorColor: TColor;
var
   IndicatorCol: TVirtualDBTreeColumn;
begin
  result:= DefaultIndicatorColor; 

  IndicatorCol:= DBGrid.IndicatorColumn;
  if (IndicatorCol <> nil) then
     result:= IndicatorCol.Color;
end;

procedure TMainForm.SetSettings(Sender: TObject);
var
   Checked: boolean;
   Tag: Integer;
begin
  Checked:= (Sender as TCheckBox).Checked;
  Tag:=     (Sender as TCheckBox).Tag;

  with DBGrid.DBOptions do
  case (Tag) of
       10: begin { Show stripped rows }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoStrippedRows]
            else AdvOptions:= AdvOptions - [aoStrippedRows];
       end;

       11: begin { Show horizontal lines }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoShowHorzLines]
            else AdvOptions:= AdvOptions - [aoShowHorzLines];
       end;

       12: begin { Show vertical lines }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoShowVertLines]
            else AdvOptions:= AdvOptions - [aoShowVertLines];
       end;

       13: begin { Full row select }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoFullRowSelect]
            else AdvOptions:= AdvOptions - [aoFullRowSelect];
       end;

       14: begin { Multi select }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoMultiSelect]
            else AdvOptions:= AdvOptions - [aoMultiSelect];
       end;

       20: begin { Auto insert indicator }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoAutoInsertIndicator]
            else AdvOptions:= AdvOptions - [aoAutoInsertIndicator];
       end;

       30: begin { Cell values editable (and post changes to database) }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoEditable]
            else AdvOptions:= AdvOptions - [aoEditable];
       end;

       31: begin { Autosort (build in sorting feature, doesn't use db to sort) }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoAllowSorting]
            else AdvOptions:= AdvOptions - [aoAllowSorting];
       end;

       32: begin { Sort database field columns }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoSortDBFieldColumns]
            else AdvOptions:= AdvOptions - [aoSortDBFieldColumns];
       end;

       33: begin { Sort calculated columns }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoSortCalculatedColumns]
            else AdvOptions:= AdvOptions - [aoSortCalculatedColumns];
       end;


       35: begin { Center scroll into view }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoCenterScrollIntoView]
            else AdvOptions:= AdvOptions - [aoCenterScrollIntoView];
       end;

       36: begin { Show HourGlass cursor on sort action }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoHourGlassCursor]
            else AdvOptions:= AdvOptions - [aoHourGlassCursor];
       end;

       37: begin { Edit DB field columns }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoEditDBFieldColumns]
            else AdvOptions:= AdvOptions - [aoEditDBFieldColumns];
       end;

       38: begin { Edit calculated columns }
         if (Checked)
            then AdvOptions:= AdvOptions + [aoEditCalculatedColumns]
            else AdvOptions:= AdvOptions - [aoEditCalculatedColumns];
       end;

  end;
end;

procedure TMainForm.chActiveDatabaseClick(Sender: TObject);
begin
  if (chActiveDatabase.Checked)
     then OpenDataset
     else CloseDataset;
end;

procedure TMainForm.CloseDataset;
begin
  Dbf1.Active:= false;
  cmbSortColumn.Items.Clear;
  cmbSortDirection.ItemIndex:= -1; 
end;

procedure TMainForm.SetupGrid;
begin
  // Assign datasource to dbgrid, you can also assign datasource at design time
  DBGrid.DBOptions.DataSource := DataSource1;
  DBGrid.AddCalcColumn('RowNumber', 80);
  DBGrid.AddCalcColumn('Time', 80);
  DBGrid.AddCalcColumn('Numb1+Numb2', 100);
end;


procedure TMainForm.lbOddRowColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorD.Color:= (Sender as TLabel).Color;
  if (not ColorD.Execute) then exit;
  (Sender as TLabel).Color:= ColorD.Color;

  if (Sender as TLabel).Tag = 0
     then DBGrid.DBOptions.OddRowColor:= ColorD.Color
     else DBGrid.DBOptions.EvenRowColor:= ColorD.Color;
end;

procedure TMainForm.cmbIndicatorAlignChange(Sender: TObject);
begin
  DBGrid.DBOptions.IndicatorAlign:= TIndicatorAlign(cmbIndicatorAlign.ItemIndex);
end;

procedure TMainForm.cmbIndicatorVAlignChange(Sender: TObject);
begin
  DBGrid.DBOptions.IndicatorVAlign:= TIndicatorVAlign(cmbIndicatorVAlign.ItemIndex);
end;

procedure TMainForm.cmbIndicatorImageChange(Sender: TObject);
begin
  DBGrid.DBOptions.IndicatorImageIndex:= cmbIndicatorImage.ItemIndex - 1;
end;

procedure TMainForm.clrIndicatorBgColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorD.Color:= clrIndicatorBgColor.Brush.Color;
  if (not ColorD.Execute) then exit;

  clrIndicatorBgColor.Brush.Color:= ColorD.Color;

  if (DBGrid.IndicatorColumn <> nil) then
     DBGrid.IndicatorColumn.Color:= ColorD.Color;
end;

procedure TMainForm.cmbSortingTypeChange(Sender: TObject);
begin
  DBGrid.DBOptions.SortingType:= TSortingType(cmbSortingType.ItemIndex);
end;

procedure TMainForm.DBGridCustomSort(Sender: TObject; Column: TColumnIndex;
  ColumnType: TColumnType; SortBy: String; SortDirection: TSortDirection;
  var RefreshGrid: Boolean);
const
     SortDirectionStr: array[TSortDirection] of string =
                       ('ASC', 'DESC');
begin
  if (ColumnType = ctDBField) then
  begin
    OrderByStr:= SortBy;
    SortDirStr:= SortDirectionStr[SortDirection];

    Dbf1.Active:= false;
    Dbf1.Active:= true;

    RefreshGrid:= false;
  end;
end;

procedure TMainForm.DBGridFormatFieldValue(Sender: TObject;
  Column: TColumnIndex; RecordData: TRecordData; RowIndex: Cardinal;
  Field: TField; var FieldValue: Variant);
begin
   if (Field.FieldName = 'XHOST_DATE') then
   begin
      if Odd(RecordData.RecNo)
         then FieldValue:= FormatDateTime('mm.dd.YYYY', Field.Value)
         else FieldValue:= '[no date]';
   end;
end;

procedure TMainForm.UpdateButtons;
begin
  SelRecInfoBtn.Enabled:= (NTB.PageIndex = 0);
  VDBGridBtn.Enabled:= not SelRecInfoBtn.Enabled;
end;

procedure TMainForm.SelRecInfoBtnClick(Sender: TObject);
var
  i, j: integer;
  rec: TRecordData;
  Str: String;
const
  calcstr: array[boolean] of string = ('', ' (calculated)');
  indicstr: array[boolean] of string = ('', ' (indicator)');
begin
  NTB.PageIndex:= 1;
  UpdateButtons;

  Memo1.Lines.Clear;
  Memo1.Lines.Add('Selected records: '+inttostr(DBGrid.SelectedCount));
  Memo1.Lines.Add('========================================');

  for i := 0 to DBGrid.SelectedCount-1 do
  begin
     rec:= DBGrid.SelectedRecord[i];
     if (rec <> nil) then
     begin
        Memo1.Lines.Add(format('Selected record #%d values:', [i]));
        Memo1.Lines.Add('-----------------------------------');
        for j := 0 to rec.FieldsCount - 1 do
        begin
          str := calcstr[rec.IsCalculatedByIdx[j]];
          if str = '' then
            str := indicstr[rec.IsIndicatorByIdx[j]];
          Memo1.Lines.Add(Format('[%s%s]: %s', [rec.FieldName[j], Str, VarToStr(rec.FieldValueByIdx[j])]));
        end;
        Memo1.Lines.Add('-----------------------------------');
     end;
  end;
end;

procedure TMainForm.VDBGridBtnClick(Sender: TObject);
begin
  NTB.PageIndex:= 0;
  UpdateButtons;
end;

procedure TMainForm.DBGridPostChanges(Sender: TObject;
  FieldNameOrIDText: String; Column: TColumnIndex; ColumnType: TColumnType;
  RecordData: TRecordData; RowIndex: Cardinal; var NewValue: UTF8String;
  var DoDefault: Boolean);
begin
  if (ColumnType = ctCalculated) then
    DoDefault:= MessageDlg('Do you really want to save changes of calculated value?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TMainForm.DBGridChangeSort(Sender: TObject;
  SortColumn: TColumnIndex; SortDirection: TSortDirection);

var
   ColumnText: string;
   Index: Integer;
begin
  if (SortColumn > NoColumn) then
  begin
    ColumnText:= DBGrid.Header.Columns[SortColumn].Text;

    Index:= cmbSortColumn.Items.IndexOf(ColumnText);
    if (Index > -1) and (Index < cmbSortColumn.Items.Count) then
       cmbSortColumn.ItemIndex:= Index;

    if (Byte(SortDirection) < cmbSortDirection.Items.Count) then
       cmbSortDirection.ItemIndex:= Byte(SortDirection);
  end;
end;

procedure TMainForm.SetSortBtnClick(Sender: TObject);
var
   sDirection: TSortDirection;
   SortColumnText: string;
begin
  if (cmbSortColumn.ItemIndex = -1) and
     (cmbSortDirection.ItemIndex = -1)
     then exit; 

  SortColumnText:= '';
  sDirection:= sdAscending;

  if (cmbSortColumn.ItemIndex > -1) then
     SortColumnText:= cmbSortColumn.Text;

  if (cmbSortDirection.ItemIndex > -1) then
     sDirection:= TSortDirection(cmbSortDirection.ItemIndex);

  DBGrid.SetSortColumn(SortColumnText, sDirection);
end;

procedure TMainForm.DBGridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

var
  RecordData: TRecordData;
  VDBGridColumn: TVirtualDBTreeColumn;
begin
  if (Column <= NoColumn) then exit;
  VDBGridColumn := TVirtualDBTreeColumn(DBGrid.Header.Columns[Column]);
  if (VDBGridColumn.ColumnType = ctCalculated) and (VDBGridColumn.Text = 'RowNumber') then
  begin
    RecordData := DBGrid.GetNodeRecordData(Node);
    if RecordData <> nil then
    begin
      TargetCanvas.Font.Style := [fsBold];
      if Odd(RecordData.RecNo) then
        TargetCanvas.Font.Color := clBlack
      else
        TargetCanvas.Font.Color := clNavy;
    end;
  end;
end;

procedure TMainForm.DBGridBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  VDBGridColumn: TVirtualDBTreeColumn;
begin
  if (Column <= NoColumn) then exit;
  VDBGridColumn := TVirtualDBTreeColumn(DBGrid.Header.Columns[Column]);

  if VDBGridColumn.ColumnType = ctCalculated then
  begin
    if VDBGridColumn.Text = 'RowNumber' then
    begin
      TargetCanvas.Brush.Color := clSilver;
      TargetCanvas.FillRect(CellRect);
    end else if VDBGridColumn.Text = 'Numb1+Numb2' then
    begin
      TargetCanvas.Brush.Color := clWhiteSmoke;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TMainForm.DBGridCalculateValue(Sender: TObject; IDText: String;
  Column: TColumnIndex; RecordData: TRecordData; RowIndex: Cardinal; 
  var CalculatedValue: String; var CalculatedValueType: TFieldType);

var
   numb1,
   numb2: Variant;
begin
  if IDText = 'RowNumber' then
  begin
     CalculatedValue:= Format('%.5d', [RecordData.RecNo]);
     CalculatedValueType:= ftString;
  end else if IDText = 'Time' then
  begin
     CalculatedValue:= FormatDateTime('hh: nn: ss', now);
     CalculatedValueType:= ftString;
  end else if IDText = 'Numb1+Numb2' then
  begin
     numb1 := RecordData.FieldValue['Number1'];
     numb2 := RecordData.FieldValue['Number2'];
     if not VarIsNull(numb1) and not VarIsNull(numb2) then
     begin
       CalculatedValue := numb1 + numb2;
       CalculatedValueType := ftInteger;
     end;
  end;
end;

initialization
  {$i Umegademo.lrs}

end.
