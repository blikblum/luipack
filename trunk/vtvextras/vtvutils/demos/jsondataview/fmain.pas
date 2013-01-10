unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  ExtCtrls, StdCtrls, Grids, ComCtrls, VTJSON, fpjson, jsonparser, jsonscanner,
  VirtualTrees;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExpandButton: TButton;
    CollapseButton: TButton;
    Splitter1: TSplitter;
    TreeViewTabSheet: TTabSheet;
    ViewsPageControl: TPageControl;
    PropertiesGrid: TStringGrid;
    ListViewTabSheet: TTabSheet;
    UseColumnsCheckBox: TCheckBox;
    TextPropertyLabel: TLabel;
    PropertiesComboBox: TComboBox;
    LogMemo: TMemo;
    ShowCheckedButton: TButton;
    EnableCheckItemsCheckBox: TCheckBox;
    FileNameEdit: TFileNameEdit;
    JSONListView: TVirtualJSONListView;
    JSONTreeView: TVirtualJSONTreeView;
    procedure CollapseButtonClick(Sender: TObject);
    procedure ExpandButtonClick(Sender: TObject);
    procedure PropertiesGridCheckboxToggled(sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure PropertiesGridEditingDone(Sender: TObject);
    procedure TextPropertyEditEditingDone(Sender: TObject);
    procedure EnableCheckItemsCheckBoxChange(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FormDestroy(Sender: TObject);
    procedure ShowCheckedButtonClick(Sender: TObject);
    procedure UseColumnsCheckBoxChange(Sender: TObject);
  private
    { private declarations }
    JSONData: TJSONData;
    procedure LoadPropertyNames;
    procedure UpdateDataViewsColumns;
    procedure UpdatePropertiesDisplay;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Data: TJSONData;
begin
  Stream := nil;
  Parser := nil;
  Data := nil;
  try
    try
      Stream := TFileStream.Create(Value, fmOpenRead);
      Parser := TJSONParser.Create(Stream);
      Data := Parser.Parse;
    finally
      Parser.Free;
      Stream.Free;
    end;
  except
    on E: EFOpenError do
      ShowMessageFmt('Error opening "%s" : %s', [Value, E.Message]);
    on E: EParserError do
    begin
      ShowMessageFmt('Error parsing "%s" : %s', [Value, E.Message]);
    end;
  end;
  if Data <> nil then
  begin
    if Data.JSONType in [{jtObject,} jtArray] then
    begin
      JSONData.Free;
      JSONData := Data;
      LoadPropertyNames;
      UpdatePropertiesDisplay;
      JSONListView.Data := JSONData;
      JSONListView.LoadData;
      JSONTreeView.Data := JSONData;
      JSONTreeView.LoadData;
    end
    else
    begin
      ShowMessageFmt('Expecting a TJSONArray got "%s"', [Data.ClassName]);
      Data.Destroy;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  JSONData.Free;
end;

procedure TMainForm.ShowCheckedButtonClick(Sender: TObject);
begin
  if ViewsPageControl.ActivePageIndex = 0 then
    LogMemo.Lines.Text := JSONListView.CheckedData.FormatJSON([foSingleLineObject])
  else
    LogMemo.Lines.Text := JSONTreeView.CheckedData.FormatJSON([foSingleLineObject]);
end;

procedure TMainForm.UseColumnsCheckBoxChange(Sender: TObject);
begin
  UpdatePropertiesDisplay;
end;

procedure TMainForm.LoadPropertyNames;
var
  JSONObject: TJSONObject;
  PropertyName: String;
  i: Integer;
begin
  PropertiesComboBox.Clear;
  PropertiesGrid.RowCount := 1;
  if (JSONData.JSONType = jtArray) and (JSONData.Count > 0) then
  begin
    if JSONData.Items[0].JSONType = jtObject then
    begin
      JSONObject := TJSONObject(JSONData.Items[0]);
      for i := 0 to JSONObject.Count -1 do
      begin
        PropertiesGrid.RowCount := PropertiesGrid.RowCount + 1;
        PropertyName := JSONObject.Names[i];
        PropertiesComboBox.Items.Add(PropertyName);
        PropertiesGrid.Cells[0, i + 1] := '1';
        PropertiesGrid.Cells[1, i + 1] := PropertyName;
        PropertiesGrid.Cells[2, i + 1] := PropertyName;
      end;
    end;
  end;
  if PropertiesComboBox.Items.Count > 0 then
  begin
    PropertiesComboBox.ItemIndex := 0;
    JSONListView.TextProperty := PropertiesComboBox.Text;
    JSONTreeView.TextProperty := PropertiesComboBox.Text;
  end;
end;

procedure TMainForm.UpdateDataViewsColumns;
var
  i: Integer;
  Column: TVirtualJSONDataViewColumn;
begin
  JSONTreeView.Header.Columns.BeginUpdate;
  JSONListView.Header.Columns.BeginUpdate;
  for i := 1 to PropertiesGrid.RowCount -1 do
  begin
    if PropertiesGrid.Cells[0, i] = '1' then
    begin
      Column := TVirtualJSONDataViewColumn(JSONListView.Header.Columns.Add);
      Column.PropertyName := PropertiesGrid.Cells[1, i];
      Column.Text := PropertiesGrid.Cells[2, i];
      Column.Width := 64;

      Column := TVirtualJSONDataViewColumn(JSONTreeView.Header.Columns.Add);
      Column.PropertyName := PropertiesGrid.Cells[1, i];
      Column.Text := PropertiesGrid.Cells[2, i];
      Column.Width := 64;
    end;
  end;
  JSONListView.Header.Columns.EndUpdate;
  JSONTreeView.Header.Columns.EndUpdate;
end;

procedure TMainForm.UpdatePropertiesDisplay;
var
  UseColumns: Boolean;
begin
  UseColumns := UseColumnsCheckBox.Checked;
  PropertiesGrid.Visible := UseColumns;
  PropertiesComboBox.Visible := not UseColumns;
  TextPropertyLabel.Visible := not UseColumns;
  JSONListView.Header.Columns.Clear;
  JSONTreeView.Header.Columns.Clear;
  if UseColumns then
  begin
    JSONListView.Header.Options := JSONListView.Header.Options + [hoVisible];
    JSONTreeView.Header.Options := JSONListView.Header.Options + [hoVisible];
    UpdateDataViewsColumns;
  end
  else
  begin
    JSONListView.Header.Options := JSONListView.Header.Options - [hoVisible];
    JSONTreeView.Header.Options := JSONListView.Header.Options - [hoVisible];
  end;
end;

procedure TMainForm.TextPropertyEditEditingDone(Sender: TObject);
begin
  JSONListView.TextProperty := PropertiesComboBox.Text;
  JSONTreeView.TextProperty := PropertiesComboBox.Text;
end;

procedure TMainForm.PropertiesGridCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  JSONListView.Header.Columns.Clear;
  JSONTreeView.Header.Columns.Clear;
  UpdateDataViewsColumns;
end;

procedure TMainForm.CollapseButtonClick(Sender: TObject);
begin
  JSONTreeView.FullCollapse;
end;

procedure TMainForm.ExpandButtonClick(Sender: TObject);
begin
  JSONTreeView.FullExpand;
end;

procedure TMainForm.PropertiesGridEditingDone(Sender: TObject);
begin
  JSONListView.Header.Columns.Clear;
  JSONTreeView.Header.Columns.Clear;
  UpdateDataViewsColumns;
end;

procedure TMainForm.EnableCheckItemsCheckBoxChange(Sender: TObject);
begin
  if EnableCheckItemsCheckBox.Checked then
  begin
    JSONListView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions + [toCheckSupport];
    JSONTreeView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions + [toCheckSupport];
  end
  else
  begin
    JSONListView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions - [toCheckSupport];
    JSONTreeView.TreeOptions.MiscOptions := JSONListView.TreeOptions.MiscOptions - [toCheckSupport];
  end;
end;

end.

