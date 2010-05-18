unit LuiDBDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Controls, VirtualDBGrid;

type
  TDataModification = (dmAdd, dmDelete, dmUpdate);
  TDataModifications = set of TDataModification;

  TDataDialogInfo = record
    FieldNames: String;
    FieldWidths: array of Integer;
    Title: String;
  end;

function EditDataSet(ADataSet: TDataSet; AParent: TWinControl; const DialogInfo: TDataDialogInfo): TDataModifications;

implementation

uses
  fEditDataSet, Math;

function EditDataSet(ADataSet: TDataSet; AParent: TWinControl;
  const DialogInfo: TDataDialogInfo): TDataModifications;

  function GetDefaultFieldWidth(FieldType: TFieldType): Integer;
  begin
    case FieldType of
      ftInteger, ftLargeint, ftWord:
        Result := 20;
      ftFloat, ftCurrency:
        Result := 30;
      ftDate, ftDateTime, ftTime:
        Result := 40;
      else
        Result := 120;
    end;
  end;

  procedure SetupGrid(AGrid: TVirtualDBGrid);
  var
    Fields: TList;
    Field: TField;
    i, FieldWidth, TotalFieldWidth: Integer;
  begin
    Fields := TList.Create;
    try
      ADataSet.GetFieldList(Fields, DialogInfo.FieldNames);

      if Fields.Count > 0 then
      begin
        TotalFieldWidth := 0;
        for i := 0 to Fields.Count - 1 do
        begin
          Field := TField(Fields[i]);
          FieldWidth := IfThen(i > High(DialogInfo.FieldWidths),
            GetDefaultFieldWidth(Field.DataType), DialogInfo.FieldWidths[i]);
          Inc(TotalFieldWidth, FieldWidth);
          AGrid.AddDBColumn(Field.FieldName, Field.DisplayLabel, FieldWidth);
        end;
        AGrid.Width := Max(TotalFieldWidth + 20, AGrid.Width);
      end;
    finally
      Fields.Destroy;
    end;
  end;

begin
  with TEditDataSourceForm.Create(AParent) do
  try
    Caption := DialogInfo.Title;
    SetupGrid(Grid);
    DataSet := ADataSet;
    ShowModal;
    Result := Modifications;
  finally
    Destroy;
  end;
end;

end.

