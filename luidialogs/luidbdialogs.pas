unit LuiDBDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Controls;

type
  TDataModification = (dmAdd, dmDelete, dmUpdate);
  TDataModifications = set of TDataModification;

function EditDataSet(ADataSet: TDataSet; AParent: TWinControl; const DialogInfo: String): TDataModifications;

implementation

uses
  fEditDataSet, LuiJSONUtils, fpjson;

function EditDataSet(ADataSet: TDataSet; AParent: TWinControl;
  const DialogInfo: String): TDataModifications;

var
  DialogData: TJSONData;
  EditForm: TEditDataSourceForm;

  function GetDefaultFieldWidth(FieldType: TFieldType): Integer;
  begin
    case FieldType of
      ftInteger, ftLargeint, ftWord, ftAutoInc:
        Result := 30;
      ftFloat, ftCurrency:
        Result := 30;
      ftDate, ftDateTime, ftTime:
        Result := 40;
      else
        Result := 120;
    end;
  end;

  procedure AddField(Field: TField; Title: String; Width: Integer);
  begin
    if Field = nil then
      Exit;
    if Width = -1 then
      Width := GetDefaultFieldWidth(Field.DataType);
    if Title = '' then
      Title := Field.DisplayLabel;
    EditForm.Grid.AddDBColumn(Field.FieldName, Title, Width);
  end;

  procedure ParseFieldData(const FieldData: TJSONData);
  var
    FieldObject: TJSONObject absolute FieldData;
    i: Integer;
  begin
    case FieldData.JSONType of
      //passed only one field
      jtString:
        begin
          AddField(ADataSet.FindField(FieldData.AsString), '', -1);
        end;
      //passed an array of objects or strings
      jtArray:
        begin
          for i := 0 to FieldData.Count - 1 do
            ParseFieldData(FieldData.Items[i]);
        end;
      //passed as an object
      jtObject:
        begin
          AddField(ADataSet.FindField(GetJSONProp(FieldObject, 'name', '')),
            GetJSONProp(FieldObject, 'title', ''),
            GetJSONProp(FieldObject, 'width', -1));
        end;
    end;
  end;

  procedure ParseDialogData;
  var
    FieldData: TJSONData;
    DialogObject: TJSONObject absolute DialogData;
  begin
    case DialogData.JSONType of
      jtObject:
        begin
          FieldData := GetJSONProp(DialogObject, 'fields');
          EditForm.Caption := GetJSONProp(DialogObject, 'title', EditForm.Caption);
          EditForm.EditorClassName := GetJSONProp(DialogObject, 'editor', '');
          EditForm.AppendEditorClassName := GetJSONProp(DialogObject, 'appendeditor', '');
        end;
      jtArray, jtString:
        FieldData := DialogData;
    end;
    if FieldData = nil then
      raise Exception.Create('EditDataSet - Field Data Not Set');
    ParseFieldData(FieldData);
  end;

begin
  DialogData := nil;
  try
    DialogData := StrToJSON(DialogInfo);
    EditForm := TEditDataSourceForm.Create(AParent);
    with EditForm do
    try
      if DialogData <> nil then
        ParseDialogData;
      DataSet := ADataSet;
      ShowModal;
      Result := Modifications;
    finally
      Destroy;
    end;
  finally
    FreeAndNil(DialogData);
  end;
end;

end.

