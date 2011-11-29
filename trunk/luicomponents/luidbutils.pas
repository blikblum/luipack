unit LuiDBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

procedure DeleteCurrentRecord(Dataset: TDataSet);

procedure CopyDatasetData(SourceDataset, DestDataset: TDataset);

implementation

procedure DeleteCurrentRecord(Dataset: TDataSet);
begin
  if not Dataset.IsEmpty then
    Dataset.Delete;
end;

procedure CopyDatasetData(SourceDataset, DestDataset: TDataset);
var
  SrcFields, DestFields: TFPList;
  SrcField, DestField: TField;
  FieldName: String;
  i, OldRecNo: Integer;
begin
  //todo add checks (open / assigned)
  // based in TMemDataset.CopyFromDataset
  SrcFields := TFPList.Create;
  DestFields := TFPList.Create;
  try
    for i := 0 to DestDataset.FieldDefs.Count - 1 do
    begin
      FieldName := DestDataset.FieldDefs[i].Name;
      SrcField := SourceDataset.FieldByName(FieldName);
      DestField := DestDataset.FieldByName(FieldName);
      SrcFields.Add(SrcField);
      DestFields.Add(DestField);
    end;
    //todo: change to BlockRead ?? (requires fpc 2.6)
    SourceDataset.DisableControls;
    OldRecNo := SourceDataset.RecNo;
    try
      SourceDataset.First;
      while not SourceDataset.EOF do
      begin
        DestDataset.Append;
        for i := 0 to SrcFields.Count - 1 do
        begin
          SrcField := TField(SrcFields[i]);
          DestField := TField(DestFields[i]);
          DestField.Value := SrcField.Value;
        end;
        DestDataset.Post;
        SourceDataset.Next;
      end;
    finally
      if OldRecNo <> -1 then
        SourceDataset.RecNo := OldRecNo;
      SourceDataset.EnableControls;
    end;
  finally
    SrcFields.Destroy;
    DestFields.Destroy;
  end;
end;

end.

