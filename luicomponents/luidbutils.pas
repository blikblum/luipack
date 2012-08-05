unit LuiDBUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

procedure CopyDatasetData(SourceDataset, DestDataset: TDataset);

procedure DatasetToStrings(Dataset: TDataset; Strings: TStrings; const FieldNames: string; Clear: Boolean = False);

procedure DeleteCurrentRecord(Dataset: TDataSet);

implementation

uses
  LuiStrUtils;

procedure DatasetToStrings(Dataset: TDataset; Strings: TStrings; const FieldNames: string; Clear: Boolean);
var
  MainFieldName, ObjFieldName: string;
  MainField, ObjField: TField;
  ABookmark: TBookmark;
begin
  ExtractNameValue(FieldNames, MainFieldName, ObjFieldName);
  MainField := Dataset.FieldByName(MainFieldName);
  ObjField := Dataset.FindField(ObjFieldName);
  Strings.BeginUpdate;
  try
    if Clear then
      Strings.Clear;
    if not Dataset.IsEmpty then
    begin
      Dataset.DisableControls;
      ABookmark := Dataset.GetBookmark;
      try
        Dataset.First;
        while not Dataset.EOF do
        begin
          if ObjField = nil then
            Strings.Add(MainField.AsString)
          else
            Strings.AddObject(MainField.AsString, TObject(ObjField.AsInteger));
          Dataset.Next;
        end;
      finally
        Dataset.GotoBookmark(ABookmark);
        Dataset.FreeBookmark(ABookmark);
        Dataset.EnableControls;
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

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

