unit VirtualDBCheckGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualDBGrid, db, VirtualTrees;

type

  TVirtualDBCheckGroupDataLink = class;

  { TVirtualDBCheckGroup }

  TVirtualDBCheckGroup = class(TCustomVirtualDBGrid)
  private
    FCheckDataLink: TVirtualDBCheckGroupDataLink;
    FKeyField: String;
    FCheckField: String;
    FUpdatingCheckState: Boolean;
    procedure Changed;
    procedure CheckDataLinkActiveChanged;
    procedure CheckDataLinkDatasetChanged;
    function FindNodeByValue(const FieldName: String; Value: Variant): PVirtualNode;
    function GetCheckSource: TDataSource;
    function GetNodeFieldValue(Node: PVirtualNode; const FieldName: String): Variant;
    procedure SetCheckSource(Value: TDataSource);
    procedure SetKeyField(const Value: String);
    procedure SetCheckField(const Value: String);
    procedure UpdateCheckStates;
  protected
    procedure DataLinkDatasetChanged; override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoInitNode(AParent, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CheckField: String read FCheckField write SetCheckField;
    property CheckSource: TDataSource read GetCheckSource write SetCheckSource;
    property KeyField: String read FKeyField write SetKeyField;
    //
    property BorderSpacing;
    property DBOptions;
    property OnGetRecordCount;
    property OnCalculateValue;
    property OnFormatFieldValue;
    property OnLoadRecord;
    property OnCustomSort;
    property OnPostChanges;
    property OnChangeSort;
    property OnCompareRecord;
    property OnRecordDblClick;
  end;

  { TVirtualDBCheckGroupDataLink }

  TVirtualDBCheckGroupDataLink = class(TDataLink)
  private
    FGrid: TVirtualDBCheckGroup;
  public
    constructor Create(Grid: TVirtualDBCheckGroup);
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    //procedure RecordChanged(Field: TField); override;
    //procedure DataSetScrolled(Distance: Integer); override;
  end;


implementation

uses
  Variants;

{ TVirtualDBCheckGroupDataLink }

constructor TVirtualDBCheckGroupDataLink.Create(Grid: TVirtualDBCheckGroup);
begin
  FGrid := Grid;
  VisualControl := True;
end;

procedure TVirtualDBCheckGroupDataLink.ActiveChanged;
begin
  if not (csLoading in FGrid.ComponentState) then
    FGrid.CheckDataLinkActiveChanged;
end;

procedure TVirtualDBCheckGroupDataLink.DataSetChanged;
begin
  if not (csLoading in FGrid.ComponentState) then
    FGrid.CheckDataLinkDatasetChanged;
end;

{ TVirtualDBCheckGroup }

procedure TVirtualDBCheckGroup.SetCheckSource(Value: TDataSource);
begin
  FCheckDataLink.DataSource := Value;
end;

procedure TVirtualDBCheckGroup.Changed;
begin
  if not (csLoading in ComponentState) then
    UpdateCheckStates;
end;

procedure TVirtualDBCheckGroup.CheckDataLinkActiveChanged;
begin
  UpdateCheckStates;
end;

procedure TVirtualDBCheckGroup.CheckDataLinkDatasetChanged;
begin
  UpdateCheckStates;
end;

function TVirtualDBCheckGroup.FindNodeByValue(const FieldName: String;
  Value: Variant): PVirtualNode;
var
  RecNo: Integer;
begin
  if LinkedDataSet.Locate(FieldName, Value, []) then
  begin
    RecNo := LinkedDataSet.RecNo;
    Result := FindNodeByRecNo(RecNo);
  end
  else
    Result := nil;
end;

function TVirtualDBCheckGroup.GetCheckSource: TDataSource;
begin
  Result := FCheckDataLink.DataSource;
end;

function TVirtualDBCheckGroup.GetNodeFieldValue(Node: PVirtualNode;
  const FieldName: String): Variant;
var
  Data: TRecordData;
  OldRecNo: Integer;
begin
  OldRecNo := LinkedDataSet.RecNo;
  LinkedDataSet.DisableControls;
  try
    Data := GetNodeRecordData(Node);
    LinkedDataSet.RecNo := Data.RecNo;
    Result := LinkedDataSet.FieldByName(FieldName).Value;
  finally
    LinkedDataSet.RecNo := OldRecNo;
    LinkedDataSet.EnableControls;
  end;
end;

procedure TVirtualDBCheckGroup.SetKeyField(const Value: String);
begin
  if FKeyField = Value then Exit;
  FKeyField := Value;
  Changed;
end;

procedure TVirtualDBCheckGroup.SetCheckField(const Value: String);
begin
  if FCheckField = Value then Exit;
  FCheckField := Value;
  Changed;
end;

procedure TVirtualDBCheckGroup.UpdateCheckStates;
var
  CheckDataset: TDataset;
  OldCheckRecNo, OldMainRecNo: Integer;
  Node: PVirtualNode;
begin
  if FUpdatingCheckState then
    Exit;
  FUpdatingCheckState := True;
  try
    ClearChecked;
    if FCheckDataLink.Active then
    begin
      CheckDataset := FCheckDataLink.DataSet;
      if CheckDataset.RecordCount > 0 then
      begin
        OldCheckRecNo := CheckDataset.RecNo;
        OldMainRecNo := LinkedDataSet.RecNo;
        //todo: use BlockRead??
        LinkedDataSet.DisableControls;
        CheckDataset.DisableControls;
        try
          CheckDataset.First;
          while not CheckDataset.EOF do
          begin
            //todo cache FieldByName
            Node := FindNodeByValue(FKeyField, CheckDataset.FieldByName(FCheckField).Value);
            if Node <> nil then
              CheckState[Node] := csCheckedNormal;
            CheckDataset.Next;
          end;
        finally
          CheckDataset.RecNo := OldCheckRecNo;
          if OldMainRecNo <> -1 then
            LinkedDataSet.RecNo := OldMainRecNo;
          LinkedDataSet.EnableControls;
          CheckDataset.EnableControls;
        end;
      end;
    end;
  finally
    FUpdatingCheckState := False;
  end;
end;

procedure TVirtualDBCheckGroup.DataLinkDatasetChanged;
begin
  inherited DataLinkDatasetChanged;
  if LinkedDataSet.State <> dsInsert then
    UpdateCheckStates;
end;

procedure TVirtualDBCheckGroup.DoChecked(Node: PVirtualNode);
var
  OldRecNo: Integer;
begin
  if not FUpdatingCheckState and FCheckDataLink.Active then
  begin
    OldRecNo := FCheckDataLink.DataSet.RecNo;
    FUpdatingCheckState := True;
    FCheckDataLink.DataSet.DisableControls;
    try
      if Node^.CheckState = csCheckedNormal then
      begin
        FCheckDataLink.DataSet.Append;
        FCheckDataLink.DataSet.FieldByName(FCheckField).Value := GetNodeFieldValue(Node, FKeyField);
        FCheckDataLink.DataSet.Post;
      end
      else
      begin
        if FCheckDataLink.DataSet.Locate(FCheckField, GetNodeFieldValue(Node, FKeyField), []) then
        begin
          if FCheckDataLink.DataSet.RecNo <= OldRecNo then
            Dec(OldRecNo);
          FCheckDataLink.DataSet.Delete;
        end;
      end;
    finally
      if OldRecNo > 0 then
        FCheckDataLink.DataSet.RecNo := OldRecNo;
      FCheckDataLink.DataSet.EnableControls;
      FUpdatingCheckState := False;
    end;
  end;
  inherited DoChecked(Node);
end;

procedure TVirtualDBCheckGroup.DoInitNode(AParent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  inherited DoInitNode(AParent, Node, InitStates);
  Node^.CheckType := ctCheckBox;
end;

procedure TVirtualDBCheckGroup.Loaded;
begin
  inherited Loaded;
  Changed;
end;

constructor TVirtualDBCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckDataLink := TVirtualDBCheckGroupDataLink.Create(Self);
  Header.Options := Header.Options - [hoVisible];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toAlwaysHideSelection] - [toHideFocusRect];
end;

destructor TVirtualDBCheckGroup.Destroy;
begin
  FCheckDataLink.Destroy;
  inherited Destroy;
end;

end.

