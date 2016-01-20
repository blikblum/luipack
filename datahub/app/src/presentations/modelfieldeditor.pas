unit ModelFieldEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, DataModel, VTJSON, fpjson, VirtualTrees;

type

  { TModelFieldEditorForm }

  TModelFieldEditorForm = class(TForm)
    CancelButton: TBitBtn;
    DisplayLabelEdit: TTIEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SaveButton: TBitBtn;
    FieldNameEdit: TTIEdit;
    FieldTypeComboBox: TTIComboBox;
    ConstraintsInspector: TVirtualJSONInspector;
    procedure ConstraintsInspectorAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure ConstraintsInspectorClick(Sender: TObject);
    procedure ConstraintsInspectorFilterProperty(Sender: TVirtualJSONInspector;
      ParentData, ItemData: TJSONData; ItemIndex: Integer; var Accept: Boolean);
    procedure ConstraintsInspectorFormatValue(Sender: TVirtualJSONInspector;
      const PropName: String; PropData: TJSONData; var DisplayText: String);
    procedure FieldTypeComboBoxEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FEditField: TDataModelField;
    FEditConstraintsData: TJSONObject;
    procedure LoadConstraintsData;
    procedure SaveConstraintsData;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function EditField(Field: TDataModelField): Boolean;
    class function AddField(Fields: TDataModelFields): Boolean;
  end;

var
  ModelFieldEditorForm: TModelFieldEditorForm;

implementation

uses
  DataHubUtils, LuiJSONUtils, strutils, Themes, TmSchema;

{$R *.lfm}

{ TModelFieldEditorForm }

const
  NumberRules: array[0..1] of String = ('minimum', 'maximum');
  StringRules: array[0..2] of String = ('pattern', 'minLength', 'maxLength');

type
  TStringArray = array of String;

function GetRulesForFieldType(FieldType: TDataModelFieldType): TStringArray;
begin
  Result := nil;
  case FieldType of
     dftString, dftMemo:
       Result := StringRules;
     dftInteger, dftFloat, dftCurrency:
       Result := NumberRules;
   end;
   SetLength(Result, Length(Result) + 2);
   Result[Length(Result) - 2] := 'primaryKey';
   Result[Length(Result) - 1] := 'required';
end;

procedure TModelFieldEditorForm.ConstraintsInspectorFilterProperty(
  Sender: TVirtualJSONInspector; ParentData, ItemData: TJSONData;
  ItemIndex: Integer; var Accept: Boolean);
var
  PropName: String;
begin
  if ParentData.JSONType <> jtObject then
    Exit;
  PropName := TJSONObject(ParentData).Names[ItemIndex];
  Accept := AnsiMatchStr(PropName, GetRulesForFieldType(FEditField.FieldType));
end;

procedure TModelFieldEditorForm.ConstraintsInspectorFormatValue(
  Sender: TVirtualJSONInspector; const PropName: String; PropData: TJSONData;
  var DisplayText: String);
begin
  if PropData.JSONType in [jtBoolean, jtNull] then
    DisplayText := '';
end;

procedure TModelFieldEditorForm.ConstraintsInspectorAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellRect: TRect);
var
  PropData: TJSONData;
  PropName: String;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if Column = 1 then
  begin
    PropData := ConstraintsInspector.GetData(Node);
    PropName := ConstraintsInspector.GetName(Node);
    if AnsiMatchStr(PropName, ['required', 'primaryKey']) then
    begin
      Details.Element := teButton;
      Details.Part := BP_CHECKBOX;
      R := Rect(CellRect.Left + 10, CellRect.Top + 2, CellRect.Left + 26, CellRect.Top + 18);
      if PropData is TJSONBoolean then
      begin
        if PropData.AsBoolean then
          Details.State := 5
        else
          Details.State := 1;
      end
      else
        Details.State := 4;
      ThemeServices.DrawElement(TargetCanvas.Handle, Details, R);
    end;
  end;
end;

procedure TModelFieldEditorForm.ConstraintsInspectorClick(Sender: TObject);
var
  P: TPoint;
  PropData: TJSONData;
  Info: THitInfo;
  PropName: String;
begin
  P := ConstraintsInspector.ScreenToClient(Mouse.CursorPos);
  ConstraintsInspector.GetHitTestInfoAt(P.x, P.y, True, Info);
  if (Info.HitNode <> nil) and (Info.HitColumn = 1) then
  begin
    PropName := ConstraintsInspector.GetName(Info.HitNode);
    if AnsiMatchStr(PropName, ['required', 'primaryKey']) then
    begin
      PropData := ConstraintsInspector.GetData(Info.HitNode);
      if not (PropData is TJSONBoolean) or not PropData.AsBoolean then
        FEditConstraintsData.Booleans[PropName] := True
      else
        FEditConstraintsData.Booleans[PropName] := False;
      ConstraintsInspector.Reload;
    end;
  end;
end;

procedure TModelFieldEditorForm.FieldTypeComboBoxEditingDone(Sender: TObject);
begin
  ConstraintsInspector.Reload;
  ConstraintsInspector.Invalidate; //todo: should not be necessary Invalidate
end;

procedure TModelFieldEditorForm.FormShow(Sender: TObject);
begin
  SetRTTILinkObject(Self, FEditField);
  LoadConstraintsData;
end;

procedure TModelFieldEditorForm.SaveButtonClick(Sender: TObject);
begin
  SaveConstraintsData;
end;

procedure TModelFieldEditorForm.LoadConstraintsData;
begin
  if FEditField.ConstraintsData <> nil then
    CopyJSONObject(FEditField.ConstraintsData, FEditConstraintsData);
  ConstraintsInspector.RootData := FEditConstraintsData;
end;

procedure TModelFieldEditorForm.SaveConstraintsData;
var
  TmpData: TJSONObject;
begin
  TmpData := TJSONObject.Create;
  try
    CopyJSONObject(FEditConstraintsData, TmpData, GetRulesForFieldType(FEditField.FieldType));
    FEditField.Constraints := TmpData.AsJSON;
  finally
    TmpData.Destroy;
  end;
end;

constructor TModelFieldEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditField := TDataModelField.Create(nil);
  FEditConstraintsData := TJSONObject.Create(
    [
    'primaryKey', nil,
    'required', nil,
    'minimum', nil,
    'maximum', nil,
    'minLength', nil,
    'maxLength', nil,
    'pattern', nil
    ]
  );
end;

destructor TModelFieldEditorForm.Destroy;
begin
  FEditField.Destroy;
  FEditConstraintsData.Destroy;
  inherited Destroy;
end;

class function TModelFieldEditorForm.EditField(Field: TDataModelField): Boolean;
var
  Form: TModelFieldEditorForm;
begin
  Assert(Field <> nil, 'TModelFieldEditorForm.EditField Field = nil');
  Form := TModelFieldEditorForm.Create(nil);
  try
    Form.FEditField.Assign(Field);
    Result := Form.ShowModal = mrOK;
    if Result then
      Field.Assign(Form.FEditField);
  finally
    Form.Destroy;
  end;
end;

class function TModelFieldEditorForm.AddField(Fields: TDataModelFields): Boolean;
var
  Form: TModelFieldEditorForm;
  NewField: TDataModelField;
begin
  Assert(Fields <> nil, 'TModelFieldEditorForm.AddField Fields = nil');
  Form := TModelFieldEditorForm.Create(nil);
  try
    Form.FEditField.FieldName := 'NewField';
    Result := Form.ShowModal = mrOK;
    if Result then
    begin
      NewField := Fields.Add;
      NewField.Assign(Form.FEditField);
    end;
  finally
    Form.Destroy;
  end;
end;

end.

