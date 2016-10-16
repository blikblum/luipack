unit JSONSchemaBuilderView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, VirtualTrees,
  JSONSchemaBuilder, fpjson, StdCtrls;

type

  { TJSONSchemaBuilderForm }

  TJSONSchemaBuilderForm = class(TForm)
    CancelButton: TBitBtn;
    SaveButton: TBitBtn;
    PrimitiveListView: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure PrimitiveListViewBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure PrimitiveListViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PrimitiveListViewCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure PrimitiveListViewEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure PrimitiveListViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure PrimitiveListViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure PrimitiveListViewNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure SaveButtonClick(Sender: TObject);
  private
    FBuilder: TJSONSchemaBuilder;
    FData: TJSONData;
    FSchemaData: TJSONObject;
    FPrimitives: TStringList;
    function HasNullTypes: Boolean;
    procedure UpdateDefinitionType(Node: PVirtualNode; const PropertyType: String);
    procedure PrimitiveProperty(const Path: String; DefinitionData: TJSONObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateSchema(Data: TJSONData): TJSONObject;
  end;

var
  JSONSchemaBuilderForm: TJSONSchemaBuilderForm;

implementation

uses
  LuiJSONHelpers, VTComboEditLink;

{$R *.lfm}

{ TJSONSchemaBuilderForm }

procedure TJSONSchemaBuilderForm.FormShow(Sender: TObject);
begin
  FSchemaData := FBuilder.Build(FData);
  PrimitiveListView.RootNodeCount := FPrimitives.Count;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; var ContentRect: TRect);
begin
  case Column of
    1:
      begin
        if PrimitiveListView.Text[Node, 1] = 'null' then
        begin
          TargetCanvas.Brush.Color := clRed;
          TargetCanvas.FillRect(CellRect);
        end;
      end;
  end;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateDefinitionType(Node, PrimitiveListView.Text[Node, 1]);
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  TypeLink: TVTComboEditLink;
begin
  case Column of
    1:
      begin
        TypeLink := TVTComboEditLink.Create;
        TypeLink.Combo.Items.AddStrings(['string', 'number', 'integer', 'boolean']);
        TypeLink.Combo.Style := csDropDownList;
        EditLink := TypeLink;
      end;
  end;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 1;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  DefinitionData: TJSONObject;
begin
  case Column of
    0: CellText := FPrimitives[Node^.Index];
    1:
      begin
        DefinitionData := TJSONObject(FPrimitives.Objects[Node^.Index]);
        if not DefinitionData.Find('type', CellText) and not DefinitionData.FindPath('type[0]', CellText) then
          CellText := '';
      end;
    2:
      begin
        CellText := '';//BoolToStr(Sender.CheckState[Node] = csCheckedNormal, True);
      end;
  end;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  DefinitionData: TJSONObject;
  TypeData: TJSONArray;
begin
  Node^.CheckType := ctCheckBox;
  DefinitionData := TJSONObject(FPrimitives.Objects[Node^.Index]);
  if DefinitionData.Find('type', TypeData) and (TypeData.IndexOf('null') > -1) then
    Node^.CheckState := csCheckedNormal;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  case Column of
    1: UpdateDefinitionType(Node, NewText);
  end;
end;

procedure TJSONSchemaBuilderForm.SaveButtonClick(Sender: TObject);
var
  CanClose: Boolean;
begin
  CanClose := not HasNullTypes;
  if not CanClose then
  begin
    CanClose := MessageDlg('Confirm close', 'There are fields with null as type' +
      LineEnding + 'Do you want to continue?', mtConfirmation, mbYesNo, 0) = mrYes;
  end;
  if CanClose then
    ModalResult := mrOK;
end;

function TJSONSchemaBuilderForm.HasNullTypes: Boolean;
var
  Node: PVirtualNode;
begin
  Result := False;
  Node := PrimitiveListView.GetFirst;
  while Node <> nil do
  begin
    Result := Result or (PrimitiveListView.Text[Node, 1] = 'null');
    Node := PrimitiveListView.GetNext(Node);
  end;
end;

procedure TJSONSchemaBuilderForm.UpdateDefinitionType(Node: PVirtualNode; const PropertyType: String);
var
  DefinitionData: TJSONObject;
begin
  DefinitionData := TJSONObject(FPrimitives.Objects[Node^.Index]);
  if Node^.CheckState = csCheckedNormal then
    DefinitionData.Arrays['type'] := TJSONArray.Create([PropertyType, 'null'])
  else
    DefinitionData.Strings['type'] := PropertyType;
end;

procedure TJSONSchemaBuilderForm.PrimitiveProperty(const Path: String; DefinitionData: TJSONObject);
begin
  FPrimitives.AddObject(Path, DefinitionData);
end;

constructor TJSONSchemaBuilderForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBuilder := TJSONSchemaBuilder.Create;
  FBuilder.OnPrimitiveProperty := @PrimitiveProperty;
  FPrimitives := TStringList.Create;
end;

destructor TJSONSchemaBuilderForm.Destroy;
begin
  FSchemaData.Free;
  FPrimitives.Destroy;
  FBuilder.Destroy;
  inherited Destroy;
end;

class function TJSONSchemaBuilderForm.CreateSchema(Data: TJSONData): TJSONObject;
var
  Instance: TJSONSchemaBuilderForm;
begin
  Result := nil;
  Instance := TJSONSchemaBuilderForm.Create(nil);
  try
    Instance.FData := Data;
    if Instance.ShowModal = mrOK then
    begin
      Result := Instance.FSchemaData;
      Instance.FSchemaData := nil;
    end;
  finally
    Instance.Destroy;
  end;
end;

end.

