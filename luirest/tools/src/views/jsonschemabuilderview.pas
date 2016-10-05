unit JSONSchemaBuilderView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, VirtualTrees,
  JSONSchemaBuilder, fpjson;

type

  { TJSONSchemaBuilderForm }

  TJSONSchemaBuilderForm = class(TForm)
    CancelButton: TBitBtn;
    SaveButton: TBitBtn;
    PrimitiveListView: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure PrimitiveListViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PrimitiveListViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure PrimitiveListViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FBuilder: TJSONSchemaBuilder;
    FData: TJSONData;
    FSchemaData: TJSONObject;
    FPrimitives: TStringList;
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
  LuiJSONHelpers;

{$R *.lfm}

{ TJSONSchemaBuilderForm }

procedure TJSONSchemaBuilderForm.FormShow(Sender: TObject);
begin
  FSchemaData := FBuilder.Build(FData);
  PrimitiveListView.RootNodeCount := FPrimitives.Count;
end;

procedure TJSONSchemaBuilderForm.PrimitiveListViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  DefinitionData: TJSONObject;
  PropertyType: String;
begin
  DefinitionData := TJSONObject(FPrimitives.Objects[Node^.Index]);
  PropertyType := PrimitiveListView.Text[Node, 1];
  if Node^.CheckState = csCheckedNormal then
    DefinitionData.Arrays['type'] := TJSONArray.Create([PropertyType, 'null'])
  else
    DefinitionData.Strings['type'] := PropertyType;
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
begin
  Node^.CheckType := ctCheckBox;
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

