unit JSONCollectionView;

{$mode objfpc}{$H+}
//todo: add proper internacionalization code
{$define USE_PORTUGUESE}

//todo: configure colums header

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, VTJSON, fpjson, VirtualTrees, LuiJSONModel, PresentationManager;

type

  { TJSONCollectionForm }

  TJSONCollectionForm = class(TForm, IFPObserver)
    AddButton: TBitBtn;
    DeleteButton: TBitBtn;
    CloseButton: TBitBtn;
    EditButton: TBitBtn;
    ListView: TVirtualJSONListView;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAddPresentation: String;
    FAddViewClass: TFormClass;
    FCollection: TJSONCollection;
    FCollectionClass: TJSONCollectionClass;
    FEditPresentation: String;
    FEditViewClass: TFormClass;
    FModelProperty: String;
    FPresentation: String;
    FPresentations: IPresentationManager;
    FOwnsCollection: Boolean;
    FViewClass: TFormClass;
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    procedure PresentationsNeeded;
    procedure SetPresentation(const Value: String);
    procedure SetViewClass(Value: TFormClass);
    procedure ShowPresentation(Model: TJSONModel; const PresentationName: String);
    procedure ShowView(Model: TJSONModel; ViewClass: TFormClass);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure Initialize;
    property AddPresentation: String read FAddPresentation write FAddPresentation;
    property AddViewClass: TFormClass read FAddViewClass write FAddViewClass;
    property Collection: TJSONCollection read FCollection write FCollection;
    property CollectionClass: TJSONCollectionClass read FCollectionClass write FCollectionClass;
    property EditPresentation: String read FEditPresentation write FEditPresentation;
    property EditViewClass: TFormClass read FEditViewClass write FEditViewClass;
    property ModelProperty: String read FModelProperty write FModelProperty;
    property Presentation: String read FPresentation write SetPresentation;
    property ViewClass: TFormClass read FViewClass write SetViewClass;
  end;

implementation

uses
  LuiServices, LuiDialogs;

{$R *.lfm}

var
  strAddRecord: String = 'Add';
  strEditRecord: String = 'Edit';
  strDeleteRecord: String = 'Remove';
  strCloseDialog: String = 'Close';


procedure LoadPortugueseStrings;
begin
  strAddRecord := 'Adicionar';
  strEditRecord := 'Editar';
  strDeleteRecord := 'Excluir';
  strCloseDialog := 'Fechar';
end;

{ TJSONCollectionForm }

procedure TJSONCollectionForm.AddButtonClick(Sender: TObject);
var
  Model: TJSONModel;
begin
  PresentationsNeeded;
  Model := FCollection.ItemClass.Create;
  if FAddPresentation <> '' then
    ShowPresentation(Model, FAddPresentation)
  else if FAddViewClass <> nil then
    ShowView(Model, FAddViewClass)
  else
    raise Exception.Create('TJSONCollection - No presentation or viewclass defined');
  if not Model.IsNew then
    FCollection.Add(Model)
  else
    Model.Destroy;
end;

procedure TJSONCollectionForm.DeleteButtonClick(Sender: TObject);
var
  Model: TJSONModel;
  ModelData: TJSONObject;
begin
  if ListView.GetData(ListView.FocusedNode, ModelData) then
  begin
    Model := FCollection.Get(ModelData);
    //todo: make delete a function
    Model.Delete;
  end;
end;

procedure TJSONCollectionForm.EditButtonClick(Sender: TObject);
var
  Model: TJSONModel;
  ModelData: TJSONObject;
begin
  if ListView.GetData(ListView.FocusedNode, ModelData) then
  begin
    PresentationsNeeded;
    Model := FCollection.Get(ModelData);
    if FEditPresentation <> '' then
      ShowPresentation(Model, FEditPresentation)
    else if FEditViewClass <> nil then
      ShowView(Model, FEditViewClass)
    else
      raise Exception.Create('TJSONCollection - No presentation or viewclass defined');
  end;
end;

procedure TJSONCollectionForm.FormCreate(Sender: TObject);
begin
  AddButton.Caption := strAddRecord;
  DeleteButton.Caption := strDeleteRecord;
  EditButton.Caption := strEditRecord;
  CloseButton.Caption := strCloseDialog;
end;

procedure TJSONCollectionForm.ShowPresentation(Model: TJSONModel;
  const PresentationName: String);
begin
  FPresentations[PresentationName].ShowModal([FModelProperty, Model]);
end;

procedure TJSONCollectionForm.ShowView(Model: TJSONModel; ViewClass: TFormClass);
begin
  ShowForm(FViewClass, [FModelProperty, Model]);
end;

constructor TJSONCollectionForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModelProperty := 'Model';
end;

procedure TJSONCollectionForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = FCollection then
  begin
    case Operation of
      ooChange, ooAddItem, ooDeleteItem:
        begin
          if (Data = nil) or (Operation in [ooAddItem, ooDeleteItem]) then
          begin
            ListView.Data := Collection.Data;
            ListView.LoadData;
          end
          else
            ListView.Invalidate;
        end;
    end;
  end;
end;

procedure TJSONCollectionForm.PresentationsNeeded;
begin
  if FPresentations = nil then
    Services.Resolve(IPresentationManager, FPresentations);
end;

procedure TJSONCollectionForm.SetPresentation(const Value: String);
begin
  FPresentation := Value;
  if FAddPresentation = '' then
    FAddPresentation := Value;
  if FEditPresentation = '' then
    FEditPresentation := Value;
end;

procedure TJSONCollectionForm.SetViewClass(Value: TFormClass);
begin
  FViewClass := Value;
  if FAddViewClass = nil then
    FAddViewClass := Value;
  if FEditViewClass = nil then
    FEditViewClass := Value;
end;

destructor TJSONCollectionForm.Destroy;
begin
  FCollection.FPODetachObserver(Self);
  if FOwnsCollection then
    FCollection.Free;
  inherited Destroy;
end;

procedure TJSONCollectionForm.Initialize;
begin
  if FCollection = nil then
  begin
    if FCollectionClass = nil then
      raise Exception.Create('JSONCollectionView - Collection or CollectionClass must be defined');
    FCollection := FCollectionClass.Create;
    FCollection.Fetch;
    FOwnsCollection := True;
  end;
  ListView.Data := FCollection.Data;
  ListView.LoadData;
  FCollection.FPOAttachObserver(Self);
end;

initialization
  {$ifdef USE_PORTUGUESE}
  LoadPortugueseStrings;
  {$endif}

end.

