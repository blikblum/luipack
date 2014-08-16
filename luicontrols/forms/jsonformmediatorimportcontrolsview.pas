unit JSONFormMediatorImportControlsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, VirtualTrees, JSONFormMediator;

type

  { TJSONFormMediatorImportControlsViewForm }

  TJSONFormMediatorImportControlsViewForm = class(TForm)
    CancelButton: TBitBtn;
    RecursiveCheckBox: TCheckBox;
    ImportButton: TBitBtn;
    ParentControlComboBox: TComboBox;
    Label1: TLabel;
    ControlListVew: TVirtualStringTree;
    procedure ControlListVewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure ControlListVewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormShow(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure ParentControlComboBoxChange(Sender: TObject);
  private
    FControlList: TFpList;
    FMediator: TJSONFormMediator;
    FTopLevelControl: TWinControl;
    procedure AddControlToList(Control: TControl);
    procedure AddControlToParentList(ParentControl: TWinControl);
    function CreateCheckedControlList: TFPList;
    procedure LoadParentControlList;
    procedure ParentControlChanged;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property TopLevelControl: TWinControl read FTopLevelControl write FTopLevelControl;
    property Mediator: TJSONFormMediator read FMediator write FMediator;
  end;

var
  JSONFormMediatorImportControlsViewForm: TJSONFormMediatorImportControlsViewForm;

implementation

{$R *.lfm}

uses
  strutils, ComCtrls;

const
  ControlSuffixes: array[0..8] of String = (
    'edit',
    'combobox',
    'memo',
    'checkbox',
    'checkgroup',
    'label',
    'picker',
    'radiobutton',
    'radiogroup'
  );


procedure LoadElementsFromControls(Elements: TJSONFormElements; ControlList: TFPList);
var
  Control: TControl;
  ControlName, PropertyName: String;
  i, j, SuffixPos: Integer;
  NewElement: TJSONFormElement;
begin
  Elements.BeginUpdate;
  try
    for i := 0 to ControlList.Count - 1 do
    begin
      Control := TControl(ControlList[i]);
      ControlName := LowerCase(Control.Name);
      PropertyName := ControlName;
      for j := Low(ControlSuffixes) to High(ControlSuffixes) do
      begin
        if AnsiEndsText(ControlSuffixes[j], ControlName) then
        begin
          SuffixPos := RPos(ControlSuffixes[j], ControlName);
          if SuffixPos > 0 then
            PropertyName := Copy(ControlName, 1, SuffixPos - 1);
          break;
        end;
      end;
      NewElement := Elements.Add;
      NewElement.Control := Control;
      NewElement.Name := PropertyName;
      NewElement.PropertyName := PropertyName;
    end;
  finally
    Elements.EndUpdate;
  end;
end;

{ TJSONFormMediatorImportControlsViewForm }

procedure TJSONFormMediatorImportControlsViewForm.FormShow(Sender: TObject);
begin
  LoadParentControlList;
end;

procedure TJSONFormMediatorImportControlsViewForm.ImportButtonClick(
  Sender: TObject);
var
  CheckedControlList: TFPList;
begin
  CheckedControlList := CreateCheckedControlList;
  try
    LoadElementsFromControls(Mediator.Elements, CheckedControlList);
  finally
    CheckedControlList.Destroy;
  end;
end;

procedure TJSONFormMediatorImportControlsViewForm.ParentControlComboBoxChange(
  Sender: TObject);
begin
  ParentControlChanged;
end;

procedure TJSONFormMediatorImportControlsViewForm.AddControlToList(
  Control: TControl);
var
  i: Integer;
begin
  //todo: filter TPanel / TabSheet
  FControlList.Add(Control);
  if RecursiveCheckBox.Checked and (Control is TWinControl)
    and ((csAcceptsControls in Control.ControlStyle) or Control.InheritsFrom(TCustomTabControl)) then
  begin
    for i := 0 to TWinControl(Control).ControlCount - 1 do
      AddControlToList(TWinControl(Control).Controls[i]);
  end;
end;

procedure TJSONFormMediatorImportControlsViewForm.AddControlToParentList(
  ParentControl: TWinControl);
var
  i: Integer;
  Control: TControl;
begin
  ParentControlComboBox.AddItem(ParentControl.Name, ParentControl);
  for i := 0 to ParentControl.ControlCount - 1 do
  begin
    Control := ParentControl.Controls[i];
    if (Control is TWinControl) and
      ((csAcceptsControls in Control.ControlStyle) or Control.InheritsFrom(TCustomTabControl)) then
      AddControlToParentList(TWinControl(Control));
  end;
end;

function TJSONFormMediatorImportControlsViewForm.CreateCheckedControlList: TFPList;
var
  Node: PVirtualNode;
begin
  Result := TFPList.Create;
  Node := ControlListVew.GetFirstChecked;
  while Node <> nil do
  begin
    Result.Add(FControlList[Node^.Index]);
    Node := ControlListVew.GetNextChecked(Node);
  end;
end;

procedure TJSONFormMediatorImportControlsViewForm.ControlListVewGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  AControl: TControl;
begin
  AControl := TControl(FControlList[Node^.Index]);
  CellText := AControl.Name;
end;

procedure TJSONFormMediatorImportControlsViewForm.ControlListVewInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  //todo: use a tree structur instead of a list one
  Sender.CheckType[Node] := ctCheckBox;
end;

procedure TJSONFormMediatorImportControlsViewForm.LoadParentControlList;
var
  i: Integer;
  Control: TControl;
begin
  ParentControlComboBox.Clear;
  ParentControlComboBox.AddItem(FTopLevelControl.Name, FTopLevelControl);
  for i := 0 to TopLevelControl.ControlCount - 1 do
  begin
    Control := TopLevelControl.Controls[i];
    //todo: investigate why TPageControl does not have csAcceptControls
    if (Control is TWinControl) and
      ((csAcceptsControls in Control.ControlStyle) or Control.InheritsFrom(TCustomTabControl)) then
      AddControlToParentList(TWinControl(Control));
  end;
end;

procedure TJSONFormMediatorImportControlsViewForm.ParentControlChanged;
var
  ParentControl: TWinControl;
  i: Integer;
begin
  FControlList.Clear;
  if ParentControlComboBox.ItemIndex > -1 then
  begin
    ParentControl := ParentControlComboBox.Items.Objects[ParentControlComboBox.ItemIndex] as TWinControl;
    for i := 0 to ParentControl.ControlCount - 1 do
      AddControlToList(ParentControl.Controls[i]);
  end;
  ControlListVew.RootNodeCount := FControlList.Count;
end;

constructor TJSONFormMediatorImportControlsViewForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FControlList := TFPList.Create;
end;

destructor TJSONFormMediatorImportControlsViewForm.Destroy;
begin
  FControlList.Destroy;
  inherited Destroy;
end;

end.

