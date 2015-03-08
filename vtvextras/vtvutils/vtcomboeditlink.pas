unit VTComboEditLink;

{$mode objfpc}{$H+}

interface

uses
  Forms, LCLIntf, Classes, SysUtils, VirtualTrees, LMessages, StdCtrls, Graphics,
  DbCtrls, db, Controls;

type

  TVTCustomComboEditLink = class;

  TVTComboEditLinkEvent = procedure(Link: TVTCustomComboEditLink) of object;

  TVTPrepareComboEvent = procedure(Link: TVTCustomComboEditLink; Node: PVirtualNode;
    Column: TColumnIndex; const NodeText: String) of object;

  TCustomComboBoxClass = class of TCustomComboBox;

  { TVTCustomComboEditLink }

  TVTCustomComboEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FCombo: TCustomComboBox;                  // A custom combo box control.
    FOnEditingDone: TVTComboEditLinkEvent;
    FOnPrepareCombo: TVTPrepareComboEvent;
    FOnComboSelect: TVTComboEditLinkEvent;
    FTree: TCustomVirtualStringTree;        // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FOriginalIndex: Integer;
    FAlignment: TAlignment;
    FStopping: Boolean;
    procedure ComboEditingDone(Sender: TObject);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NotifyEndEdit;
  protected
    procedure DoPrepareCombo(ANode: PVirtualNode; Column: TColumnIndex;
      const NodeText: String); virtual;
    procedure DoEditingDone; virtual;
    procedure DoSelect; virtual;
    class function GetComboClass: TCustomComboBoxClass; virtual; abstract;
    property Tree: TCustomVirtualStringTree read FTree;
  public
    constructor Create;
    destructor Destroy; override;
    //IVTEditLink methods
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(ATree: TBaseVirtualTree; ANode: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
    property Node: PVirtualNode read FNode;
    //todo: change to Control
    property Combo: TCustomComboBox read FCombo;
    property OnPrepareCombo: TVTPrepareComboEvent read FOnPrepareCombo write FOnPrepareCombo;
    property OnEditingDone: TVTComboEditLinkEvent read FOnEditingDone write FOnEditingDone;
    //rename to onselect
    property OnComboSelect: TVTComboEditLinkEvent read FOnComboSelect write FOnComboSelect;
  end;

  { TVTComboEditLink }

  //todo: add OnKeyPress based on column field type
  TVTComboEditLink = class (TVTCustomComboEditLink)
  protected
    procedure DoPrepareCombo(ANode: PVirtualNode; Column: TColumnIndex;
      const NodeText: String); override;
    procedure DoEditingDone; override;
    class function GetComboClass: TCustomComboBoxClass; override;
  public
  end;

  { TVTDBComboEditLink }

  TVTDBComboEditLink = class (TVTCustomComboEditLink)
  private
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDataField: String;
    procedure SetDataField(const AValue: String);
  protected
    class function GetComboClass: TCustomComboBoxClass; override;
  public
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  { TVTDBLookupComboEditLink }

  TVTDBLookupComboEditLink = class (TVTCustomComboEditLink)
  private
    FDataSource: TDataSource;
    function GetDataField: String;
    function GetKeyField: String;
    function GetListField: String;
    function GetListSource: TDataSource;
    procedure SetDataField(const AValue: String);
    procedure SetKeyField(const AValue: String);
    procedure SetListField(const AValue: String);
    procedure SetListSource(const AValue: TDataSource);
  protected
    procedure DoPrepareCombo(ANode: PVirtualNode; Column: TColumnIndex;
      const NodeText: String); override;
    class function GetComboClass: TCustomComboBoxClass; override;
  public
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property KeyField: String read GetKeyField write SetKeyField;
    property ListField: String read GetListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
  end;

implementation

uses
  LCLType;

type
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

  TCustomComboBoxAccess = class(TCustomComboBox)
  end;

  TControlAccess = class(TControl)
  end;

{ TVTCustomComboEditLink }

procedure TVTCustomComboEditLink.ComboEditingDone(Sender: TObject);
begin
  NotifyEndEdit;
end;

procedure TVTCustomComboEditLink.ComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Tab key completes the text and finish
  //todo: better handling of VK_SCAPE
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
    Key := VK_TAB;
end;

procedure TVTCustomComboEditLink.NotifyEndEdit;
var
  ATree: TCustomVirtualStringTree;
begin
  if not FStopping then
  begin
    ATree := FTree;
    TBaseVirtualTreeAccess(FTree).DoEndEdit;
    ATree.SetFocus;
  end;
end;

procedure TVTCustomComboEditLink.DoPrepareCombo(ANode: PVirtualNode;
  Column: TColumnIndex; const NodeText: String);
begin
  if Assigned(FOnPrepareCombo) then
    FOnPrepareCombo(Self, ANode, Column, NodeText);
end;

procedure TVTCustomComboEditLink.DoEditingDone;
begin
  if Assigned(FOnEditingDone) then
    FOnEditingDone(Self);
end;

procedure TVTCustomComboEditLink.DoSelect;
begin
  if Assigned(FOnComboSelect) then
    FOnComboSelect(Self);
end;

constructor TVTCustomComboEditLink.Create;
begin
  FCombo := GetComboClass.Create(nil);
  with TCustomComboBoxAccess(FCombo) do
  begin
    //Style := csDropDownList;
    Visible := False;
    AutoSize := False;
  end;
  TControlAccess(FCombo).OnEditingDone := @ComboEditingDone;
  TCustomComboBoxAccess(FCombo).OnCloseUp := @ComboEditingDone;
  FCombo.OnKeyDown := @ComboKeyDown;
end;

destructor TVTCustomComboEditLink.Destroy;
begin
  Application.ReleaseComponent(FCombo);
  inherited Destroy;
end;

function TVTCustomComboEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FCombo.Show;
    FCombo.SetFocus;
  end;
end;

function TVTCustomComboEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FCombo.Hide;
    FTree.CancelEditNode;
  end;
end;

function TVTCustomComboEditLink.EndEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    //todo: remove onselect keep only editing done
    if FCombo.ItemIndex <> FOriginalIndex then
      DoSelect;
    DoEditingDone;
    FCombo.Hide;
  except
    FStopping := False;
    raise;
  end;
end;

function TVTCustomComboEditLink.GetBounds: TRect; stdcall;
begin
  Result := FCombo.BoundsRect;
end;

function TVTCustomComboEditLink.PrepareEdit(ATree: TBaseVirtualTree;
  ANode: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  NodeText: String;
begin
  Result := ATree is TCustomVirtualStringTree;
  if Result then
  begin
    FStopping := False;
    FTree := TCustomVirtualStringTree(ATree);
    FNode := ANode;
    FColumn := Column;

    FCombo.Font.Color := clWindowText;
    FCombo.Parent := ATree;
    FCombo.HandleNeeded;
    // Initial size, font and text of the ANode.
    FTree.GetTextInfo(ANode, Column, FCombo.Font, FTextBounds, NodeText);
    DoPrepareCombo(ANode, Column, NodeText);
    FOriginalIndex := FCombo.ItemIndex;

    if Column <= NoColumn then
    begin
      FCombo.BidiMode := FTree.BidiMode;
      FAlignment := TBaseVirtualTreeAccess(FTree).Alignment;
    end
    else
    begin
      FCombo.BidiMode := TBaseVirtualTreeAccess(FTree).Header.Columns[Column].BidiMode;
      FAlignment := TBaseVirtualTreeAccess(FTree).Header.Columns[Column].Alignment;
    end;
//    if FCombo.BidiMode <> bdLeftToRight then
//      ChangeBidiModeAlignment(FAlignment);
  end;

end;

procedure TVTCustomComboEditLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FCombo.WindowProc(Message);
end;

procedure TVTCustomComboEditLink.SetBounds(R: TRect); stdcall;
var
  Offset: Integer;
begin
  if not FStopping then
  begin
    with R do
    begin
      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      //todo: properly get the arrow width
      //Inc(Right, 20);
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FCombo.BoundsRect := R;

      // The selected text shall exclude the text margins and be centered vertically.
      // We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
      // control leaves around the (selected) text.

      R := FCombo.ClientRect;
      Offset := 2;
      if tsUseThemes in FTree.TreeStates then
        Inc(Offset);
      InflateRect(R, -TBaseVirtualTreeAccess(FTree).TextMargin + Offset, Offset);
      if not (vsMultiline in FNode^.States) then
        OffsetRect(R, 0, FTextBounds.Top - FCombo.Top);

      SendMessage(FCombo.Handle, EM_SETRECTNP, 0, PtrInt(@R));
    end;
  end;
end;

{ TVTComboEditLink }

procedure TVTComboEditLink.DoPrepareCombo(ANode: PVirtualNode;
  Column: TColumnIndex; const NodeText: String);
begin
  Combo.Text := NodeText;
  inherited DoPrepareCombo(ANode, Column, NodeText);
end;

procedure TVTComboEditLink.DoEditingDone;
begin
  inherited;
  FTree.Text[FNode, FColumn] := FCombo.Text;
end;

class function TVTComboEditLink.GetComboClass: TCustomComboBoxClass;
begin
  Result := TComboBox;
end;

{ TVTDBLookupComboEditLink }

procedure TVTDBLookupComboEditLink.SetDataField(const AValue: String);
begin
  (Combo as TDBLookupComboBox).DataField := AValue;
end;

function TVTDBLookupComboEditLink.GetDataField: String;
begin
  Result := (Combo as TDBLookupComboBox).DataField;
end;

function TVTDBLookupComboEditLink.GetKeyField: String;
begin
  Result := (Combo as TDBLookupComboBox).KeyField;
end;

function TVTDBLookupComboEditLink.GetListField: String;
begin
  Result := (Combo as TDBLookupComboBox).ListField;
end;

function TVTDBLookupComboEditLink.GetListSource: TDataSource;
begin
  Result := (Combo as TDBLookupComboBox).ListSource;
end;

procedure TVTDBLookupComboEditLink.SetKeyField(const AValue: String);
begin
  (Combo as TDBLookupComboBox).KeyField := AValue;
end;

procedure TVTDBLookupComboEditLink.SetListField(const AValue: String);
begin
  (Combo as TDBLookupComboBox).ListField := AValue;
end;

procedure TVTDBLookupComboEditLink.SetListSource(const AValue: TDataSource);
begin
  (Combo as TDBLookupComboBox).ListSource := AValue;
end;

procedure TVTDBLookupComboEditLink.DoPrepareCombo(ANode: PVirtualNode;
  Column: TColumnIndex; const NodeText: String);
begin
  inherited DoPrepareCombo(ANode, Column, NodeText);
  (Combo as TDBLookupComboBox).DataSource := FDataSource;
end;

class function TVTDBLookupComboEditLink.GetComboClass: TCustomComboBoxClass;
begin
  Result := TDBLookupComboBox;
end;

{ TVTDBComboEditLink }

function TVTDBComboEditLink.GetDataSource: TDataSource;
begin
  Result := (Combo as TDBComboBox).DataSource;
end;

procedure TVTDBComboEditLink.SetDataSource(Value: TDataSource);
begin
  (Combo as TDBComboBox).DataSource := Value;
end;

function TVTDBComboEditLink.GetDataField: String;
begin
  Result := (Combo as TDBComboBox).DataField;
end;

procedure TVTDBComboEditLink.SetDataField(const AValue: String);
begin
  (Combo as TDBComboBox).DataField := AValue;
end;

class function TVTDBComboEditLink.GetComboClass: TCustomComboBoxClass;
begin
  Result := TDBComboBox;
end;

end.

