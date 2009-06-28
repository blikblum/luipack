unit VTComboEditLink;

{$mode objfpc}{$H+}

interface

uses
  Forms, LCLIntf, Classes, SysUtils, VirtualTrees, LMessages, StdCtrls, Graphics,
  DbCtrls, db;

type

  TVTCustomComboEditLink = class;

  TVTComboEditLinkEvent = procedure(Link: TVTCustomComboEditLink) of object;

  TVTPrepareComboEvent = procedure(Link: TVTCustomComboEditLink; Node: PVirtualNode;
    Column: TColumnIndex; const NodeText: UTF8String) of object;

  TCustomComboBoxClass = class of TCustomComboBox;

  { TVTCustomComboEditLink }

  TVTCustomComboEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FCombo: TCustomComboBox;                  // A custom combo box control.
    FOnPrepareCombo: TVTPrepareComboEvent;
    FOnComboSelect: TVTComboEditLinkEvent;
    FTree: TCustomVirtualStringTree;        // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FOriginalIndex: Integer;
    FAlignment: TAlignment;
    FStopping: Boolean;
    procedure ComboSelect(Sender: TObject);
  protected
    procedure DoPrepareCombo(Node: PVirtualNode; Column: TColumnIndex;
      const NodeText: UTF8String); virtual;
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
    function PrepareEdit(ATree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;

    property Combo: TCustomComboBox read FCombo;
    property OnPrepareCombo: TVTPrepareComboEvent read FOnPrepareCombo write FOnPrepareCombo;
    property OnComboSelect: TVTComboEditLinkEvent read FOnComboSelect write FOnComboSelect;
  end;

  { TVTComboEditLink }

  TVTComboEditLink = class (TVTCustomComboEditLink)
  protected
    procedure DoPrepareCombo(Node: PVirtualNode; Column: TColumnIndex;
      const NodeText: UTF8String); override;
    procedure DoSelect; override;
    class function GetComboClass: TCustomComboBoxClass; override;
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
    procedure DoPrepareCombo(Node: PVirtualNode; Column: TColumnIndex;
      const NodeText: UTF8String); override;
    class function GetComboClass: TCustomComboBoxClass; override;
  public
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property KeyField: String read GetKeyField write SetKeyField;
    property ListField: String read GetListField write SetListField;
    property ListSource: TDataSource read GetListSource write SetListSource;
  end;

implementation

type
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

  TCustomComboBoxAccess = class(TCustomComboBox)
  end;

{ TVTCustomComboEditLink }

procedure TVTCustomComboEditLink.ComboSelect(Sender: TObject);
var
  ATree: TCustomVirtualStringTree;
begin
  ATree := FTree;
  TBaseVirtualTreeAccess(FTree).DoEndEdit;
  ATree.SetFocus;
end;

procedure TVTCustomComboEditLink.DoPrepareCombo(Node: PVirtualNode;
  Column: TColumnIndex; const NodeText: UTF8String);
begin
  if Assigned(FOnPrepareCombo) then
    FOnPrepareCombo(Self, Node, Column, NodeText);
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
    OnSelect := @ComboSelect;
  end;
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
    if FCombo.ItemIndex <> FOriginalIndex then
      DoSelect;
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
  Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  NodeText: UTF8String;
begin
  Result := ATree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := TCustomVirtualStringTree(ATree);
    FNode := Node;
    FColumn := Column;

    FCombo.Font.Color := clWindowText;
    FCombo.Parent := ATree;
    FCombo.HandleNeeded;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FCombo.Font, FTextBounds, NodeText);
    DoPrepareCombo(Node, Column, NodeText);
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

procedure TVTComboEditLink.DoPrepareCombo(Node: PVirtualNode;
  Column: TColumnIndex; const NodeText: UTF8String);
begin
  Combo.Text := NodeText;
  inherited DoPrepareCombo(Node, Column, NodeText);
end;

procedure TVTComboEditLink.DoSelect;
begin
  FTree.Text[FNode, FColumn] := FCombo.Text;
  inherited DoSelect;
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

procedure TVTDBLookupComboEditLink.DoPrepareCombo(Node: PVirtualNode;
  Column: TColumnIndex; const NodeText: UTF8String);
begin
  inherited DoPrepareCombo(Node, Column, NodeText);
  (Combo as TDBLookupComboBox).DataSource := FDataSource;
end;

class function TVTDBLookupComboEditLink.GetComboClass: TCustomComboBoxClass;
begin
  Result := TDBLookupComboBox;
end;

end.

