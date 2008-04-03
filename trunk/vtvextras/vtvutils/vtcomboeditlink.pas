unit VTComboEditLink;

{$mode objfpc}{$H+}

interface

uses
  Forms, LCLIntf, Classes, SysUtils, VirtualTrees, LMessages, StdCtrls, Graphics;

type

  TVTComboEditLink = class;

  TVTComboEditLinkEvent = procedure(Link: TVTComboEditLink) of object;

  { TVTComboEditLink }

  TVTComboEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FCombo: TComboBox;                  // A normal custom edit control.
    FOnPrepare: TVTComboEditLinkEvent;
    FOnSelect: TVTComboEditLinkEvent;
    FTree: TCustomVirtualStringTree;        // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FOriginalIndex: Integer;
    FAlignment: TAlignment;
    FStopping: Boolean;
    procedure ComboSelect(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;

    property Combo: TComboBox read FCombo;
    property OnPrepare: TVTComboEditLinkEvent read FOnPrepare write FOnPrepare;
    property OnSelect: TVTComboEditLinkEvent read FOnSelect write FOnSelect;
  end;


implementation

type
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

{ TVTComboEditLink }

procedure TVTComboEditLink.ComboSelect(Sender: TObject);
var
  Tree: TCustomVirtualStringTree;
begin
  Tree := FTree;
  TBaseVirtualTreeAccess(FTree).DoEndEdit;
  Tree.SetFocus;
end;

constructor TVTComboEditLink.Create;
var
  AObject: TObject;

begin
  AObject := Self;
  FCombo := TComboBox.Create(nil);
  with FCombo do
  begin
    //Style := csDropDownList;
    Visible := False;
    AutoSize := False;
    OnSelect := @ComboSelect;
  end;
end;

destructor TVTComboEditLink.Destroy;
begin
  Application.ReleaseComponent(FCombo);
  inherited Destroy;
end;

function TVTComboEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FCombo.Show;
    FCombo.SetFocus;
  end;
end;

function TVTComboEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FCombo.Hide;
    FTree.CancelEditNode;
  end;
end;

function TVTComboEditLink.EndEdit: Boolean; stdcall;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    if FCombo.ItemIndex <> FOriginalIndex then
    begin
      FTree.Text[FNode, FColumn] := FCombo.Text;
      if Assigned(FOnSelect) then
        FOnSelect(Self);
    end;
    FCombo.Hide;
  except
    FStopping := False;
    raise;
  end;
end;

function TVTComboEditLink.GetBounds: TRect; stdcall;
begin
  Result := FCombo.BoundsRect;
end;

function TVTComboEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;

var
  Text: WideString;
begin
  Result := Tree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TCustomVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FCombo.Font, FTextBounds, Text);
    FCombo.Font.Color := clWindowText;
    FCombo.Parent := Tree;
    FCombo.HandleNeeded;

    if Assigned(FOnPrepare) then
      FOnPrepare(Self);

    FCombo.Text := Text;
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

procedure TVTComboEditLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  FCombo.WindowProc(Message);
end;

procedure TVTComboEditLink.SetBounds(R: TRect); stdcall;
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

      SendMessage(FCombo.Handle, EM_SETRECTNP, 0, Integer(@R));

    end;
  end;
end;


end.

