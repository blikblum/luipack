unit ObjectAvlTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree;
  

type
  {
  TPointerToPointerTree - Associative array
  adapted from LCL.AvgLvlTree.TPointerToPointerTree
  }


  TPointerToPointerItem = record
    Key: Pointer;
    Value: Pointer;
  end;
  PPointerToPointerItem = ^TPointerToPointerItem;

  TPointerToPointerTree = class
  private
    FItems: TAVLTree;
    function GetCount: Integer;
    function GetValues(const Key: Pointer): Pointer;
    procedure SetValues(const Key: Pointer; const AValue: Pointer);
    function FindNode(const Key: Pointer): TAVLTreeNode;
    function GetNode(Node: TAVLTreeNode; out Key, Value: Pointer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(Key: Pointer);
    function Contains(const Key: Pointer): Boolean;
    function GetFirst(out Key, Value: Pointer): Boolean;
    function GetLast(out Key, Value: Pointer): Boolean;
    function GetNext(const Key: Pointer; out NextKey, NextValue: Pointer): Boolean;
    function GetPrev(const Key: Pointer; out PrevKey, PrevValue: Pointer): Boolean;
    property Count: Integer read GetCount;
    property Values[const Key: Pointer]: Pointer read GetValues write SetValues; default;
    property Tree: TAVLTree read FItems;
  end;


function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointer(PPointerToPointerItem(Data1)^.Key,
                         PPointerToPointerItem(Data2)^.Key);
end;

function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;
begin
  Result:=ComparePointer(Key,PPointerToPointerItem(Data)^.Key);
end;


{ TPointerToPointerTree }

function TPointerToPointerTree.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

function TPointerToPointerTree.GetValues(const Key: Pointer): Pointer;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Result:=PPointerToPointerItem(Node.Data)^.Value
  else
    Result:=nil;
end;

procedure TPointerToPointerTree.SetValues(const Key: Pointer;
  const AValue: Pointer);
var
  NewItem: PPointerToPointerItem;
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Key);
  if (Node<>nil) then
    PPointerToPointerItem(Node.Data)^.Value:=AValue
  else begin
    New(NewItem);
    NewItem^.Key:=Key;
    NewItem^.Value:=AValue;
    FItems.Add(NewItem);
  end;
end;

function TPointerToPointerTree.FindNode(const Key: Pointer): TAVLTreeNode;
begin
  Result:=FItems.FindKey(Key,@ComparePointerWithPtrToPtrItem)
end;

function TPointerToPointerTree.GetNode(Node: TAVLTreeNode; out Key,
  Value: Pointer): Boolean;
var
  Item: PPointerToPointerItem;
begin
  if Node<>nil then begin
    Item:=PPointerToPointerItem(Node.Data);
    Key:=Item^.Key;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Key:=nil;
    Value:=nil;
    Result:=false;
  end;
end;

constructor TPointerToPointerTree.Create;
begin
  FItems:=TAVLTree.Create(@ComparePointerToPointerItems);
end;

destructor TPointerToPointerTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TPointerToPointerTree.Clear;
var
  Node: TAVLTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    Dispose(Item);
    Node:=FItems.FindSuccessor(Node);
  end;
  FItems.Clear;
end;

procedure TPointerToPointerTree.Remove(Key: Pointer);
var
  Node: TAVLTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FindNode(Key);
  if Node=nil then exit;
  Item:=PPointerToPointerItem(Node.Data);
  FItems.Delete(Node);
  Dispose(Item);
end;

function TPointerToPointerTree.Contains(const Key: Pointer): Boolean;
begin
  Result:=FindNode(Key)<>nil;
end;

function TPointerToPointerTree.GetFirst(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Key,Value);
end;

function TPointerToPointerTree.GetLast(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Key,Value);
end;

function TPointerToPointerTree.GetNext(const Key: Pointer; out NextKey,
  NextValue: Pointer): Boolean;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Tree.FindSuccessor(Node);
  Result:=GetNode(Node,NextKey,NextValue);
end;

function TPointerToPointerTree.GetPrev(const Key: Pointer; out PrevKey,
  PrevValue: Pointer): Boolean;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Tree.FindPrecessor(Node);
  Result:=GetNode(Node,PrevKey,PrevValue);
end;



end.

