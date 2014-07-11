unit LuiDOMClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, contnrs;

type

  { TCustomTagTree }

  TCustomTagTree = class
  private
    FCurrentElement: TDOMElement;
    FParentNode: TDOMNode;
    FRootNode: TDOMNode;
    FParentStack: TStack;
    procedure CheckCurrentElement;
    procedure InternalAdd(El: TDOMElement; DoSetParent: Boolean);
  protected
    function GetDocument: TXMLDocument; virtual; abstract;
    function GetRootNode: TDOMNode; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //todo: add insert
    function Add(const TagName: String): TCustomTagTree;
    function Add(const TagName, Text: String): TCustomTagTree;
    function AddText(const Text: String): TCustomTagTree;
    function Open(const TagName: String): TCustomTagTree;
    function Open(const TagName, Text: String): TCustomTagTree;
    //todo: add close with TagName (to check if the tag is the expected)
    function Close: TCustomTagTree;
    function GetAttribute(const AttributeName: String; out Value: String): TCustomTagTree;
    function RemoveAttribute(const AttributeName: String): TCustomTagTree;
    function SetAttribute(const AttributeName, Value: String): TCustomTagTree;
    procedure SaveToFile(const FileName: String);
    procedure SaveToStream(Stream: TStream); virtual;
    //todo: add Current, Parent properties
    property Document: TXMLDocument read GetDocument;
  end;

  { THTMLTree }

  THTMLTree = class(TCustomTagTree)
  private
    FDocument: TXMLDocument;
    FHeadEl: TDOMElement;
    FBodyEl: TDOMElement;
  protected
    function GetDocument: TXMLDocument; override;
    function GetRootNode: TDOMNode; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  { TTagTree }

  TTagTree = class(TCustomTagTree)
  private
    FDocument: TXMLDocument;
  protected
    function GetDocument: TXMLDocument; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  XMLWrite;

{ TTagTree }

function TTagTree.GetDocument: TXMLDocument;
begin
  Result := FDocument;
end;

constructor TTagTree.Create;
begin
  inherited Create;
  FDocument := TXMLDocument.Create;
end;

destructor TTagTree.Destroy;
begin
  FDocument.Destroy;
  inherited Destroy;
end;

{ THTMLTree }

function THTMLTree.GetDocument: TXMLDocument;
begin
  Result := FDocument;
end;

function THTMLTree.GetRootNode: TDOMNode;
begin
  Result := FBodyEl;
end;

constructor THTMLTree.Create;
var
  HTMLNode: TDOMNode;
begin
  inherited Create;
  FDocument := TXMLDocument.Create;
  //todo: add a head: TDOMTree property
  FHeadEl := FDocument.CreateElement('head');
  FBodyEl := FDocument.CreateElement('body');
  HTMLNode := FDocument.CreateElement('html');
  HTMLNode.AppendChild(FHeadEl);
  HTMLNode.AppendChild(FBodyEl);
  FDocument.AppendChild(HTMLNode);
end;

destructor THTMLTree.Destroy;
begin
  FDocument.Destroy;
  inherited Destroy;
end;

procedure THTMLTree.SaveToStream(Stream: TStream);
const
  DocTypeStr = '<!DOCTYPE HTML>';
begin
  //set DOCTYPE. For now only HTML5
  Stream.WriteBuffer(DocTypeStr[1], Length(DocTypeStr));
  WriteXML(FHeadEl.ParentNode, Stream);
end;

{ TCustomTagTree }

procedure TCustomTagTree.CheckCurrentElement;
begin
  if FCurrentElement = nil then
    raise Exception.Create('Current element not set');
end;

procedure TCustomTagTree.InternalAdd(El: TDOMElement; DoSetParent: Boolean);
begin
  // lazy parent node initialization
  if FParentNode = nil then
  begin
    FParentNode := GetRootNode;
    if FParentNode = nil then
    begin
      //no root node found
      //set as new one
      FRootNode := El;
      FParentNode := El;
    end;
  end;
  if FParentNode <> El then
    FParentNode.AppendChild(El);
  if DoSetParent then
  begin
    FParentStack.Push(El);
    FParentNode := El;
  end;
end;

function TCustomTagTree.GetRootNode: TDOMNode;
begin
  Result := FRootNode;
end;

constructor TCustomTagTree.Create;
begin
  FParentStack := TStack.Create;
end;

destructor TCustomTagTree.Destroy;
begin
  FParentStack.Destroy;
  inherited Destroy;
end;

function TCustomTagTree.Add(const TagName: String): TCustomTagTree;
begin
  FCurrentElement := Document.CreateElement(TagName);
  InternalAdd(FCurrentElement, False);
  Result := Self
end;

function TCustomTagTree.Add(const TagName, Text: String): TCustomTagTree;
begin
  FCurrentElement := Document.CreateElement(TagName);
  FCurrentElement.AppendChild(Document.CreateTextNode(UTF8Decode(Text)));
  InternalAdd(FCurrentElement, False);
  Result := Self;
end;

function TCustomTagTree.AddText(const Text: String): TCustomTagTree;
begin
  FParentNode.AppendChild(Document.CreateTextNode(UTF8Decode(Text)));
  Result := Self;
end;

function TCustomTagTree.Open(const TagName: String): TCustomTagTree;
begin
  FCurrentElement := Document.CreateElement(TagName);
  InternalAdd(FCurrentElement, True);
  Result := Self;
end;

function TCustomTagTree.Open(const TagName, Text: String): TCustomTagTree;
begin
  FCurrentElement := Document.CreateElement(TagName);
  FCurrentElement.AppendChild(Document.CreateTextNode(UTF8Decode(Text)));
  InternalAdd(FCurrentElement, True);
  Result := Self;
end;

function TCustomTagTree.Close: TCustomTagTree;
begin
  FParentStack.Pop;
  //update parent
  if FParentStack.AtLeast(1) then
    FParentNode := TDOMNode(FParentStack.Peek)
  else
    FParentNode := GetRootNode;
  Result := Self;
end;

function TCustomTagTree.GetAttribute(const AttributeName: String; out
  Value: String): TCustomTagTree;
begin
  CheckCurrentElement;
  Value := FCurrentElement.GetAttribute(AttributeName);
  Result := Self;
end;

function TCustomTagTree.RemoveAttribute(const AttributeName: String): TCustomTagTree;
begin
  CheckCurrentElement;
  FCurrentElement.RemoveAttribute(AttributeName);
  Result := Self;
end;

function TCustomTagTree.SetAttribute(const AttributeName, Value: String): TCustomTagTree;
begin
  CheckCurrentElement;
  FCurrentElement.SetAttribute(AttributeName, Value);
  Result := Self;
end;

procedure TCustomTagTree.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomTagTree.SaveToStream(Stream: TStream);
begin
  WriteXML(GetRootNode, Stream);
end;


end.

