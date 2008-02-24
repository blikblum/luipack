unit DomUtils;

 { Helper DOM classes

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br


  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}
{$H+}

interface

uses
  Dom, Classes, DB, miscutils;

type

  THtmlSubTree = class;

  { THtmlTree }

  THtmlTree = class

  private
    //Todo:implement AddCssLink OK
    //and store files in a StringList ????? same for JSFile
    FTitle: String;
    FHeadNode: TDomElement;
    FBodyNode: TDomElement;
    FRootNode: TDomElement;   
    FDocHandle: TXmlDocument;
    FSubTree:THtmlSubTree;
    FBookmarkStrings:TStringList;
    FBookmarkIndexes:TIndexedList;
    procedure ResetNodes;
    procedure SetTitle(const AName: String);
  public
    constructor Create;
    destructor Destroy;
    procedure SaveToFile(const AFileName: String);
    procedure Clear;
    function CreateText(const AText: String): TDomNode;
    procedure AddBookmark(ANode:TDomNode; const AText: String);
    procedure AddBookmark(ANode:TDomNode; const AIndex: Integer);
    function GetBookmark(const AText: String): TDomNode;
    function GetBookmark(const AIndex: Integer): TDomNode;
    procedure AddCSSLink(const AFilePath: String);
    procedure AddJSLink(const AFilePath: String);
    property BookmarkIndexes: TIndexedList read FBookmarkIndexes;
    property Title: String read FTitle write SetTitle;
    property HeadNode: TDOMElement read FHeadNode;
    property BodyNode: TDOMElement read FBodyNode;
    property RootNode: TDOMElement read FRootNode;
    property DocHandle: TXmlDocument read FDocHandle;
    property SubTree: THtmlSubTree read FSubTree;
  end;
  

  { THtmlSubTree }

  THtmlSubTree = class
  private
    FNodeList:TFPList;
    FParent:THtmlTree;
    FBaseIndex: Integer;
    function GetLastElement: TDOMElement;
    function GetBaseElement: TDOMElement;
  public
    constructor Create(AParent: THtmlTree);virtual;
    destructor Destroy;virtual;
    //todo: Add function Add([array of const]);
    function AddTo(ANode:TDomNode;const ATag: String): TDomNode;
    function Add(const ATag:String): TDomNode;
    function AddBase(const ATag:String):TDomNode;
    procedure AddText(const AText:String);
    procedure AddListItem(const AText:String);
    procedure AttachTo(AParent:TDomNode);virtual;
    procedure Attach;
    procedure Clear;virtual;
    property Last: TDomElement read GetLastElement;
    property Base: TDomElement read GetBaseElement;
  end;
  
  { TTableTree }

  TTableTree = class (THtmlSubTree)
  private
    FRowNode:TDomNode;
    FTableNode:TDomNode;
  public
    constructor Create(AParent: THtmlTree); override;
    destructor Destroy; override;
    procedure AddRow;
    procedure AddCol(const AText:String);
    procedure AttachTo(AParent:TDomNode);override;
    procedure Clear;override;
  end;


  TColumnCallback = function: String;

  TDBTree = class;
  { TDbColumn }

  TDBColumn = class
  private
    FCallback: TColumnCallback;
    FField: TField;
    FFieldName: String;
    FTitle: String;
    procedure UpdateField(ADataSet:TDataSet);
  public
    constructor Create(ADataset:TDataset;AFieldName:String);
    destructor Destroy;
    function GetData:String;
    property FieldName: String read FFieldName write FFieldName;
    property Title: String read FTitle write FTitle;
    property Callback: TColumnCallback read FCallback write FCallback;
  end;
  
  TDBColumns = class
  private
    FColumnList:TList;
    FParent:TDBTree;
    function GetColumn(Index:Integer):TDbColumn; 
    function GetCount:Integer;
  public
    constructor Create(AParent: TDBTree);
    destructor Destroy;
    procedure Add(AFieldName,ATitle:String;ACallback:TColumnCallback); overload;
    procedure Add(AFieldName,ATitle:String); overload;
    procedure Add(AFieldName:String;ACallback:TColumnCallback); overload;
    procedure Add(AFieldName:String); overload;
    procedure Clear;
    property Items[Index:Integer]:TDbColumn read GetColumn; default;
    property Count:Integer read GetCount;
  end;
  
  { TDBTree }

  TDBTree = class(TTableTree)
  private
    FDataset: TDataset;
    FTitle: String;
    FColumns:TDBColumns;
    FRecordLimit: Integer;
    FShowColumnTitles: Boolean;
    procedure UpdateColumns;
  public
    constructor Create(AParent:THtmlTree);override;
    destructor Destroy; override;
    procedure BuildTable;
    property Dataset:TDataset read FDataset write FDataset;
    property Title: String read FTitle write FTitle;
    property Columns:TDBColumns read FColumns;
    property RecordLimit: Integer read FRecordLimit write FRecordLimit;
    property ShowColumnTitles: Boolean read FShowColumnTitles write FShowColumnTitles;
  end;

implementation

uses
  SysUtils, XmlWrite;

constructor THtmlTree.Create;
begin
  FDocHandle:= TXMLDocument.Create;
  ResetNodes;
  FSubTree:=THtmlSubTree.Create(Self);
  FBookmarkStrings:=TStringList.Create;
  FBookmarkIndexes:=TIndexedList.Create;
end;  

destructor THtmlTree.Destroy;
begin
  DocHandle.Destroy;
  FSubTree.Destroy;
  FBookmarkStrings.Destroy;
  FBookmarkIndexes.Destroy;
end;  

//procedure LoadFromFile(const AFileName:String);

procedure THtmlTree.SaveToFile(const AFileName:String);
begin
  WriteXMLFile(FDocHandle,AFileName);
end;

procedure THtmlTree.Clear;
begin
  FDocHandle.RemoveChild(FRootNode);
  ResetNodes;
  SubTree.Clear;
  FBookmarkIndexes.Clear;
  FBookmarkStrings.Clear;
end;

procedure THtmlTree.AddCSSLink(const AFilePath: String);
var
  LinkEl: TDOMElement;
begin
  LinkEl:= FDocHandle.CreateElement('link');
  LinkEl['rel'] := 'stylesheet';
  LinkEl['type'] := 'text/css';
  LinkEl['href'] := AFilePath;
  FHeadNode.AppendChild(LinkEl);  
end; 

procedure THtmlTree.AddJSLink(const AFilePath:String);
var
  LinkEl:TDOMElement;
begin
  LinkEl:= FDocHandle.CreateElement('script');
  LinkEl['language'] := 'JavaScript1.5';
  LinkEl['type'] := 'text/javascript';
  LinkEl['src'] := AFilePath;
  LinkEl.AppendChild(FDocHandle.CreateTextNode(''));
  FHeadNode.AppendChild(LinkEl);  
end;

procedure THtmlTree.SetTitle(const AName: String);
var
  TempNode: TDOMElement;
begin
  FTitle := AName;
  TempNode := FDocHandle.CreateElement('title');
  TempNode.AppendChild(FDocHandle.CreateTextNode(AName));
  FHeadNode.AppendChild(TempNode);
end;

procedure THtmlTree.ResetNodes;
begin
  FRootNode:=FDocHandle.CreateElement('html');
  FHeadNode:=FDocHandle.CreateElement('head');
  FBodyNode:=FDocHandle.CreateElement('body');
  FRootNode.AppendChild(FHeadNode);
  FRootNode.AppendChild(FBodyNode);
  FDocHandle.AppendChild(FRootNode);
end;

function THtmlTree.CreateText(const AText: String): TDomNode;
begin
  Result:=FDocHandle.CreateTextNode(AText);
end;

procedure THtmlTree.AddBookMark(ANode: TDomNode; const AText: String);
begin
  //todo:handle duplicates
  FBookmarkStrings.AddObject(AText,ANode);
end;

procedure THtmlTree.AddBookMark(ANode: TDomNode; const AIndex: Integer);
begin
  FBookmarkIndexes.Add(AIndex,ANode);
end;

function THtmlTree.GetBookmark(const AText: String): TDomNode;
var
  i:Integer;
begin
  i:=FBookmarkStrings.IndexOf(AText);
  if i <> -1 then
    Result:=TDomNode(FBookmarkStrings.Objects[i])
  else
    raise Exception.Create('Could not find a node bookmarked as "'+AText+'"');
end;

function THtmlTree.GetBookmark(const AIndex: Integer): TDomNode;
begin
  if not FBookmarkIndexes.Get(AIndex,Pointer(Result)) then
    raise Exception.Create('Could not find a node with index = '+IntToStr(AIndex));
end;

//TDbColumn

procedure TDbColumn.UpdateField(ADataSet:TDataSet);
begin
  FField:=ADataSet.FieldByName(FFieldName);
end;

constructor TDbColumn.Create(ADataset:TDataset; AFieldName:String);
begin
  FFieldName:=AFieldName;
  FField:=ADataset.FieldByName(AFieldName);
  if FField = nil then 
    raise Exception.Create('TDBTree - Field '+AFieldName+' not found');
end;

destructor TDbColumn.Destroy;
begin
end;    

function TDbColumn.GetData: String;
begin
  if FCallback = nil then
    Result:=FField.AsString
  else
    Result:=FCallback();
end;

//TDbColumns

constructor TDbColumns.Create(AParent: TDBTree);
begin
  FParent:=AParent;
  FColumnList:=TList.Create;
end;

destructor TDbColumns.Destroy;
begin
  Clear;
  FColumnList.Free;
end;

function TDbColumns.GetCount:Integer;
begin
  Result:=FColumnList.Count;
end;

function TDbColumns.GetColumn(Index:Integer):TDbColumn;
begin
  Result:=TDbColumn(FColumnList[Index]);
end;

procedure TDbColumns.Add(AFieldName,ATitle:String;ACallback:TColumnCallback);
var
  AColumn:TDbColumn;
begin
  AColumn:=TDbColumn.Create(FParent.Dataset,AFieldName);
  AColumn.Title:=ATitle;
  AColumn.Callback:=ACallBack;
  FColumnList.Add(Pointer(AColumn));
end;

procedure TDbColumns.Add(AFieldName,ATitle:String);
begin
  Add(AFieldName,ATitle,nil);
end;

procedure TDbColumns.Add(AFieldName:String);
begin
  Add(AFieldName,AFieldName,nil);
end;

procedure TDbColumns.Add(AFieldName:String;ACallback:TColumnCallback);
begin
  Add(AFieldName,AFieldName,ACallBack);
end;

procedure TDbColumns.Clear;
var 
  Counter:Integer;
begin
  for Counter := 0 to FColumnList.Count - 1 do
    TDbColumn(FColumnList[Counter]).Destroy;
  FColumnList.Clear;
end;  

// TDBTree

constructor TDBTree.Create(AParent:THtmlTree);
begin
  Inherited Create(AParent);
  FColumns:= TDbColumns.Create(Self);
  FShowColumnTitles:=True;
end;

destructor TDBTree.Destroy;
begin
  Inherited Destroy;
end;

procedure TDBTree.UpdateColumns;
var
  i:Integer;
begin
  if Columns.Count = 0 then
  begin
    if Dataset.Fields.Count = 0 then
    begin
      raise Exception.Create('TDBTree - The dataset has no fields');
    end;
    for i := 0 to Dataset.Fields.Count - 1 do
      Columns.Add(Dataset.Fields[i].FieldName);
  end else
  begin
    for i:= 0 to Columns.Count - 1 do
      Columns[i].UpdateField(DataSet);
  end;
end;

procedure TDBTree.BuildTable;
var
  i,RecCounter: Integer;
begin
  UpdateColumns;
  
  if FTitle <> '' then
  begin
    AddRow;
    Last['class']:='tabletitle';
    AddCol(FTitle);
    Last['colspan']:=IntToStr(Columns.Count);
  end;

  if ShowColumnTitles then
  begin
    AddRow;
    Last['class']:='columntitle';
    for i:= 0 to Columns.Count - 1 do
      AddCol(Columns[i].Title);
  end;

  //Setting RecordLimit to anything than 0 will make a table with RecordLimit rows
  //this is useful to spliting the same dataset around several tables
  if FRecordLimit = 0 then
    Dataset.First;

  RecCounter:=0;
  while not Dataset.Eof  do
  begin
    AddRow;
    for i := 0 to Columns.Count - 1 do
      AddCol(Columns[i].GetData);
    Dataset.Next;
    Inc(RecCounter);
    if (FRecordLimit > 0) and (RecCounter = FRecordLimit) then
      Break;
  end; //while
end;

{ THtmlSubTree }

function THtmlSubTree.GetLastElement: TDOMElement;
begin
  Result:=TDomElement(FNodeList.Last);
end;

function THtmlSubTree.GetBaseElement: TDOMElement;
begin
  if FBaseIndex = -1 then
    Result:=TDOMElement(FNodeList[0])
  else
    Result:=TDOMElement(FNodeList[FBaseIndex]);
end;

constructor THtmlSubTree.Create(AParent: THtmlTree);
begin
  FBaseIndex:=-1;
  FNodeList:=TFPList.Create;
  FParent:=AParent;
end;

destructor THtmlSubTree.Destroy;
begin
  if FNodeList.Count > 0 then
    TDomNode(FNodeList[0]).Free;
  FNodeList.Destroy;
end;

function THtmlSubTree.AddTo(ANode: TDomNode; const ATag: String): TDomNode;
begin
  Result:=FParent.DocHandle.CreateElement(ATag);
  if ANode <> nil then
    ANode.AppendChild(Result);
  FNodeList.Add(Result);
end;

function THtmlSubTree.Add(const ATag: String):TDomNode;
begin
  Result:=FParent.DocHandle.CreateElement(ATag);
  if FNodeList.Count > 0 then
    Base.AppendChild(Result);
  FNodeList.Add(Result);
end;

procedure THtmlSubTree.AddText(const AText: String);
begin
  if FNodeList.Count > 0 then
    TDomNode(FNodeList.Last).AppendChild(FParent.DocHandle.CreateTextNode(AText));
end;

function THtmlSubTree.AddBase(const ATag: String):TDomNode;
begin
  Result:=Add(ATag);
  FBaseIndex:=FNodeList.Count - 1;
end;

procedure THtmlSubTree.AddListItem(const AText: String);
begin
  Add('li');
  Last.AppendChild(FParent.DocHandle.CreateTextNode(AText));
end;

procedure THtmlSubTree.AttachTo(AParent: TDomNode);
begin
  if FNodeList.Count > 0 then
  begin
    AParent.AppendChild(TDomNode(FNodeList[0]));
    FNodeList.Clear;
    FBaseIndex:= -1;
  end;
end;

procedure THtmlSubTree.Attach;
begin
  AttachTo(FParent.BodyNode);
end;

procedure THtmlSubTree.Clear;
begin
  if FNodeList.Count > 0 then
    TDomNode(FNodeList[0]).Free;
  FNodeList.Clear;
  FBaseIndex:= -1;
end;

{ TTableTree }

constructor TTableTree.Create(AParent: THtmlTree);
begin
  Inherited Create(AParent);
  FTableNode:=Add('table');
end;

destructor TTableTree.Destroy;
begin
  Inherited Destroy;
end;


procedure TTableTree.AddRow;
begin
  FRowNode:=AddTo(FTableNode,'tr');
end;

procedure TTableTree.AddCol(const AText: String);
begin
  AddTo(FRowNode,'td');
  Last.AppendChild(FParent.DocHandle.CreateTextNode(Atext));
end;

procedure TTableTree.AttachTo(AParent: TDomNode);
begin
  inherited AttachTo(AParent);
  FTableNode:=Add('table');
end;

procedure TTableTree.Clear;
begin
  inherited Clear;
  FTableNode:=Add('table');
end;


end.
