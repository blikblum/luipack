unit SqliteTrees;

{
  SqliteTrees provides a way to iterate with sqlite data modeled as a tree

  Copyright (C) 2008 Luiz Americo Pereira Camara
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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, customsqliteds, Sqlite3Wrapper, db;

type

  TCustomTreeIterator = class;

  TRecordNotify = procedure (Sender: TCustomTreeIterator; HasChild: Boolean) of object;
  TParentChangeNotify = procedure (Sender: TCustomTreeIterator; Parent: Integer) of object;

  { TCustomTreeIterator }

  TCustomTreeIterator = class (TComponent)
  private
    FFilter: String;
    FFieldNames: String;
    FIndexFieldName: String;
    FLevel: Integer;
    FOnParentChange: TParentChangeNotify;
    FOnRecord: TRecordNotify;
    FParentFieldName: String;
    FTableName: String;
    FSqlTemplate: String;
    FHasChildSql: String;
    //Boolean fields
    FBreakRecursion: Boolean;
  protected
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); virtual; abstract;
    procedure GetChild(Parent: Integer);
  public
    Data: Pointer;
    constructor Create(AOwner: TComponent); override;
    procedure Run(Parent: Integer; Recurse: Boolean = True); virtual; abstract;
    property TableName: String read FTableName write FTableName;
    property FieldNames: String read FFieldNames write FFieldNames;
    property ParentFieldName: String read FParentFieldName write FParentFieldName;
    property IndexFieldName: String read FIndexFieldName write FIndexFieldName;
    property Filter: String read FFilter write FFilter;
    property Level: Integer read FLevel;
    property BreakRecursion: Boolean read FBreakRecursion write FBreakRecursion;
    property OnRecord: TRecordNotify read FOnRecord write FOnRecord;
    property OnParentChange: TParentChangeNotify read FOnParentChange write FOnParentChange;
  end;

  { TDatasetTreeIterator }

  TDatasetTreeIterator = class (TCustomTreeIterator)
  private
    FDataset: TCustomSqliteDataset;
    FIndexField: TField;
  protected
    property IndexField: TField read FIndexField;
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); override;
  public
    procedure Run(Parent: Integer; Recurse: Boolean = True); override;
    property Dataset: TCustomSqliteDataset read FDataset write FDataset;
  end;


  { TSqlite3TreeIterator }

  TSqlite3TreeIterator = class (TCustomTreeIterator)
  private
    FDatabase: TSqlite3Database;
    FIndexField: Integer;
    FReader: TSqlite3DataReader;
  protected
    property IndexField: Integer read FIndexField;
    procedure DoBrowseRecords(Parent: Integer; ChildList: TFpList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run (Parent: Integer; Recurse: Boolean = True); override;
    property Connection: TSqlite3Database read FDatabase write FDatabase;
    property Reader: TSqlite3DataReader read FReader;
  end;


implementation

{ TDatasetTreeIterator }

procedure TDatasetTreeIterator.DoBrowseRecords(Parent: Integer; ChildList: TFpList);
var
  HasChild: Boolean;
  CurrentLinkValue: Integer;
begin
  with FDataset do
  begin
    Sql := FSqlTemplate + IntToStr(Parent) + FFilter;
    RefetchData;
    while not Eof do
    begin
      CurrentLinkValue := FIndexField.AsInteger;
      ExecuteDirect(FHasChildSql + IntToStr(CurrentLinkValue) + ' Limit 1');
      HasChild := ReturnCode = 100 {SQLITE_ROW};
      FOnRecord(Self, HasChild);
      if HasChild then
        ChildList.Add(Pointer(PtrInt(CurrentLinkValue)));
      Next;
    end;
  end;
end;

procedure TDatasetTreeIterator.Run (Parent: Integer; Recurse: Boolean = True);
var
  OldSaveOnRefetch, OldActive: Boolean;
  OldSql: String;
  OldRecNo: Integer;
begin
  if not Assigned(FOnRecord) then
    raise Exception.Create('OnRecord notify function not set');
  with Dataset do
  begin
    //save ds state
    OldSaveOnRefetch := SaveOnRefetch;
    SaveOnRefetch := False;
    OldSql := Sql;
    OldActive := Active;
    if OldActive then
    begin
      ApplyUpdates;
      OldRecNo := RecNo;
    end;
    //prepare template
    if FFieldNames = '' then
      FSqlTemplate := 'Select * from '
    else
      FSqlTemplate := 'Select ' + FFieldNames + ' from ';
    FSqlTemplate := FSqlTemplate + FTableName + ' Where ';
    FHasChildSql := 'Select _ROWID_ from ' + FTableName + ' Where ' + FParentFieldName + ' = ';
    DisableControls;
    //dummy open to allow refetchdata
    Sql := FSqlTemplate + '1 = 0';
    Close;
    Open;
    //finish SqlTemplate
    FSqlTemplate := FSqlTemplate + FParentFieldName + ' = ';
    FIndexField := FieldByName(FIndexFieldName);
    FLevel := 0;
    FBreakRecursion := not Recurse;
    //start iteration
    GetChild(Parent);
    //restore ds state
    SaveOnRefetch := OldSaveOnRefetch;
    Sql := OldSql;
    Close;
    if OldActive then
    begin
      Open;
      RecNo := OldRecNo;
    end;
    EnableControls;
  end;
end;

{ TSqlite3TreeIterator }

procedure TSqlite3TreeIterator.DoBrowseRecords(Parent: Integer; ChildList: TFpList);
var
  HasChild: Boolean;
  CurrentLinkValue: Integer;
begin
  with FDatabase, FReader do
  begin
    Prepare(FSqlTemplate + IntToStr(Parent) + FFilter, FReader);
    while Step do
    begin
      CurrentLinkValue := GetInteger(FIndexField);
      ExecSql(FHasChildSql + IntToStr(CurrentLinkValue) + ' Limit 1');
      HasChild := ReturnCode = 100 {SQLITE_ROW};
      FOnRecord(Self, HasChild);
      if HasChild then
        ChildList.Add(Pointer(PtrInt(CurrentLinkValue)));
    end;
    Finalize;
  end;
end;

constructor TSqlite3TreeIterator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReader := TSqlite3DataReader.Create;
end;

destructor TSqlite3TreeIterator.Destroy;
begin
  FReader.Destroy;
  inherited Destroy;
end;

procedure TSqlite3TreeIterator.Run(Parent: Integer; Recurse: Boolean);
begin
  if not Assigned(FOnRecord) then
    raise Exception.Create('OnRecord notify function not set');
  with FDatabase do
  begin
    if FFieldNames = '' then
      FSqlTemplate := 'Select * from '
    else
      FSqlTemplate := 'Select ' + FFieldNames + ' from ';
    FSqlTemplate := FSqlTemplate + FTableName + ' Where ';
    Open;
    Prepare(FSqlTemplate + '1 = 0', FReader);
    FIndexField := FReader.GetFieldIndex(FIndexFieldName);
    FReader.Finalize;
    if FIndexField = -1 then
      raise Exception.Create('Index Field "' + FIndexFieldName + '" Not Found');
    FSqlTemplate := FSqlTemplate + FParentFieldName + ' = ';
    FHasChildSql := 'Select _ROWID_ from ' + FTableName + ' Where ' + FParentFieldName + ' = ';
    FLevel := 0;
    FBreakRecursion := not Recurse;
    //start iteration
    GetChild(Parent);
  end;
end;

{ TCustomTreeIterator }

procedure TCustomTreeIterator.GetChild(Parent: Integer);
var
  i: Integer;
  HasChildList: TFpList;
begin
  if Assigned(FOnParentChange) then
    FOnParentChange(Self, Parent);
  HasChildList := TFPList.Create;
  DoBrowseRecords(Parent, HasChildList);
  Inc(FLevel);
  for i := 0 to HasChildList.Count - 1 do
    if not FBreakRecursion then
      GetChild(PtrInt(HasChildList[i]));
  Dec(FLevel);
  HasChildList.Destroy;
end;

constructor TCustomTreeIterator.Create(AOwner: TComponent);
begin
  //todo: see is necessary this constructor
  inherited Create(AOwner);
end;

end.

