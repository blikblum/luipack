program testsqlitetree;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, dbutils, sqliteds;

type

  { TDummy }

  TDummy = class
    procedure TestCallback(Tree: TSqliteTree; Index: Integer; HasChild: Boolean);
  end;

{ TDummy }

procedure TDummy.TestCallback(Tree: TSqliteTree; Index: Integer;
  HasChild: Boolean);
begin
  writeln('Index: ', Index);
  WriteLn('HasChild: ',HasChild);
  WriteLn('Tree.Dataset.FieldByName(''Code''): ',Tree.Dataset.FieldByName('Code').AsString);
  WriteLn('Tree.Level: ', Tree.Level);
end;

var
  ATree: TSqliteTree;
  ADb: TSqliteDataset;
  ADummy: TDummy;
  
begin
  ADummy:=TDummy.Create;
  ADb:=TSqliteDataset.Create(nil);
  with ADb do
  begin
    FileName:='testtree.db';
  end;
  ATree:=TSqliteTree.Create;
  with ATree do
  begin
    TableName:='Topics';
    ParentFieldName:='Parent';
    IndexFieldName:='Code';
    InitialParentValue:=-1;
    Dataset:=Adb;
    Iterate(@ADummy.TestCallback);
    Destroy;
  end;
  ADb.Destroy;
  ADummy.Destroy;
end.

