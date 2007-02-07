program testHtmlTree;
{$Mode ObjFpc}
{$H+}

{$define DEBUGHEAP}
uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
  domutils,sysutils,classes,sqliteds;
var	
  i:Integer;
  ATree:THtmlTree;
  ATable:TDbTree;
  dsTest:TSqliteDataset;

begin //Main 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.txt');
  {$endif}
  dsTest:=TSqliteDataset.Create(nil);
  with dsTest do
  begin
    FileName:='benchDbResults.db';
    TableName:='Results';
    Open;
  end;  
  ATree:=THtmlTree.Create;
  with ATree do
  begin
    Title:='Small Test to TDBTree class';
    ATable:=TDBTree.Create(ATree);
    with ATable do
    begin
      Dataset:=dsTest;
      BuildTable;
      Attach;
      Destroy;
    end;
    SaveToFile('testDbTree.html');    
    Destroy;
  end;
  dsTest.Destroy;
end.
