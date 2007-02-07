program testHtmlTree;
{$Mode ObjFpc}
{$H+}

{$define DEBUGHEAP}
uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
  domutils,sysutils,classes;
var	
  i:Integer;
  ATree:THtmlTree;
  ATable:TTableTree;

begin //Main 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.txt');
  {$endif}
  ATree:=THtmlTree.Create;
  with ATree do
  begin
    Title:='Small Test to THTMLTree class';
    with SubTree do
    begin
      Add('ul');
      Add('li');
      AddText('List Created with Raw Functions');
      Attach;
      Add('p');
      AddText('AParagraph');
      Attach;
      AddBase('ul');
      AddListItem('First Item of a');
      AddListItem('List Created');
      AddListItem('With Especialized list functions');
      Attach;
    end;
    ATable:=TTableTree.Create(ATree);
    with ATable do
    begin
      AddRow;

      AddCol('Row1Col1');
      AddCol('Row1Col2');
      AddCol('Row1Col3');
      AddRow;
      AddCol('Row2Col1');
      AddCol('Row2Col2');
      AddCol('Row2Col3');

      Attach;
      Destroy;
    end;
    SaveToFile('testHtmlTree.html');    
    Destroy;
  end;
end.
