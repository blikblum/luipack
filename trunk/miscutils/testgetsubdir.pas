program template;
{$Mode ObjFpc}
{$H+}

{ $define DEBUGHEAP}
uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
  crt,sysutils,classes,miscutils;
var	
  AList:TStrings;
  i:Integer;

begin //Main 
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.txt');
  {$endif}
  AList:=TStringList.Create;
  GetDirectoryTree('e:\work\pascal',AList);
  for i:= 0 to AList.Count -1 do
    Writeln(AList[i]);
  AList.Destroy;
end.