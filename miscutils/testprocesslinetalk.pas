program template;
{$Mode ObjFpc}
{$H+}

{ $define DEBUGHEAP}
uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
  crt,sysutils,classes,processlinetalk;
var	
  AProcess: TProcessLineTalk;
  Line: String;

begin //Main 
  AProcess := TProcessLineTalk.Create(nil);
 // SetCurrentDir('C:\bin\fileutils\7-Zip\');
  AProcess.CommandLine := 'C:\bin\fileutils\7-Zip\7z.exe l -slt "1999 - Hore, Mitakotoka! Seikimatsu.7z"';
  AProcess.Execute;
  Line := AProcess.ReadLine;
  while Line <> '' do
  begin
    WriteLn('"',Line,'"');
    Line := AProcess.ReadLine; 
  end;  
  AProcess.Destroy;
end.