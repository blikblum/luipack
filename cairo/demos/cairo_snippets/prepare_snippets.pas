program prepare_snippets;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, strutils;

const
  SampleDir = 'snippets' + PathDelim;
  ExcludeFilters: array [0..1] of String = ('libsvg*','operator_*');
  
var
  UnitFile: Text;
  FileInfo: TSearchRec;
  SnippetList: TStringList;
  i: Integer;
  
procedure CreateSnippetFunction(const SnippetName: String);
var
  Source: TStringList;
  j: Integer;
begin
  WriteLn(UnitFile, 'procedure do_'+ SnippetName+'(cr: PCairo_t; width, height: Integer);');
  Source := TStringList.Create;
  Source.LoadFromFile(SampleDir + SnippetName + '.cairo');
  for j := 0 to Source.Count - 1 do
    WriteLn(UnitFile, Source[j]);
  WriteLn(UnitFile);
  Source.Destroy;
end;

function MatchExclude(const FileName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 1 do
  begin
    if IsWild(FileName, ExcludeFilters[i], True) then
      Exit(True)
  end;
end;

begin
  SnippetList := TStringList.Create;
  SnippetList.Sorted := True;
  //get the snippet filenames
  if FindFirst(SampleDir + '*.cairo', faAnyFile, FileInfo) = 0 then
  repeat
    if not MatchExclude(FileInfo.Name) then
    begin
      i := Pos('.', FileInfo.Name);
      SnippetList.Add(Copy(FileInfo.Name, 1, i - 1));
    end;
  until FindNext(FileInfo) <> 0;
  //build the unit file
  Assign(UnitFile, 'snippets.pas');
  Rewrite(UnitFile);
  //the header
  WriteLn(UnitFile, 'unit snippets;', LineEnding);
  WriteLn(UnitFile, 'interface', LineEnding);
  WriteLn(UnitFile, 'uses',LineEnding,'  cairo, math;',LineEnding);
  WriteLn(UnitFile, 'const');
  WriteLn(UnitFile, '  snippet_count = ',SnippetList.Count, ';');
  WriteLn(UnitFile, '  snippet_name: array [0..', SnippetList.Count - 1, '] of String = (');
  for i := 0 to SnippetList.Count - 1 do
    WriteLn(UnitFile, '    ''', SnippetList[i], '''', ifthen(i < SnippetList.Count - 1, ','));
  WriteLn(UnitFile, '   );', LineEnding);
  WriteLn(UnitFile, 'procedure snippet_do(cr: PCairo_t; snippet_no, width, height: Integer);', LineEnding);
  WriteLn(UnitFile, 'procedure snippet_normalize(cr: PCairo_t; width, height: Integer);', LineEnding);
  WriteLn(UnitFile, 'implementation', LineEnding);
  for i := 0 to SnippetList.Count - 1 do
    CreateSnippetFunction(SnippetList[i]);
  WriteLn(UnitFile, 'type');
  WriteLn(UnitFile, '  TSnippetProc = procedure (cr: PCairo_t; width, height: Integer);', LineEnding);
  WriteLn(UnitFile, 'const');
  WriteLn(UnitFile, '  snippet_functions: array [0..', SnippetList.Count - 1, '] of TSnippetProc = (');
  for i := 0 to SnippetList.Count - 1 do
    WriteLn(UnitFile, '    @do_', SnippetList[i], ifthen(i < SnippetList.Count - 1, ','));
  WriteLn(UnitFile, '  );', LineEnding);
  
  WriteLn(UnitFile, 'procedure snippet_do(cr: PCairo_t; snippet_no, width, height: Integer);');
  WriteLn(UnitFile, 'begin');
  WriteLn(UnitFile, '  if (snippet_no >= 0) and (snippet_no < snippet_count) then');
  WriteLn(UnitFile, '    snippet_functions[snippet_no](cr, width, height);');
  WriteLn(UnitFile, 'end;', LineEnding);
  
  WriteLn(UnitFile, 'procedure snippet_normalize(cr: PCairo_t; width, height: Integer);');
  WriteLn(UnitFile, 'begin');
  WriteLn(UnitFile, '  cairo_scale (cr, width, height);', LineEnding,
    '  cairo_set_line_width (cr, 0.04);', LineEnding, 'end;');
  WriteLn(UnitFile,'end.');
  Close(UnitFile);

  SnippetList.Destroy;
end.

