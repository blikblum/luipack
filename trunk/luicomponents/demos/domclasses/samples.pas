unit Samples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOMClasses;

procedure SimpleHtmlTree(out SampleFile: String; Stream: TStream);

procedure RawListHtmlTree(out SampleFile: String; Stream: TStream);

procedure ListHtmlTree(out SampleFile: String; Stream: TStream);

procedure TableHtmlTree(out SampleFile: String; Stream: TStream);

implementation

procedure SimpleHtmlTree(out SampleFile: String; Stream: TStream);
var
  Tree: THTMLTree;
begin
  SampleFile := 'simplesample.inc';
  {$i simplesample.inc}
end;

procedure RawListHtmlTree(out SampleFile: String; Stream: TStream);
var
  Tree: THtmlTree;
begin
  SampleFile := 'rawlistsample.inc';
  {$i rawlistsample.inc}
end;

procedure ListHtmlTree(out SampleFile: String; Stream: TStream);
var
  Tree: THtmlTree;
begin
  SampleFile := 'listsample.inc';
  {.$i listsample.inc}
end;

procedure TableHtmlTree(out SampleFile: String; Stream: TStream);
var
  Tree: THtmlTree;
  //Table: TTableTree;
begin
  SampleFile := 'tablesample.inc';
  //{. $i tablesample.inc}
end;

end.

