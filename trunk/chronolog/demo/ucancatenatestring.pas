unit uCancatenateString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ChronoLog;

procedure TestConcatenateString(Log: TChronoLog);

implementation

procedure TestConcatenateString(Log: TChronoLog);

var
  i: Integer;
  AStr, AnotherStr, YetAnotherStr: String;

begin
  with Log do
  begin
    AStr := '';
    AnotherStr := 'xxx';
    YetAnotherStr := '---';
    SessionName := 'ConcatenateStringTest';
    RegisterId(0, 'Reassigning AnsiString');
    Start;
    for i := 1 to 1000 do
      AStr := AStr + ' Plus ' + AnotherStr + ' Plus ' + YetAnotherStr;
    Stop(0);
  end;
end;

end.

