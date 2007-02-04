unit ATxFProcFPC;

{$mode objfpc}{$H+}

interface

{$i ATxFProcFPCh.inc}

function FFileOpen(const fn: WideString): THandle;
function FGetFileSize(Handle: THandle): Int64; overload;

implementation

function FFileOpen(const fn: WideString): THandle;
begin
 //String(fn) would work??
  Result:= FileOpen(WideCharToString(PWideChar(fn)),fmOpenRead);
end;


{$i ATxFProcFPC.inc}


end.

