

function OSSupportsUTF16: Boolean;
begin
  Result:= Win32Platform = VER_PLATFORM_WIN32_NT;
end;
