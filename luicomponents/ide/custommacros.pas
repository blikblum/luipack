unit CustomMacros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MacroIntf, MacroDefIntf, DirectorySelectView, LazIDEIntf,
  LazFileUtils, Controls;

type

  { TMacroCallbacks }

  TMacroCallbacks = object
    function ResolveCustomDir(const Param: String; const Data: PtrInt;
      var Abort: Boolean): String;
  end;

procedure Register;

implementation

const
  CustomDirPrefix = 'CustomDir.';

var
  Callbacks: TMacroCallbacks;

procedure Register;
begin
  IDEMacros.Add(TTransferMacro.Create('CustomDir', '', 'Custom Directory',
    @Callbacks.ResolveCustomDir, [tmfInteractive]));
end;

{ TMacroCallbacks }

function TMacroCallbacks.ResolveCustomDir(const Param: String;
  const {%H-}Data: PtrInt; var Abort: Boolean): String;
var
  PropName: String;
begin
  PropName := CustomDirPrefix + Param;
  Result := LazarusIDE.ActiveProject.CustomSessionData.Values[PropName];
  if not DirectoryExistsUTF8(Result) then
  begin
    with TDirectorySelectForm.Create(nil) do
    begin
      try
        DirLabel.Caption := Param;
        Directory := Result;
        Abort := ShowModal <> mrOK;
        if not Abort then
        begin
          Result := Directory;
          LazarusIDE.ActiveProject.CustomSessionData.Values[PropName] := Result;
          LazarusIDE.ActiveProject.Modified := True;
        end
        else
          Result := '';
      finally
        Destroy;
      end;
    end;
  end;
end;

end.

