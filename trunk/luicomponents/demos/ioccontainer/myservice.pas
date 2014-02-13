unit MyService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { IMyService }

  IMyService = interface
    ['{DDE6F19B-480A-4D76-AA66-0D52F5CBE899}']
    function GetDescription: String;
  end;

  IMySecondService = interface
    ['{E1B60CF0-7466-400B-930E-D5B381C212DD}']
    function GetDescription: String;
  end;

implementation

end.

