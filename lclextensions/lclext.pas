unit lclext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function OSSupportsUTF16: Boolean;

implementation

uses
{$i uses_lclext.inc}
  LCLType;

{$i lclext.inc}

end.

