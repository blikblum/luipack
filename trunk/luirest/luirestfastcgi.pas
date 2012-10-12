unit LuiRESTFastCGI;

{$mode objfpc}
{$H+}

interface

uses
  Classes, custfcgi, sysutils;

Type

  { TRESTFCGIApplication }

  TRESTFCGIApplication = class(TCustomFCGIApplication)

  end;

var
  Application : TRESTFCGIApplication;
  ShowCleanUpErrors : Boolean = False;

Implementation

uses CustApp;

Procedure InitFCGI;

begin
  Application:=TRESTFCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneFCGI;

begin
  Try
    if CustomApplication=Application then
      CustomApplication := nil;
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Initialization
  InitFCGI;

Finalization
  DoneFCGI;

end.

