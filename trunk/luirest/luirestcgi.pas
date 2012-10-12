unit LuiRESTCGI;

{$mode objfpc}
{$H+}

interface

uses
  Classes, sysutils, custweb, custcgi;

type

  { TRESTCGIRequest }

  TRESTCGIRequest = class(TCGIRequest)
  protected
    //do nothing
    procedure InitPostVars; override;
    //dont fail on unknow methods
    procedure InitRequestVars; override;
  end;

  { TRESTCGIHandler }

  TRESTCGIHandler = class(TCgiHandler)
  protected
    function CreateRequest: TCGIRequest; override;
  end;

  { TRESTCGIApplication }

  TRESTCGIApplication = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

var
  Application : TRESTCGIApplication;
  ShowCleanUpErrors : Boolean = False;

Implementation

uses CustApp;

Procedure InitCGI;

begin
  Application:=TRESTCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneCGI;

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

{ TRESTCGIApplication }

function TRESTCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TRESTCGIHandler.Create(Self);
end;

{ TRESTCGIRequest }

procedure TRESTCGIRequest.InitPostVars;
begin
  //
end;

procedure TRESTCGIRequest.InitRequestVars;
var
  AMethod : String;
begin
  AMethod := Method;
  if (AMethod = '') then
    raise Exception.Create('No REQUEST_METHOD passed from server.');
  if CompareText(AMethod,'POST')=0 then
    begin
    InitPostVars;
    if HandleGetOnPost then
      InitGetVars;
    end
  else if (CompareText(AMethod,'GET')=0) or (CompareText(AMethod,'HEAD')=0) or (CompareText(AMethod,'OPTIONS')=0) then
    InitGetVars;
end;

{ TRESTCGIHandler }

function TRESTCGIHandler.CreateRequest: TCGIRequest;
begin
  Result := TRESTCGIRequest.CreateCGI(Self);
end;

Initialization
  InitCGI;

Finalization
  DoneCGI;

end.


