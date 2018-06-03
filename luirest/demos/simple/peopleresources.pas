unit PeopleResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiRESTServer, HTTPDefs;

type

  { TPerson }

  TPerson = class(TRESTResource)
  private
    class var ContentStr: String;
  public
    constructor Create; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TPersonDetails }

  TPersonDetails = class(TPerson)
  public
    constructor Create; override;
  end;

  { TPeople }

  TPeople = class(TRESTResource)
  protected
  public
    constructor Create; override;
    procedure HandleGet(ARequest: TRequest; AResponse: TResponse); override;
  end;


implementation

const
  PeopleStr = '[{"id":1, "name":"Luiz Américo"}]';
  PersonStr = '{"id":1, "name":"Luiz Américo"}';
  PersonDetailsStr = '{"id":1, "name":"Luiz Américo", "email":"xxx@yahoo.com.br", "phone": "5555-8888"}';

{ TPersonDetails }

constructor TPersonDetails.Create;
begin
  inherited Create;
  ContentStr := PersonDetailsStr;
end;

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  RegisterSubPath('details', TPersonDetails, 0);
  ContentStr := PersonStr;
end;

procedure TPerson.HandleGet(ARequest: TRequest;
  AResponse: TResponse);
var
  i: Integer;
begin
  i := URIParams.IndexOfName('id');
  if (i = -1) or (URIParams.Items[i].AsString <> '1') then
    AResponse.Code := 404
  else
    AResponse.Contents.Add(ContentStr);
end;

{ TPeople }

constructor TPeople.Create;
begin
  inherited Create;
  SetDefaultSubPath('id', TPerson, 0);
end;


procedure TPeople.HandleGet(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Contents.Add(PeopleStr);
end;

end.

