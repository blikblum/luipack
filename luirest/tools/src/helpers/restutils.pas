unit RESTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, httpsend;

function DoRequest(const Method, URL, Payload: String;
  out Response: String; out Code: Integer): Boolean;

function DoRequest(EndPointData: TJSONObject; const Payload: String;
  out Response: String; out Code: Integer): Boolean;

function FormatTest(const Method, Schema: String): String;

implementation

uses
  LuiJSONUtils, LuiJSONHelpers;

function DoRequest(const Method, URL, Payload: String; out Response: String;
  out Code: Integer): Boolean;
var
  Http: THTTPSend;
  ResponseData: TJSONData;
begin
  Http := THTTPSend.Create;
  Response := '';
  Code := 0;
  try
    if Length(Payload) > 0 then
      Http.Document.Write(Payload[1], Length(Payload));
    Result := Http.HTTPMethod(Method, URL);
    if Result then
    begin
      Code := Http.ResultCode;
      if TryStreamToJSON(Http.Document, ResponseData) then
      begin
        Response := ResponseData.FormatJSON();
        ResponseData.Destroy;
      end
      else
      begin
        Http.Document.Position := 0;
        SetLength(Response, Http.Document.Size);
        Http.Document.Write(Response[1], Http.Document.Size);
      end;
    end;
  finally
    Http.Destroy;
  end;
end;

function DoRequest(EndPointData: TJSONObject; const Payload: String; out Response: String;
  out Code: Integer): Boolean;
begin
  Result := DoRequest(EndPointData.GetPath('request.method', ''),
    EndPointData.GetPath('request.url', ''), Payload, Response, Code);
end;

const
  GETTestTemplate =
    'var schema = %s' + LineEnding + LineEnding +

    'pm.test("Status code is 200", function () {' + LineEnding +
    '  pm.response.to.have.status(200);' + LineEnding +
    '});' + LineEnding + LineEnding +

    'pm.test("Response matches schema", function () {' + LineEnding +
    '  pm.response.to.have.jsonSchema(schema);' + LineEnding +
    '});';

  POSTPUTTestTemplate =
    'var schema = %s' + LineEnding + LineEnding +
                                  
    'pm.test("Status code is %d", function () {' + LineEnding +
    '  pm.response.to.have.status(%d);' + LineEnding +
    '});' + LineEnding + LineEnding +

    'pm.test("Response matches schema", function () {' + LineEnding +
    '  pm.response.to.have.jsonSchema(schema);' + LineEnding +
    '});' + LineEnding + LineEnding +

    'pm.test("Response content equals request", function () {' + LineEnding +
    '  var requestData = JSON.parse(pm.request.body.raw)' + LineEnding +
    '  var responseData = pm.response.json()' + LineEnding +
    '  var keys = _.intersection(_.keys(requestData), _.keys(responseData));' + LineEnding +
    '  pm.expect(_.pick(responseData, keys)).to.eql(_.pick(requestData, keys));' + LineEnding +
    '});';

function FormatTest(const Method, Schema: String): String;
begin
  Result := Schema;
  case Method of
    'GET':
      Result := Format(GETTestTemplate, [Schema]);
    'POST':
      Result := Format(POSTPUTTestTemplate, [Schema, 201, 201]);
    'PUT':
      Result := Format(POSTPUTTestTemplate, [Schema, 200, 200]);
  end;
end;

end.

