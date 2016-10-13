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
    'var responseData = JSON.parse(responseBody);' + LineEnding + LineEnding +
    'var schema = %s' + LineEnding +
    'tests["Status code is 200"] = responseCode.code === 200;' + LineEnding +
    'tests["Response data matches schema"] = tv4.validate(responseData, schema);' + LineEnding +
    'if (tv4.error) {' + LineEnding +
    ' console.log("Validation failed: ", tv4.error);' + LineEnding +
    '}';

  POSTPUTTestTemplate =
     'var schema = %s' + LineEnding + LineEnding +
     'var responseData = JSON.parse(responseBody);' + LineEnding +
     'var requestData = JSON.parse(request.data);' + LineEnding +
     '' + LineEnding +
     'tests["Status code is %d"] = responseCode.code === %d;' + LineEnding +
     'tests["Response data matches schema"] = tv4.validate(responseData, schema);' + LineEnding +
     'if (tv4.error) {' + LineEnding +
     '  console.log("Validation failed: ", tv4.error);' + LineEnding +
     '}' + LineEnding +
     'var keys = _.intersection(_.keys(requestData), _.keys(responseData));' + LineEnding +
     'var contentIsEqual = _.isEqual(_.pick(requestData, keys), _.pick(responseData, keys));' + LineEnding +
     'tests["Response content equals request"] = contentIsEqual;' + LineEnding +
     'if (!contentIsEqual) {' + LineEnding +
     '  var difference = _.reduce(keys, function (result, key) {' + LineEnding +
     '    if (requestData[key] !== responseData[key]) {' + LineEnding +
     '        result[key] = {request: requestData[key], response: responseData[key]}' + LineEnding +
     '    }' + LineEnding +
     '    return result;' + LineEnding +
     '  }, {});' + LineEnding +
     '  console.log(''requestData'', requestData);' + LineEnding +
     '  console.log(''responseData'', responseDataNoId);' + LineEnding +
     '  console.log(''difference'', difference);' + LineEnding +
     '}';


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

