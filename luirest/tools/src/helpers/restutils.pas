unit RESTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, httpsend;

function DoRequest(const Method, URL, Payload: String;
  out Response: String; out Code: Integer): Boolean;

function DoRequest(EndPointData: TJSONObject; const Payload: String;
  out Response: String; out Code: Integer): Boolean;

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

end.

