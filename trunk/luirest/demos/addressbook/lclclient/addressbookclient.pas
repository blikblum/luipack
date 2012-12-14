unit AddressBookClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, httpsend;

const
  RES_CONTACT = 101;
  RES_CONTACTS = 102;
  RES_CONTACTPHONE = 103;
  RES_CONTACTPHONES = 104;

type
  THTTPMethodType = (hmtGet, hmtPost, hmtPut, hmtDelete);

  TRESTResponseEvent = procedure(ResourceId: Integer; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream) of object;

  { TAddressBookRESTClient }


  TAddressBookRESTClient = class(TComponent)
  private
    FBaseURL: String;
    FHttpClient: THTTPSend;
    FOnError: TRESTResponseEvent;
    FOnSuccess: TRESTResponseEvent;
    FQueryParams: String;
    FResourceDefs: TJSONObject;
    procedure DoResponseCallback(ResourceId: Integer; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream);
    function GetPath(ResourceId: PtrInt; const Args: array of const): String;
    procedure SetBaseURL(const AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(ResourceId: PtrInt; const Args: array of const): Boolean;
    function Get(ResourceId: PtrInt; const Args: array of const): Boolean;
    function Post(ResourceId: PtrInt; const Args: array of const; const DataStr: String): Boolean;
    function Put(ResourceId: PtrInt; const Args: array of const; const DataStr: String): Boolean;
    property BaseURL: String read FBaseURL write SetBaseURL;
    property QueryParams: String read FQueryParams write FQueryParams;
    property OnSuccess: TRESTResponseEvent read FOnSuccess write FOnSuccess;
    property OnError: TRESTResponseEvent read FOnError write FOnError;
  end;

implementation

{ TAddressBookRESTClient }

procedure TAddressBookRESTClient.DoResponseCallback(ResourceId: Integer; Method: THTTPMethodType;
   ResponseCode: Integer; ResponseStream: TStream);
begin
  if ResponseCode < 300 then
  begin
    if Assigned(FOnSuccess) then
      FOnSuccess(ResourceId, Method, ResponseCode, ResponseStream);
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(ResourceId, Method, ResponseCode, ResponseStream);
  end;
end;

function TAddressBookRESTClient.GetPath(ResourceId: PtrInt; const Args: array of const): String;
var
  ResourceIndex: Integer;
begin
  ResourceIndex := FResourceDefs.IndexOfName(IntToStr(ResourceId));
  if ResourceIndex = -1 then
    raise Exception.CreateFmt('Unable to find resource "%d"', [ResourceId]);
  Result := Format(FResourceDefs.Items[ResourceIndex].AsString, Args);
end;

procedure TAddressBookRESTClient.SetBaseURL(const AValue: String);
begin
  if AValue[Length(AValue)] <> '/' then
    FBaseURL := AValue + '/'
  else
    FBaseURL := AValue;
end;

constructor TAddressBookRESTClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpClient := THTTPSend.Create;
  FResourceDefs := TJSONObject.Create([
    IntToStr(RES_CONTACTS), 'contacts',
    IntToStr(RES_CONTACT), 'contacts/%s',
    IntToStr(RES_CONTACTPHONES), 'contacts/%s/phones',
    IntToStr(RES_CONTACTPHONE), 'contacts/%s/phones/%s'
    ]);
end;

destructor TAddressBookRESTClient.Destroy;
begin
  FHttpClient.Destroy;
  FResourceDefs.Destroy;
  inherited Destroy;
end;

function TAddressBookRESTClient.Delete(ResourceId: PtrInt; const Args: array of const): Boolean;
begin
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('DELETE', BaseURL + GetPath(ResourceId, Args));
  if Result then
    DoResponseCallback(ResourceId, hmtDelete, FHttpClient.ResultCode, FHttpClient.Document);
end;

function TAddressBookRESTClient.Get(ResourceId: PtrInt; const Args: array of const): Boolean;
begin
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('GET', BaseURL + GetPath(ResourceId, Args));
  if Result then
    DoResponseCallback(ResourceId, hmtGet, FHttpClient.ResultCode, FHttpClient.Document);
end;

function TAddressBookRESTClient.Post(ResourceId: PtrInt; const Args: array of const;
  const DataStr: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(DataStr[1], Length(DataStr));
  Result := FHttpClient.HTTPMethod('POST', BaseURL + GetPath(ResourceId, Args));
  if Result then
    DoResponseCallback(ResourceId, hmtPost, FHttpClient.ResultCode, FHttpClient.Document);
end;

function TAddressBookRESTClient.Put(ResourceId: PtrInt; const Args: array of const;
  const DataStr: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(DataStr[1], Length(DataStr));
  Result := FHttpClient.HTTPMethod('PUT', BaseURL + GetPath(ResourceId, Args));
  if Result then
    DoResponseCallback(ResourceId, hmtPut, FHttpClient.ResultCode, FHttpClient.Document);
end;

end.

