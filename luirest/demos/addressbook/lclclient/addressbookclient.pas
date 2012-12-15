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

  TRESTResponseEvent = procedure(ResourceTag: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream) of object;

  TSocketError = procedure(Sender: TObject; ErrorCode: Integer; const ErrorDescription: String) of object;

  { TAddressBookRESTClient }


  TAddressBookRESTClient = class(TComponent)
  private
    FBaseURL: String;
    FHttpClient: THTTPSend;
    FOnResponseError: TRESTResponseEvent;
    FOnResponseSuccess: TRESTResponseEvent;
    FOnSocketError: TSocketError;
    procedure DoResponseCallback(ResourcePath: PtrInt; Method: THTTPMethodType; ResponseCode: Integer; ResponseStream: TStream);
    procedure SetBaseURL(const AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
    function Get(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
    function Post(const ResourcePath: String; ResourceTag: PtrInt; const Data: String): Boolean;
    function Put(const ResourcePath: String; ResourceTag: PtrInt; const Data: String): Boolean;
    property BaseURL: String read FBaseURL write SetBaseURL;
    property OnResponseSuccess: TRESTResponseEvent read FOnResponseSuccess write FOnResponseSuccess;
    property OnResponseError: TRESTResponseEvent read FOnResponseError write FOnResponseError;
    property OnSocketError: TSocketError read FOnSocketError write FOnSocketError;
  end;

implementation

{ TAddressBookRESTClient }

procedure TAddressBookRESTClient.DoResponseCallback(ResourcePath: PtrInt; Method: THTTPMethodType;
   ResponseCode: Integer; ResponseStream: TStream);
begin
  if ResponseCode < 300 then
  begin
    if Assigned(FOnResponseSuccess) then
      FOnResponseSuccess(ResourcePath, Method, ResponseCode, ResponseStream);
  end
  else
  begin
    if Assigned(FOnResponseError) then
      FOnResponseError(ResourcePath, Method, ResponseCode, ResponseStream);
  end;
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
end;

destructor TAddressBookRESTClient.Destroy;
begin
  FHttpClient.Destroy;
  inherited Destroy;
end;

function TAddressBookRESTClient.Delete(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
begin
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('DELETE', BaseURL + ResourcePath);
  if Result then
  begin
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtDelete, FHttpClient.ResultCode, FHttpClient.Document);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TAddressBookRESTClient.Get(const ResourcePath: String; ResourceTag: PtrInt): Boolean;
begin
  FHttpClient.Clear;
  Result := FHttpClient.HTTPMethod('GET', BaseURL + ResourcePath);
  if Result then
  begin
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtGet, FHttpClient.ResultCode, FHttpClient.Document);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TAddressBookRESTClient.Post(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(Data[1], Length(Data));
  Result := FHttpClient.HTTPMethod('POST', BaseURL + ResourcePath);
  if Result then
  begin
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtPost, FHttpClient.ResultCode, FHttpClient.Document);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

function TAddressBookRESTClient.Put(const ResourcePath: String; ResourceTag: PtrInt;
  const Data: String): Boolean;
begin
  FHttpClient.Clear;
  FHttpClient.Document.Write(Data[1], Length(Data));
  Result := FHttpClient.HTTPMethod('PUT', BaseURL + ResourcePath);
  if Result then
  begin
    Result := FHttpClient.ResultCode < 300;
    DoResponseCallback(ResourceTag, hmtPut, FHttpClient.ResultCode, FHttpClient.Document);
  end
  else
  begin
    if Assigned(FOnSocketError) then
      FOnSocketError(Self, FHttpClient.Sock.LastError, FHttpClient.Sock.LastErrorDesc);
  end;
end;

end.

