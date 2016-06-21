unit AppUpdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type

  { TAppUpdate }

  TAppUpdate = class(TComponent)
  private
    FFileName: String;
    FLocalDir: String;
    FRemoteFilePath: String;
    FVersion: String;
    procedure Clear;
    procedure DoLoad(const RootURL, NewVersion, FilePath: String);
    function GetLocalFilePath: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromData(const RootURL: String; Data: TJSONObject);
    procedure LoadFromURL(const InfoURL: String);
    function IsNewer(const CurrentVersion: String): Boolean;
    property LocalDir: String read FLocalDir write FLocalDir;
    property LocalFilePath: String read GetLocalFilePath;
    property RemoteFilePath: String read FRemoteFilePath;
    property Version: String read FVersion;
  end;

implementation

uses
  LuiStrUtils, strutils, LazFileUtils, httpsend, LuiJSONUtils;

{ TAppUpdate }

procedure TAppUpdate.Clear;
begin

end;

procedure TAppUpdate.DoLoad(const RootURL, NewVersion, FilePath: String);
var
  SlashPos: Integer;
begin
  FVersion := NewVersion;
  FRemoteFilePath := RootURL + FilePath;
  SlashPos := RPos('/', FilePath);
  if SlashPos > 0 then
    FFileName := Copy(FilePath, SlashPos + 1, Length(FilePath))
  else
    FFileName := FilePath;
end;

function TAppUpdate.GetLocalFilePath: String;
begin
  Result := AppendPathDelim(LocalDir) + FFileName;
end;

constructor TAppUpdate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocalDir := GetTempDir(False);
end;

procedure TAppUpdate.LoadFromData(const RootURL: String; Data: TJSONObject);
begin
  DoLoad(RootURL, Data.Get('version', ''), Data.Get('filepath', ''));
end;

procedure TAppUpdate.LoadFromURL(const InfoURL: String);
var
  HTTP: THTTPSend;
  UpdateInfoData: TJSONObject;
  InfoLoaded: Boolean;
begin
  FVersion := '';
  HTTP := THTTPSend.Create;
  try
    InfoLoaded := HTTP.HTTPMethod('GET', InfoURL);
    if InfoLoaded then
    begin
      InfoLoaded := TryStreamToJSON(HTTP.Document, UpdateInfoData);
      if InfoLoaded then
      begin
        LoadFromData(ExtractURLAddress(InfoURL), UpdateInfoData);
        UpdateInfoData.Destroy;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function TAppUpdate.IsNewer(const CurrentVersion: String): Boolean;
begin
  Result := CompareNatural(FVersion, CurrentVersion) = 1;
end;


end.

