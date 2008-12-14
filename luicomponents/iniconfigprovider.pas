unit IniConfigProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LuiConfig;

type

  { TIniFileProvider }

  TIniFileProvider = class(TLuiConfigProvider)
  private
    FFileName: String;
    FIniFile: TMemIniFile;
    procedure SetFileName(const AValue: string);
  protected
  public
    destructor Destroy; override;
    procedure Close;
    procedure Open;
    function ReadString(const Section, Key, Default: String): String; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

implementation

{ TIniFileProvider }

procedure TIniFileProvider.SetFileName(const AValue: string);
begin
  if FFileName = AValue then exit;
  FFileName := AValue;
end;

destructor TIniFileProvider.Destroy;
begin
  FIniFile.Free;
  inherited Destroy;
end;

procedure TIniFileProvider.Close;
begin
  FIniFile.Clear;
end;

procedure TIniFileProvider.Open;
begin
  if FIniFile = nil then
    FIniFile := TMemIniFile.Create(FFileName)
  else
    FIniFile.Rename(FFileName, True);
end;

function TIniFileProvider.ReadString(const Section, Key, Default: String
  ): String;
begin
  Result := FIniFile.ReadString(Section, Key, Default);
end;


end.

