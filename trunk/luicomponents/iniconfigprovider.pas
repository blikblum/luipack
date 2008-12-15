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
    procedure SetFileName(const AValue: String);
  protected
    procedure Close; override;
    procedure Open; override;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    function ReadString(const SectionTitle, ItemKey: String; out ValueExists: Boolean): String; override;
    procedure WriteString(const SectionTitle, ItemKey: String; AValue: String); override;
  public
    destructor Destroy; override;
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
var
  ParsedFileName: String;
begin
  ParsedFileName := ReplacePathMacros(FFileName);
  DoDirSeparators(ParsedFileName);
  if FIniFile = nil then
    FIniFile := TMemIniFile.Create(ParsedFileName)
  else
    FIniFile.Rename(ParsedFileName, True);
end;

procedure TIniFileProvider.ReadSection(const SectionTitle: String;
  Strings: TStrings);
begin
  FIniFile.ReadSection(SectionTitle, Strings);
end;

procedure TIniFileProvider.ReadSections(Strings: TStrings);
begin
  FIniFile.ReadSections(Strings);
end;

function TIniFileProvider.ReadString(const SectionTitle, ItemKey: String;
  out ValueExists: Boolean): String;
begin
  //the natural way of retrieving ValueExists is to call FIniFile.ValueExists,
  //but this would have a performance impact since the section/key would be
  //searched twice. Here we compare the Result with the passed default value.
  //Is necessary to use a space to guarantees that the value does not exists since
  //the item value is trimmed when read from file or when write. See WriteString.
  Result := FIniFile.ReadString(SectionTitle, ItemKey, ' ');
  ValueExists := Result <> ' ';
end;

procedure TIniFileProvider.WriteString(const SectionTitle, ItemKey: String;
  AValue: String);
begin
  //It's necessary to trim the value to avoid storing an empty space
  FIniFile.WriteString(SectionTitle, ItemKey, TrimRight(AValue));
end;


end.

