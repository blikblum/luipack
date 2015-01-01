unit PresentationDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Forms, Controls;

type

  { TPresenterViewDescriptor }

  TPresenterViewDescriptor = class(TFileDescPascalUnitWithResource)
  private
    FPresenterClassName: String;
    FPresenterProperties: TStrings;
    FPresenterUnitName: String;
    FViewClassName: String;
    function GetPresenterPublishedSection: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function GetLocalizedDescription: String; override;
    function GetLocalizedName: string; override;
    function GetInterfaceSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function GetInterfaceUsesSection: String; override;
    function GetImplementationSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
  published
    property PresenterClassName: String read FPresenterClassName write FPresenterClassName;
    property PresenterProperties: TStrings read FPresenterProperties;
    property PresenterUnitName: String read FPresenterUnitName write FPresenterUnitName;
    property ViewClassName: String read FViewClassName write FViewClassName;
  end;

implementation

uses
  PresentationOptionsView;

{ TPresenterViewDescriptor }

function TPresenterViewDescriptor.GetPresenterPublishedSection: String;
var
  i: Integer;
  PropName, PropType: String;
begin
  Result := '';
  if FPresenterProperties.Count = 0 then
    Exit;
  Result := '  published' + LineEnding;
  for i := 0 to FPresenterProperties.Count - 1 do
  begin
    PropName := Trim(FPresenterProperties.Names[i]);
    PropType := Trim(FPresenterProperties.ValueFromIndex[i]);
    if (PropName = '') or (PropType = '') then
      Result := Result + Format('    property %s: %s read F%0:s write F%0:s;', [PropName, PropType]) + LineEnding;
  end;
end;

constructor TPresenterViewDescriptor.Create;
begin
  inherited Create;
  Name:='Presenter / View(TForm)';
  ResourceClass:=TForm;
  RequiredPackages:='LCL';
  FPresenterProperties := TStringList.Create;
  DefaultSourceName := 'MyView';
  ViewClassName := 'TMyForm';
  PresenterUnitName := 'MyPresenter';
  PresenterClassName := 'TMyPresenter';
end;

destructor TPresenterViewDescriptor.Destroy;
begin
  FPresenterProperties.Destroy;
  inherited Destroy;
end;

const
  PresenterSrc = 'unit {{presenterunit}};' + LineEnding +
    ''  + LineEnding +
    'interface'+ LineEnding +
    ''+ LineEnding +
    'uses' + LineEnding +
    '  Classes, SysUtils, PresentationManager, fpjson;' + LineEnding +
    '' + LineEnding +
    'type' + LineEnding +
    ''+ LineEnding +
    '  {{presenterclass}} = class(TPresenter)' + LineEnding +
    '{{presenterpublished}}' + LineEnding +
    '  end;' + LineEnding +
    ''+ LineEnding +
    'implementation'+ LineEnding +
    ''+ LineEnding +
    'end.';

function TPresenterViewDescriptor.CreateSource(const aFilename, aSourceName,
  aResourceName: string): string;
begin
  Result := inherited CreateSource(aFilename, aSourceName, aResourceName);
  Result := StringReplace(Result, '{{presenterunit}}', FPresenterUnitName, [rfReplaceAll]);
  Result := StringReplace(Result, '{{presenterclass}}', FPresenterClassName, [rfReplaceAll]);
end;

function TPresenterViewDescriptor.GetLocalizedDescription: String;
begin
  Result := 'Boilerplate for Presenter / View (TForm) units';
end;

function TPresenterViewDescriptor.GetLocalizedName: string;
begin
  Result := 'Presenter / View (TForm)';
end;

function TPresenterViewDescriptor.GetInterfaceSource(const aFilename,
  aSourceName, aResourceName: string): string;
const
  LE = LineEnding;
begin
  Result:=
     'type'+LE
    +'  '+ViewClassName+' = class(TForm)'+LE
    +'  private'+LE
    +'    FPresenter: {{presenterclass}};'+ LE
    +'  public'+LE
    +'  published'+LE
    +'    property Presenter: {{presenterclass}} read FPresenter write FPresenter;' +LE
    +'  end;'+LE
    +LE;
end;

function TPresenterViewDescriptor.GetInterfaceUsesSection: String;
begin
  Result := inherited GetInterfaceUsesSection + ', Forms, Controls, Graphics, Dialogs, fpjson, {{presenterunit}}';
end;

function TPresenterViewDescriptor.GetImplementationSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  Result := inherited GetImplementationSource(aFilename, aSourceName,
    aResourceName);
end;

function TPresenterViewDescriptor.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
var
  S: String;
  PresenterFile: TLazProjectFile;
begin
  with TPresentationOptionsForm.Create(nil) do
  begin
    Descriptor := Self;
    Result := ShowModal;
    Destroy;
  end;
  if Result = mrOK then
  begin
    DefaultResourceName := Copy(ViewClassName, 2, Length(ViewClassName) - 1);
    PresenterFile := LazarusIDE.ActiveProject.CreateProjectFile(PresenterUnitName + '.pas');
    PresenterFile.IsPartOfProject := True;
    LazarusIDE.ActiveProject.AddFile(PresenterFile, False);
    S := StringReplace(PresenterSrc, '{{presenterunit}}', FPresenterUnitName, [rfReplaceAll]);
    S := StringReplace(S, '{{presenterclass}}', FPresenterClassName, [rfReplaceAll]);
    S := StringReplace(S, '{{presenterpublished}}', GetPresenterPublishedSection, []);
    PresenterFile.SetSourceText(S, True);
  end;
end;

end.

