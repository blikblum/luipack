unit PresentationDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Forms;

type

  { TPresenterViewDescriptor }

  TPresenterViewDescriptor = class(TFileDescPascalUnitWithResource)
  private
    FPresenterClassName: String;
    FPresenterUnitName: String;
    FViewClassName: String;
  public
    constructor Create; override;
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
    property PresenterUnitName: String read FPresenterUnitName write FPresenterUnitName;
    property ViewClassName: String read FViewClassName write FViewClassName;
  end;

implementation

{ TPresenterViewDescriptor }

constructor TPresenterViewDescriptor.Create;
begin
  inherited Create;
  Name:='Presenter / View(TForm)';
  ResourceClass:=TForm;
  UseCreateFormStatements:=true;
  RequiredPackages:='LCL';
end;

function TPresenterViewDescriptor.CreateSource(const aFilename, aSourceName,
  aResourceName: string): string;
begin
  Result := inherited CreateSource(aFilename, aSourceName, aResourceName);
end;

function TPresenterViewDescriptor.GetLocalizedDescription: String;
begin
  Result := inherited GetLocalizedDescription;
end;

function TPresenterViewDescriptor.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
end;

function TPresenterViewDescriptor.GetInterfaceSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  Result := inherited GetInterfaceSource(aFilename, aSourceName, aResourceName);
end;

function TPresenterViewDescriptor.GetInterfaceUsesSection: String;
begin
  Result := inherited GetInterfaceUsesSection + ', Forms, Controls, Graphics, Dialogs';
end;

function TPresenterViewDescriptor.GetImplementationSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  Result := inherited GetImplementationSource(aFilename, aSourceName,
    aResourceName);
end;

function TPresenterViewDescriptor.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
begin
  Result := inherited Init(NewFilename, NewOwner, NewSource, Quiet);
end;

end.

