unit register_luirest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, FormEditingIntf, LuiRESTServer;

type

  { TRESTServiceModuleDescriptor }

  TRESTServiceModuleDescriptor = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetLocalizedName : String; override;
    function GetLocalizedDescription : String; override;
    function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterProjectFileDescriptor(TRESTServiceModuleDescriptor.Create);
  FormEditingHook.RegisterDesignerBaseClass(TRESTServiceModule);
end;

{ TRESTServiceModuleDescriptor }

constructor TRESTServiceModuleDescriptor.Create;
begin
  inherited Create;
  Name := 'LuiREST Server';
  ResourceClass := TRESTServiceModule;
  UseCreateFormStatements := True;
end;

function TRESTServiceModuleDescriptor.GetLocalizedName: String;
begin
  Result := 'LuiREST Server Data Module';
end;

function TRESTServiceModuleDescriptor.GetLocalizedDescription: String;
begin
  Result := 'A data module to implement a REST server';
end;

function TRESTServiceModuleDescriptor.GetInterfaceUsesSection: String;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fpjson, fphttp, HTTPDefs, LuiREST';
end;

function TRESTServiceModuleDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  if GetResourceType = rtRes then
    Result := Result + 'initialization' + LineEnding;
  Result := Result + '  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');' + LineEnding;
end;

end.

