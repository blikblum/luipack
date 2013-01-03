unit register_luirest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, FormEditingIntf, LuiRESTServer, Forms, Controls,
  LuiRESTClient;

type

  { TRESTServiceModuleDescriptor }

  TRESTServiceModuleDescriptor = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetLocalizedName : String; override;
    function GetLocalizedDescription : String; override;
    function GetInterfaceUsesSection : String; override;
  end;

  { TRESTServiceApplicationDescriptor }

  TRESTServiceApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

procedure Register;

implementation

var
  RESTServiceModuleDescriptor: TRESTServiceModuleDescriptor;

procedure RegisterUnitLuiRESTClient;
begin
  RegisterComponents('Data Access', [TRESTClient, TRESTResourceClient]);
end;

procedure Register;
begin
  RESTServiceModuleDescriptor := TRESTServiceModuleDescriptor.Create;
  RegisterProjectFileDescriptor(RESTServiceModuleDescriptor);
  RegisterProjectDescriptor(TRESTServiceApplicationDescriptor.Create);
  FormEditingHook.RegisterDesignerBaseClass(TRESTServiceModule);
  RegisterUnitLuiRESTClient;
end;

{ TRESTServiceApplicationDescriptor }

constructor TRESTServiceApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'LuiREST Service Application';
end;

function TRESTServiceApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'LuiREST Service Application';
end;

function TRESTServiceApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  MainFile:=AProject.CreateProjectFile('restproject1.lpr');
  MainFile.IsPartOfProject:=True;
  AProject.AddFile(MainFile,False);
  AProject.MainFileID:=0;

  // create program source
  NewSource:='program restproject1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}{$H+}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  LuiRESTCGI, HTTPDefs, fphttp;'+LineEnding
    +LineEnding
    +'type'
    +LineEnding
    +'  TCallback = object'+LineEnding
    +'    procedure GetModuleClass(Sender : TObject; ARequest : TRequest;'+LineEnding
    +'      Var ModuleClass : TCustomHTTPModuleClass);'+LineEnding
    +'  end;'+LineEnding
    +LineEnding
    +'procedure TCallback.GetModuleClass(Sender: TObject; ARequest: TRequest;'+LineEnding
    +'  var ModuleClass: TCustomHTTPModuleClass);'+LineEnding
    +'begin'+LineEnding
    +'  ModuleClass := TRESTServiceModule1;'+LineEnding
    +'end;'+LineEnding
    +LineEnding
    +'var'+LineEnding
    +'  Callback: TCallback;'+LineEnding
    +'begin'+LineEnding
    +'  Application.Title:=''restproject1'';'+LineEnding
    +'  Application.OnGetModule := @Callback.GetModuleClass;'+LineEnding
    +'  Application.Initialize;'+LineEnding
    +'  Application.Run;'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('luirest_package');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='restproject1';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  AProject.Flags := AProject.Flags - [pfRunnable];
  Result:= mrOK;
end;

function TRESTServiceApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(RESTServiceModuleDescriptor,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:= mrOK;
end;

{ TRESTServiceModuleDescriptor }

constructor TRESTServiceModuleDescriptor.Create;
begin
  inherited Create;
  Name := 'LuiREST Service Module';
  ResourceClass := TRESTServiceModule;
  UseCreateFormStatements := True;
end;

function TRESTServiceModuleDescriptor.GetLocalizedName: String;
begin
  Result := 'LuiREST Service Module';
end;

function TRESTServiceModuleDescriptor.GetLocalizedDescription: String;
begin
  Result := 'A module to implement a REST Service';
end;

function TRESTServiceModuleDescriptor.GetInterfaceUsesSection: String;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fphttp, HTTPDefs, LuiRESTServer';
end;

end.

