unit PresentationManager;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, contnrs;

type

  //workaround to avoid compilation error with fpc 2.6*
  TFormClass = class of TForm;

  TPresenter = class(TComponent)
  public
    function ShowModal: TModalResult; virtual; abstract;
  end;

  TPresenterClass = class of TPresenter;

  IPresentation = interface
    ['{5FFDE1D3-AA0B-43C9-8ED3-437E33806B15}']
    function SetProperties(const Properties: array of const): IPresentation;
    function ShowModal: IPresentation;
    function ShowModal(const Properties: array of const): TModalResult;
    function ModalResult: TModalResult;
  end;

  IPresentationManager = interface
    ['{25532926-50FF-42F3-800D-AB4A7B01B3BA}']
    function GetPresentation(const PresentationName: String): IPresentation;
    procedure Register(const PresentationName: String; PresenterClass: TPresenterClass);
    procedure Register(const PresentationName: String; ViewClass: TFormClass);
    property Items[const PresentationName: String]: IPresentation read GetPresentation; default;
  end;

  { TPresentationManager }

  TPresentationManager = class(TComponent, IPresentationManager)
  private
    FPresentationDefs: TFPHashObjectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPresentation(const PresentationName: String): IPresentation;
    procedure Register(const PresentationName: String; PresenterClass: TPresenterClass);
    procedure Register(const PresentationName: String; ViewClass: TFormClass);
    property Items[const PresentationName: String]: IPresentation read GetPresentation; default;
  end;

implementation

uses
  VarRecUtils, LuiRTTIUtils;

type
  TPresentationDef = class
  private
    FPresenterClass: TPresenterClass;
    FViewClass: TFormClass;
  end;

  { TCustomPresentation }

  TCustomPresentation = class(TInterfacedObject)
  private
    FModalResult: TModalResult;
  public
    function ModalResult: TModalResult;
  end;

  { TFormPresentation }

  TFormPresentation = class(TCustomPresentation, IPresentation)
  private
    FViewClass: TFormClass;
    FProperties: TConstArray;
  public
    constructor Create(ViewClass: TFormClass);
    destructor Destroy; override;
    function SetProperties(const Properties: array of const): IPresentation;
    function ShowModal(const Properties: array of const): TModalResult;
    function ShowModal: IPresentation;
  end;

  { TPresenterPresentation }

  TPresenterPresentation = class(TCustomPresentation, IPresentation)
  private
    FPresenter: TPresenter;
  public
    constructor Create(PresenterClass: TPresenterClass);
    destructor Destroy; override;
    function SetProperties(const Properties: array of const): IPresentation;
    function ShowModal(const Properties: array of const): TModalResult;
    function ShowModal: IPresentation;
  end;

{ TPresenterPresentation }

constructor TPresenterPresentation.Create(PresenterClass: TPresenterClass);
begin
  FPresenter := PresenterClass.Create(nil);
end;

destructor TPresenterPresentation.Destroy;
begin
  FPresenter.Free;
  inherited Destroy;
end;

function TPresenterPresentation.SetProperties(
  const Properties: array of const): IPresentation;
begin
  SetObjectProperties(FPresenter, Properties);
  Result := Self;
end;

function TPresenterPresentation.ShowModal(
  const Properties: array of const): TModalResult;
begin
  Result := SetProperties(Properties).ShowModal.ModalResult;
end;

function TPresenterPresentation.ShowModal: IPresentation;
begin
  FModalResult := FPresenter.ShowModal;
  Result := Self;
end;

constructor TFormPresentation.Create(ViewClass: TFormClass);
begin
  FViewClass := ViewClass;
end;

destructor TFormPresentation.Destroy;
begin
  FinalizeConstArray(FProperties);
  inherited Destroy;
end;

function TFormPresentation.SetProperties(
  const Properties: array of const): IPresentation;
begin
  FinalizeConstArray(FProperties);
  FProperties := CreateConstArray(Properties);
  Result := Self;
end;

function TFormPresentation.ShowModal(
  const Properties: array of const): TModalResult;
begin
  Result := SetProperties(Properties).ShowModal.ModalResult;
end;

function TFormPresentation.ShowModal: IPresentation;
var
  Form: TForm;
begin
  Form := FViewClass.Create(nil);
  try
    SetObjectProperties(Form, FProperties);
    FModalResult := Form.ShowModal;
  finally
    Form.Destroy;
  end;
  Result := Self;
end;

{ TFormPresentation }

function TCustomPresentation.ModalResult: TModalResult;
begin
  Result := FModalResult;
end;

{ TPresentationManager }

constructor TPresentationManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPresentationDefs := TFPHashObjectList.Create(True);
end;

destructor TPresentationManager.Destroy;
begin
  FPresentationDefs.Destroy;
  inherited Destroy;
end;

function TPresentationManager.GetPresentation(const PresentationName: String): IPresentation;
var
  PresentationDef: TPresentationDef;
begin
  PresentationDef := TPresentationDef(FPresentationDefs.Find(PresentationName));
  if PresentationDef = nil then
    raise Exception.CreateFmt('Presentation "%s" not found', [PresentationName]);
  if PresentationDef.FViewClass <> nil then
    Result := TFormPresentation.Create(PresentationDef.FViewClass)
  else
    Result := TPresenterPresentation.Create(PresentationDef.FPresenterClass);
end;

procedure TPresentationManager.Register(const PresentationName: String;
  PresenterClass: TPresenterClass);
var
  PresentationDef: TPresentationDef;
begin
  if PresenterClass = nil then
    raise Exception.CreateFmt('Unable to register "%s" presentation: PresenterClass must be <> nil', [PresentationName]);
  if FPresentationDefs.FindIndexOf(PresentationName) <> -1 then
    raise Exception.CreateFmt('Presentation "%s" already registered', [PresentationName]);
  PresentationDef := TPresentationDef.Create;
  PresentationDef.FPresenterClass := PresenterClass;
  FPresentationDefs.Add(PresentationName, PresentationDef);
end;

procedure TPresentationManager.Register(const PresentationName: String;
  ViewClass: TFormClass);
var
  PresentationDef: TPresentationDef;
begin
  if ViewClass = nil then
    raise Exception.CreateFmt('Unable to register "%s" presentation: ViewClass must be <> nil', [PresentationName]);
  if FPresentationDefs.FindIndexOf(PresentationName) <> -1 then
    raise Exception.CreateFmt('Presentation "%s" already registered', [PresentationName]);
  PresentationDef := TPresentationDef.Create;
  PresentationDef.FViewClass := ViewClass;
  FPresentationDefs.Add(PresentationName, PresentationDef);
end;

end.

