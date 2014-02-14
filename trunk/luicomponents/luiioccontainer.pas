unit LuiIoCContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TInstanceCreateEvent = procedure(out Instance: TObject);

  { TInterfaceDef }

  TInterfaceDef = class
  private
    FComponentClass: TComponentClass;
    FOnInstanceCreate: TInstanceCreateEvent;
    FSingletonReference: IInterface;
  public
    function GetInterfaceReference(const IID: TGuid): IInterface;
    property ComponentClass: TComponentClass read FComponentClass write FComponentClass;
    property OnInstanceCreate: TInstanceCreateEvent read FOnInstanceCreate write FOnInstanceCreate;
    property SingletonReference: IInterface read FSingletonReference;
  end;

  { TIoCContainer }

  TIoCContainer = class
  private
    FIntfList: TFPHashObjectList;
    function Find(const IID: TGuid): TInterfaceDef;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(const IID: TGuid; ComponentClass: TComponentClass);
    procedure Register(const IID: TGuid; CreateCallback: TInstanceCreateEvent);
    procedure Register(const IID: TGuid; Component: TComponent);
    function Resolve(const IID: TGuid; out Reference): Boolean;
    function Resolve(const IID: TGuid): IInterface;
  end;


implementation

const
  SRegisterError = 'RegisterInterface (%s): %s must be <> nil';
  SIntfNotRegistered = 'Interface "%s" not registered';

type
  { TComponentReference }

  TComponentReference = class(TInterfacedObject, IVCLComObject)
  private
    FComponent: TComponent;
  public
    constructor Create(Component: TComponent);
    procedure BeforeDestruction; override;
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure FreeOnRelease;
  end;

  { TComponentReference }

constructor TComponentReference.Create(Component: TComponent);
begin
  FComponent := Component;
end;

procedure TComponentReference.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FComponent.Free;
end;

function TComponentReference.GetTypeInfoCount(out Count: Integer): HResult;
  stdcall;
begin
  Result := E_NOTIMPL;
end;

function TComponentReference.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TComponentReference.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TComponentReference.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

procedure TComponentReference.FreeOnRelease;
begin

end;

{ TInterfaceDef }

function TInterfaceDef.GetInterfaceReference(const IID: TGuid): IInterface;
var
  Component: TComponent;
  ComObject: IVCLComObject;
  Obj: TObject;
begin
  if FSingletonReference <> nil then
    Result := FSingletonReference
  else
  begin
    Result := nil;
    if FComponentClass <> nil then
    begin
      Component := FComponentClass.Create(nil);
      ComObject := TComponentReference.Create(Component);
      Component.VCLComObject := ComObject;
      Component.GetInterface(IID, Result);
    end
    else if FOnInstanceCreate <> nil then
    begin
      Obj := nil;
      FOnInstanceCreate(Obj);
      if Obj <> nil then
      begin
        if not Obj.GetInterface(IID, Result) then
          Obj.Destroy;
      end
      else
        raise Exception.CreateFmt('Object does not implement "%s"', [GUIDToString(IID)]);
    end;
  end;
end;


{ TIoCContainer }

constructor TIoCContainer.Create;
begin
  FIntfList := TFPHashObjectList.Create(True);
end;

destructor TIoCContainer.Destroy;
begin
  FIntfList.Destroy;
  inherited Destroy;
end;

function TIoCContainer.Find(const IID: TGuid): TInterfaceDef;
begin
  Result := TInterfaceDef(FIntfList.Find(GUIDToString(IID)));
end;

procedure TIoCContainer.Register(const IID: TGuid;
  ComponentClass: TComponentClass);
var
  Def: TInterfaceDef;
  GuidStr: String;
begin
  GuidStr := GUIDToString(IID);
  if ComponentClass = nil then
    raise Exception.CreateFmt(SRegisterError, [GuidStr, 'ComponentClass']);
  Def := TInterfaceDef.Create;
  Def.ComponentClass := ComponentClass;
  FIntfList.Add(GuidStr, Def);
end;


procedure TIoCContainer.Register(const IID: TGuid;
  CreateCallback: TInstanceCreateEvent);
var
  Def: TInterfaceDef;
  GuidStr: String;
begin
  GuidStr := GUIDToString(IID);
  if CreateCallback = nil then
    raise Exception.CreateFmt(SRegisterError, [GuidStr, 'Event']);
  Def := TInterfaceDef.Create;
  Def.OnInstanceCreate := CreateCallback;
  FIntfList.Add(GuidStr, Def);
end;

procedure TIoCContainer.Register(const IID: TGuid; Component: TComponent);
var
  Def: TInterfaceDef;
  GuidStr: String;
begin
  GuidStr := GUIDToString(IID);
  if Component = nil then
    raise Exception.CreateFmt(SRegisterError, [GuidStr, 'Component']);
  Def := TInterfaceDef.Create;
  if not Component.GetInterface(IID, Def.FSingletonReference) then
    raise Exception.CreateFmt('Object instance does not implements "%s"', [GuidStr]);
  FIntfList.Add(GuidStr, Def);
end;

function TIoCContainer.Resolve(const IID: TGuid; out Reference): Boolean;
begin
  IInterface(Reference) := Resolve(IID);
  Result := IInterface(Reference) <> nil;
end;

function TIoCContainer.Resolve(const IID: TGuid): IInterface;
var
  Def: TInterfaceDef;
begin
  Def := Find(IID);
  if Def = nil then
    raise Exception.CreateFmt(SIntfNotRegistered, [GUIDToString(IID)]);
  Result := Def.GetInterfaceReference(IID);
  if Result = nil then
    raise Exception.CreateFmt('Unable to resolve interface "%s"', [GUIDToString(IID)]);
end;

end.

