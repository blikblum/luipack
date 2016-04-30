unit IocContainerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, LuiIoCContainer;

type

  { TIoCContainerTestCase }

  TIoCContainerTestCase = class(TTestCase)
  private
    FContainer: TIoCContainer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ResolveComponentInstance;
    procedure ResolveComponentClass;
    procedure ResolveCallback;
    procedure ResolveUnregistered;
    procedure ResolveProperties;
  end;


implementation

type

  IComponentIntf = interface
    ['{77B3B43C-543B-4929-8ACC-5B68F61B2AE6}']
    procedure Do1;
  end;

  IComponentClassIntf = interface
    ['{FCE5630F-A323-4888-A9E7-380CA3CD07DE}']
    function Do2: String;
  end;

  ICallbackIntf = interface
    ['{F3F97A94-EC5E-40FA-A9C7-042E56F437C7}']
    procedure Do3;
  end;

  { TComponentIntf }

  TComponentIntf = class(TComponent, IComponentIntf)
  public
    procedure Do1;
  end;

  { TComponentClassIntf }

  TComponentClassIntf = class(TComponent, IComponentClassIntf)
    function Do2: String;
  end;

  { TCallbackClass }

  TCallbackClass = class(TInterfacedObject, ICallbackIntf)
    procedure Do3;
  end;

{ TCallbackClass }

procedure TCallbackClass.Do3;
begin
  //
end;

{ TComponentClassIntf }

function TComponentClassIntf.Do2: String;
begin
  Result := 'do2';
end;

{ TComponentIntf }

procedure TComponentIntf.Do1;
begin
  //
end;

{ TIoCContainerTestCase }

procedure TIoCContainerTestCase.SetUp;
begin
  inherited SetUp;
  FContainer := TIoCContainer.Create;
end;

procedure TIoCContainerTestCase.TearDown;
begin
  FContainer.Free;
  inherited TearDown;
end;

procedure TIoCContainerTestCase.ResolveComponentInstance;
var
  Comp: TComponentIntf;
  Obj: TObject;
  Intf: IComponentIntf;
begin
  Comp := TComponentIntf.Create(nil);
  FContainer.Register(IComponentIntf, Comp);

  FContainer.Resolve(IComponentIntf, Intf);
  CheckNotNull(Intf);
  CheckTrue(Supports(Intf, IComponentIntf));
  CheckTrue(Supports(Intf, TComponentIntf, Obj));
  CheckSame(Comp, Obj);

  CheckTrue(FContainer.TryResolve(IComponentIntf, Intf));
  CheckNotNull(Intf);
  CheckTrue(Supports(Intf, IComponentIntf));
  CheckTrue(Supports(Intf, TComponentIntf, Obj));
  CheckSame(Comp, Obj);

  Comp.Destroy;
end;

procedure TIoCContainerTestCase.ResolveComponentClass;
var
  Intf: IComponentClassIntf;
begin
  FContainer.Register(IComponentClassIntf, TComponentClassIntf);

  FContainer.Resolve(IComponentClassIntf, Intf);
  CheckNotNull(Intf);
  CheckEquals('do2', Intf.Do2, 'Should call correct function');
  CheckTrue(Supports(Intf, IComponentClassIntf), 'Should support IComponentClassIntf');
  CheckTrue(Supports(Intf, TComponentClassIntf), 'Should descend from TComponentClassIntf');

  CheckTrue(FContainer.TryResolve(IComponentClassIntf, Intf), 'Should resolve');
  CheckNotNull(Intf);
  CheckTrue(Supports(Intf, IComponentClassIntf), 'Should support IComponentClassIntf');
  CheckTrue(Supports(Intf, TComponentClassIntf), 'Should descend from TComponentClassIntf');
end;

procedure GetInstance(out Instance: TObject);
begin
  Instance := TCallbackClass.Create;
end;

procedure TIoCContainerTestCase.ResolveCallback;
var
  Intf: ICallbackIntf;
begin
  FContainer.Register(ICallbackIntf, @GetInstance);

  FContainer.Resolve(ICallbackIntf, Intf);
  CheckNotNull(Intf);
  CheckTrue(Supports(Intf, ICallbackIntf));
  CheckTrue(Supports(Intf, TCallbackClass));

  CheckTrue(FContainer.TryResolve(ICallbackIntf, Intf));
  CheckNotNull(Intf);
  CheckTrue(Supports(Intf, ICallbackIntf));
  CheckTrue(Supports(Intf, TCallbackClass));
end;

procedure TIoCContainerTestCase.ResolveUnregistered;
var
  Intf: IComponentIntf;
  Raised: Boolean;
begin
  CheckFalse(FContainer.TryResolve(IComponentIntf, Intf));

  Raised := False;
  try
    FContainer.Resolve(IComponentIntf);
  except
    Raised := True;
  end;
  CheckTrue(Raised);

  Raised := False;
  try
    FContainer.Resolve(IComponentIntf, Intf);
  except
    Raised := True;
  end;
  CheckTrue(Raised);
end;

type

  { TWithCompIntfClass }

  TWithCompIntfClass = class
  private
    FCompClassIntf: IComponentIntf;
    FCompIntf: IComponentIntf;
  published
    property CompIntf: IComponentIntf read FCompIntf write FCompIntf;
    property CompClassIntf: IComponentIntf read FCompClassIntf write FCompClassIntf;
  end;

  { TWithCallbackIntfClass }

  TWithCallbackIntfClass = class
  private
    FCallbackIntf: ICallbackIntf;
  published
    property CallbackIntf: ICallbackIntf read FCallbackIntf write FCallbackIntf;
  end;

procedure TIoCContainerTestCase.ResolveProperties;
var
  Comp: TComponentIntf;
  WithCompIntf: TWithCompIntfClass;
  WithCallbackIntf: TWithCallbackIntfClass;
  Raised: Boolean;
begin
  Comp := TComponentIntf.Create(nil);
  FContainer.Register(IComponentIntf, Comp);
  FContainer.Register(IComponentClassIntf, TComponentClassIntf);

  WithCompIntf := TWithCompIntfClass.Create;
  LuiIoCContainer.ResolveProperties(WithCompIntf, FContainer);
  CheckNotNull(WithCompIntf.CompIntf);
  CheckNotNull(WithCompIntf.CompClassIntf);
  WithCompIntf.Destroy;

  WithCallbackIntf := TWithCallbackIntfClass.Create;
  Raised := False;
  try
    LuiIoCContainer.ResolveProperties(WithCallbackIntf, FContainer);
  except
    Raised := True;
  end;
  CheckTrue(Raised);
  CheckNull(WithCallbackIntf.CallbackIntf);
  WithCallbackIntf.Destroy;

  Comp.Destroy;
end;

initialization
  ProjectRegisterTests('IocContainer', [TIoCContainerTestCase.Suite]);

end.

