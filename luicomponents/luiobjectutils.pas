unit LuiObjectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, contnrs;

type

  {
  //todo: implement as generic when fpc is fixed
  TCreateObjectEvent<T> = procedure(var Instance: T) of object;

  TObjectPool<T> = class
  public
    function Acquire: T;
    procedure Release(Instance: T);
  end;
  }

  TVarRecArray = array of TVarRec;

  TCreateObjectEvent = procedure(var Instance: TObject; const ProfileId: String;
    const Params: TVarRecArray) of object;

  TObjectToStringMap = specialize TFPGMap<PtrInt, String>;

  TStringToObjectMap = specialize TFPGMap<String, PtrInt>;

  { TCustomObjectPool }

  TCustomObjectPool = class
  private
    FActiveList: TObjectToStringMap;
    FAvailableList: TStringToObjectMap;
    FBaseClass: TClass;
    FProfileList: TFPHashObjectList;
  protected
    procedure DoCreateObject(var Instance: TObject; const ProfileId: String;
      const Params: TVarRecArray); virtual; abstract;
    property BaseClass: TClass read FBaseClass write FBaseClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Acquire(const ProfileId: string = ''): TObject;
    procedure RegisterProfile(const ProfileId: String; const Params: TVarRecArray);
    procedure Release(Instance: TObject);
  end;

  { TObjectPool }

  TObjectPool = class(TCustomObjectPool)
  private
    FOnCreateObject: TCreateObjectEvent;
  protected
    procedure DoCreateObject(var Instance: TObject; const ProfileId: String;
      const Params: TVarRecArray); override;
  public
    property BaseClass;
    property OnCreateObject: TCreateObjectEvent read FOnCreateObject write FOnCreateObject;
  end;


implementation

{ TObjectPool<T> }

{

function TObjectPool<T>.Acquire: T;
begin

end;

procedure TObjectPool<T>.Release(Instance: T);
begin

end;

}

type

  { TObjectProfile }

  TObjectProfile = class
  private
    FParams: TVarRecArray;
  public
    constructor Create(AParams: TVarRecArray);
    property Params: TVarRecArray read FParams;
  end;

procedure TObjectPool.DoCreateObject(var Instance: TObject; const ProfileId: String;
  const Params: TVarRecArray);
begin
  if Assigned(FOnCreateObject) then
    FOnCreateObject(Instance, ProfileId, Params);
end;

{ TObjectProfile }

constructor TObjectProfile.Create(AParams: TVarRecArray);
begin
  FParams := AParams;
end;

{ TCustomObjectPool }

constructor TCustomObjectPool.Create;
begin
  FAvailableList := TStringToObjectMap.Create;
  FActiveList := TObjectToStringMap.Create;
  FBaseClass := TObject;
end;

destructor TCustomObjectPool.Destroy;
var
  i: Integer;
  Obj: TObject;
begin
  for i := 0 to FAvailableList.Count - 1 do
  begin
    Obj := TObject(FAvailableList.Data[i]);
    Obj.Free;
  end;
  FAvailableList.Destroy;
  //todo warn about free of active object
  for i := 0 to FActiveList.Count - 1 do
  begin
    Obj := TObject(FActiveList.Keys[i]);
    Obj.Free;
  end;
  FActiveList.Destroy;
  FProfileList.Free;
  inherited Destroy;
end;

function TCustomObjectPool.Acquire(const ProfileId: string = ''): TObject;
var
  i: Integer;
  Profile: TObjectProfile;
  Params: TVarRecArray;
begin
  i := FAvailableList.IndexOf(ProfileId);
  if i > -1 then
  begin
    // return an object from pool
    Result := TObject(FAvailableList.Data[i]);
    FAvailableList.Delete(i);
    FActiveList.Add(PtrInt(Result), ProfileId);
  end
  else
  begin
    // create an new object
    Result := nil;
    Params := nil;
    if FProfileList <> nil then
    begin
      Profile := TObjectProfile(FProfileList.Find(ProfileId));
      if Profile <> nil then
        Params := Profile.Params;
      //todo add option to exceptionif not found
    end;
    DoCreateObject(Result, ProfileId, Params);
    if Result = nil then
      raise Exception.Create('ObjectPool.Acquire: Object not created');
    if not Result.InheritsFrom(BaseClass) then
      raise Exception.Create('ObjectPool.Acquire: Instance does not inherits from BaseClass');
    FActiveList.Add(PtrInt(Result), ProfileId);
  end;
end;

procedure TCustomObjectPool.RegisterProfile(const ProfileId: String;
  const Params: TVarRecArray);
begin
  if FProfileList = nil then
    FProfileList := TFPHashObjectList.Create(True);
  FProfileList.Add(ProfileId, TObjectProfile.Create(Params));
end;

procedure TCustomObjectPool.Release(Instance: TObject);
var
  i: Integer;
  ProfileId: String;
begin
  i := FActiveList.IndexOf(PtrInt(Instance));
  if i > -1 then
  begin
    ProfileId := FActiveList.Data[i];
    FActiveList.Delete(i);
    FAvailableList.Add(ProfileId, PtrInt(Instance));
  end
  else
    raise Exception.Create('ObjectPool.Release: Instance not active');
end;

end.

