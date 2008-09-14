unit VirtualDataObjects;

{Loosely based in tiOPF classes}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TVirtualDataObject = class;
  
  TObjectState = (osEmpty, osUpdate, osCreate, osClean, osPK);
  
  TNotificationType = (ntUpdate, ntLoad, ntChildAddition, ntChildDeletion, ntChildUpdate, ntCreate);
  
  IObjectTracker = interface(IUnknown)
    procedure ObjectNotification(AnObject: TVirtualDataObject;
      NotificationType: TNotificationType; Data: PtrInt);
  end;
  

  { TVirtualDataObject }

  TVirtualDataObject = class (TPersistent)
  private
    FId: Integer;
    FOwner: TVirtualDataObject;
    FState: TObjectState;
    FUpdateCount: Integer;
    FTrackerList: TFpList;
    procedure SetId(const AValue: Integer);
    procedure SetOwner(const AValue: TVirtualDataObject);
    procedure TrackerListNeeded;
  protected
    procedure DoUpdate; virtual;
    procedure ChildNotification(Child: TVirtualDataObject; NotificationType: TNotificationType = ntUpdate;
      Data: PtrInt = 0); virtual;
    procedure LoadData; virtual;
    procedure PropertyChanged; virtual;
  public
    constructor Create; virtual; overload;
    constructor Create(AState: TObjectState); virtual; overload;
    constructor CreateNew; virtual; overload;
    destructor Destroy; override;
    procedure AttachTracker(Tracker: IObjectTracker);
    procedure BeginUpdate;
    procedure DataNeeded;
    procedure Delete; virtual;
    procedure DetachTracker(Tracker: IObjectTracker);
    procedure EndUpdate(IgnoreUpdates: Boolean = False);
    function IdAsString: String; inline;
    procedure NotifyTrackers(NotificationType: TNotificationType = ntUpdate;
      Data: PtrInt = 0);
    procedure Load; virtual;
    procedure Save; virtual;
    property Id: Integer read FId write SetId;
    property Owner: TVirtualDataObject read FOwner write SetOwner;
    property State: TObjectState read FState;
  end;


  { TVirtualDataObjectList }

  TVirtualDataObjectList = class (TVirtualDataObject)
  private
    FObjectList: TFpList;
  protected
    function GetItems(Index: Integer): TVirtualDataObject;
    procedure ChildNotification(Child: TVirtualDataObject;
                     NotificationType: TNotificationType=ntUpdate; Data: PtrInt=
                     0); override;
  public
    constructor Create(AState: TObjectState); override;
    destructor Destroy; override;
    procedure Add(AnObject: TVirtualDataObject); virtual;
    procedure Clear;
    procedure Delete(AnObject: TVirtualDataObject); overload; virtual;
    function Count: Integer;
    function IndexOf(AnObject: TVirtualDataObject): Integer;
    property Items[Index: Integer]: TVirtualDataObject read GetItems;
  end;

implementation

{ TVirtualDataObjectList }

function TVirtualDataObjectList.GetItems(Index: Integer): TVirtualDataObject;
begin
  Result := TVirtualDataObject(FObjectList[Index]);
end;

procedure TVirtualDataObjectList.ChildNotification(Child: TVirtualDataObject;
  NotificationType: TNotificationType; Data: PtrInt);
begin
  NotifyTrackers(ntChildUpdate, PtrInt(Child));
end;

constructor TVirtualDataObjectList.Create(AState: TObjectState);
begin
  inherited Create(AState);
  FObjectList := TFPList.Create;
end;

destructor TVirtualDataObjectList.Destroy;
begin
  Clear;
  FObjectList.Destroy;
  inherited Destroy;
end;

procedure TVirtualDataObjectList.Add(AnObject: TVirtualDataObject);
begin
  AnObject.Owner := Self;
  FObjectList.Add(AnObject);
end;

procedure TVirtualDataObjectList.Clear;
var
  i: Integer;
begin
  for i := 0 to FObjectList.Count - 1 do
    TVirtualDataObject(FObjectList[i]).Destroy;
  FObjectList.Clear;
end;

procedure TVirtualDataObjectList.Delete(AnObject: TVirtualDataObject);
var
  i: Integer;
begin
  i := FObjectList.IndexOf(AnObject);
  if i <> -1 then
    FObjectList.Delete(i);
end;

function TVirtualDataObjectList.Count: Integer;
begin
  Result := FObjectList.Count;
end;

function TVirtualDataObjectList.IndexOf(AnObject: TVirtualDataObject): Integer;
begin
  Result := FObjectList.IndexOf(AnObject);
end;

{ TVirtualDataObject }

procedure TVirtualDataObject.SetId(const AValue: Integer);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TVirtualDataObject.SetOwner(const AValue: TVirtualDataObject);
begin
  if FOwner=AValue then exit;
  FOwner:=AValue;
end;

procedure TVirtualDataObject.TrackerListNeeded;
begin
  if FTrackerList = nil then
    FTrackerList := TFPList.Create;
end;

procedure TVirtualDataObject.DoUpdate;
begin
  if FOwner <> nil then
    FOwner.ChildNotification(Self);
  NotifyTrackers;
end;

procedure TVirtualDataObject.ChildNotification(Child: TVirtualDataObject;
  NotificationType: TNotificationType = ntUpdate; Data: PtrInt = 0);
begin

end;

procedure TVirtualDataObject.Load;
begin

end;

procedure TVirtualDataObject.LoadData;
begin

end;

procedure TVirtualDataObject.Save;
begin
  FState := osClean;
end;

procedure TVirtualDataObject.PropertyChanged;
begin
  if FState <> osCreate then
    FState := osUpdate;
end;

constructor TVirtualDataObject.Create;
begin
  Create(osEmpty);
end;

constructor TVirtualDataObject.Create(AState: TObjectState);
begin
  FState := AState;
end;

constructor TVirtualDataObject.CreateNew;
begin
  Create(osCreate);
end;

destructor TVirtualDataObject.Destroy;
begin
  FTrackerList.Free;
end;

procedure TVirtualDataObject.AttachTracker(Tracker: IObjectTracker);
begin
  TrackerListNeeded;
  FTrackerList.Add(Tracker);
end;

procedure TVirtualDataObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TVirtualDataObject.DataNeeded;
begin
  if FState = osEmpty then
    Load;
  if FState = osPK then
    LoadData;
end;

procedure TVirtualDataObject.Delete;
begin

end;

procedure TVirtualDataObject.DetachTracker(Tracker: IObjectTracker);
begin
  if FTrackerList = nil then
    Exit;
  FTrackerList.Extract(Tracker);
end;

procedure TVirtualDataObject.EndUpdate(IgnoreUpdates: Boolean = False);
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if (FUpdateCount = 0) and not IgnoreUpdates then
    DoUpdate;
end;

function TVirtualDataObject.IdAsString: String;
begin
  Result := IntToStr(FId);
end;

procedure TVirtualDataObject.NotifyTrackers(NotificationType: TNotificationType = ntUpdate;
  Data: PtrInt = 0);
var
  i: Integer;
begin
  if FTrackerList = nil then
    Exit;
  for i := 0 to FTrackerList.Count - 1 do
    IObjectTracker(FTrackerList[i]).ObjectNotification(Self, NotificationType, Data);
end;

end.

