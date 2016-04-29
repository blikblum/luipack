unit LuiObjectHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TObjectHelper }

  TObjectHelper = class helper for TObject
  public
    procedure ReleaseWith(Owner: TComponent);
  end;

implementation

type

  { TComponentWrapper }

  TComponentWrapper = class(TComponent)
  private
    FInstance: TObject;
  public
    constructor Create(Instance: TObject; AOwner: TComponent);
    destructor Destroy; override;
  end;

{ TComponentWrapper }

constructor TComponentWrapper.Create(Instance: TObject; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInstance := Instance;
end;

destructor TComponentWrapper.Destroy;
begin
  FInstance.Free;
  inherited Destroy;
end;

{ TObjectHelper }

procedure TObjectHelper.ReleaseWith(Owner: TComponent);
begin
  TComponentWrapper.Create(Self, Owner);
end;

end.

