unit DataHubUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RTTICtrls;

procedure SaveRTTIProperties(Owner: TComponent);

procedure SetRTTILinkObject(Owner: TComponent; AnObject: TPersistent);

implementation

uses
  typinfo;

procedure SaveRTTIProperties(Owner: TComponent);
var
  i: Integer;
  Comp: TComponent;
  Link: TObject;
  Info: PPropInfo;
begin
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    Comp := Owner.Components[i];
    Info := GetPropInfo(Comp, 'Link');
    if Info <> nil then
    begin
      Link := GetObjectProp(Comp, Info);
      if Link is TCustomPropertyLink then
        TCustomPropertyLink(Link).SaveToProperty;
    end;
  end;
end;

procedure SetRTTILinkObject(Owner: TComponent; AnObject: TPersistent);
var
  i: Integer;
  Comp: TComponent;
  Link: TObject;
  Info: PPropInfo;
begin
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    Comp := Owner.Components[i];
    Info := GetPropInfo(Comp, 'Link');
    if Info <> nil then
    begin
      Link := GetObjectProp(Comp, Info);
      if Link is TCustomPropertyLink then
        TCustomPropertyLink(Link).TIObject := AnObject;
    end;
  end;
end;

end.

