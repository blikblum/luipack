unit LuiRTTIUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SetObjectProperties(Instance: TObject; Properties: array of const);

implementation

uses
  typinfo;

function VarRecToString(const VarRec: TVarRec): String;
begin
  case VarRec.VType of
    vtAnsiString: Result := AnsiString(VarRec.VAnsiString);
    vtPChar: Result := String(VarRec.VPChar);
    vtString: Result := VarRec.VString^;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to String');
  end;
end;

function VarRecToInteger(const VarRec: TVarRec): Integer;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Integer');
  end;
end;

function VarRecToInt64(const VarRec: TVarRec): Int64;
begin
  case VarRec.VType of
    vtInteger: Result := VarRec.VInteger;
    vtInt64: Result := (VarRec.VInt64)^;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Int64');
  end;
end;

function VarRecToObject(const VarRec: TVarRec): TObject;
begin
  case VarRec.VType of
    vtObject: Result := VarRec.VObject;
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to TObject');
  end;
end;

function VarRecToBoolean(const VarRec: TVarRec): Integer;
begin
  case VarRec.VType of
    vtBoolean: Result := Integer(VarRec.VBoolean);
  else
    raise Exception.Create('Type mismatch: is not possible convert TVarRec to Boolean');
  end;
end;

procedure SetObjectProperties(Instance: TObject; Properties: array of const);
var
  i, PropCount: Integer;
  PropInfo: PPropInfo;
  ClassInfo: PTypeInfo;
  PropertyName, PropertyValue: TVarRec;
begin
  PropCount := Length(Properties);
  if PropCount = 0 then
    Exit;
  if odd(PropCount) then
    raise Exception.Create('SetObjectProperties - Properties must of even length');
  if Instance = nil then
    raise Exception.Create('SetObjectProperties - Instance is nil');
  ClassInfo := Instance.ClassInfo;
  for i := Low(Properties) to PropCount - 2 do
  begin
    //param names are at an even index and should of ansistring type
    PropertyName := Properties[i];
    if odd(i) or (PropertyName.VType <> vtAnsiString) then
      continue;
    //todo: optimize - use GetPropInfos to get all properties at once
    PropInfo := GetPropInfo(ClassInfo, AnsiString(PropertyName.VAnsiString));
    if PropInfo <> nil then
    begin
      PropertyValue := Properties[i + 1];
      case PropInfo^.PropType^.Kind of
        tkAString, tkSString:
          SetStrProp(Instance, PropInfo, VarRecToString(PropertyValue));
        tkInteger:
          SetOrdProp(Instance, PropInfo, VarRecToInteger(PropertyValue));
        tkInt64:
          SetInt64Prop(Instance, PropInfo, VarRecToInt64(PropertyValue));
        tkClass:
          SetObjectProp(Instance, PropInfo, VarRecToObject(PropertyValue));
        tkBool:
          SetOrdProp(Instance, PropInfo, VarRecToBoolean(PropertyValue));
      else
        raise Exception.CreateFmt('SetObjectProperties - Error setting %s: kind %s is not supported',
          [PropInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(PropInfo^.PropType^.Kind))]);
      end;
    end;
  end;
end;

end.

