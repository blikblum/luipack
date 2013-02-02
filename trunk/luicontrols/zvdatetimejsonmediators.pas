unit ZVDateTimeJSONMediators;

{$mode objfpc}{$H+}

interface

uses
  Classes, LuiJSONLCLViews, ZVDateTimePicker, fpjson, sysutils, Controls;

type

  { TZVDateTimerJSONMediator }

  TZVDateTimerJSONMediator = class(TCustomJSONGUIMediator)
  public
    class procedure DoGUIToJSON(Control: TControl; JSONObject: TJSONObject; const PropName: String;
      Options: TJSONData); override;
    class procedure DoJSONToGUI(JSONObject: TJSONObject; const PropName: String; Control: TControl;
      Options: TJSONData); override;
  end;

implementation

uses
  LuiJSONUtils, Math;

{ TZVDateTimerJSONMediator }

class procedure TZVDateTimerJSONMediator.DoGUIToJSON(Control: TControl; JSONObject: TJSONObject;
  const PropName: String; Options: TJSONData);
var
  Timer: TZVDateTimePicker absolute Control;
begin
  if Timer.DateIsNull then
    RemoveJSONProp(JSONObject, PropName)
  else
  begin
    case Timer.Kind of
      dtkDate:
        JSONObject.Integers[PropName] := Trunc(Timer.Date);
      dtkDateTime:
        JSONObject.Floats[PropName] := Timer.DateTime;
      dtkTime:
        JSONObject.Floats[PropName] := Timer.Time;
    end;
  end;
end;

class procedure TZVDateTimerJSONMediator.DoJSONToGUI(JSONObject: TJSONObject; const PropName: String;
  Control: TControl; Options: TJSONData);
var
  Timer: TZVDateTimePicker absolute Control;
  PropData: TJSONData;
begin
  PropData := GetJSONProp(JSONObject, PropName);
  if (PropData = nil) or (PropData.JSONType = jtNull) then
    Timer.DateTime := NaN
  else
  begin
    case Timer.Kind of
      dtkDate:
      begin
        case PropData.JSONType of
          jtString: Timer.Date := StrToDate(PropData.AsString);
          jtNumber:
          begin
            if TJSONNumber(PropData).NumberType = ntFloat then
              Timer.Date := PropData.AsFloat
            else
              Timer.Date := PropData.AsInteger;
          end;
        end;
      end;
      dtkDateTime:
      begin
        case PropData.JSONType of
          jtString: Timer.Date := StrToDateTime(PropData.AsString);
          jtNumber:
          begin
            if TJSONNumber(PropData).NumberType = ntFloat then
              Timer.DateTime := PropData.AsFloat
            else
              Timer.DateTime := PropData.AsInteger;
          end;
        end;
      end;
      dtkTime:
      begin
        case PropData.JSONType of
          jtString: Timer.Date := StrToTime(PropData.AsString);
          jtNumber:
          begin
            if TJSONNumber(PropData).NumberType = ntFloat then
              Timer.Time := PropData.AsFloat
            else
              Timer.Time := PropData.AsInteger;
          end;
        end;
      end;
    end;
  end;
end;

initialization
  RegisterJSONMediator(TZVDateTimePicker, TZVDateTimerJSONMediator);

end.

