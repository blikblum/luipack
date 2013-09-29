unit ZVDateTimeJSONMediators;

{$mode objfpc}{$H+}

interface

uses
  Classes, LuiJSONLCLViews, ZVDateTimePicker, fpjson, sysutils, Controls;

type

  { TZVDateTimerJSONMediator }

  TZVDateTimerJSONMediator = class(TCustomJSONGUIMediator)
  public
    class procedure DoGUIToJSON(Control: TControl; Data: TJSONObject; const PropName: String;
      OptionsData: TJSONObject); override;
    class procedure DoJSONToGUI(Data: TJSONObject; const PropName: String; Control: TControl;
      OptionsData: TJSONObject); override;
  end;

implementation

uses
  Math;

{ TZVDateTimerJSONMediator }

class procedure TZVDateTimerJSONMediator.DoGUIToJSON(Control: TControl;
  Data: TJSONObject; const PropName: String; OptionsData: TJSONObject);
var
  Timer: TZVDateTimePicker absolute Control;
begin
  if Timer.DateIsNull then
    Data.Delete(PropName)
  else
  begin
    case Timer.Kind of
      dtkDate:
        Data.Integers[PropName] := Trunc(Timer.Date);
      dtkDateTime:
        Data.Floats[PropName] := Timer.DateTime;
      dtkTime:
        Data.Floats[PropName] := Timer.Time;
    end;
  end;
end;

class procedure TZVDateTimerJSONMediator.DoJSONToGUI(Data: TJSONObject;
  const PropName: String; Control: TControl; OptionsData: TJSONObject);
var
  Timer: TZVDateTimePicker absolute Control;
  PropData: TJSONData;
begin
  PropData := Data.Find(PropName);
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

