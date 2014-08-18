unit JSONZVDateTimeMediator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ZVDateTimePicker, JSONFormMediator, fpjson;

type

  { TJSONZVDateTimePickerMediator }

  TJSONZVDateTimePickerMediator = class(TJSONGUIMediator)
  public
    class procedure DoJSONToGUI(Data: TJSONObject; Element: TJSONFormElement); override;
    class procedure DoGUIToJSON(Element: TJSONFormElement; Data: TJSONObject); override;
  end;

implementation

uses
  Math;

{ TJSONZVDateTimePickerMediator }

class procedure TJSONZVDateTimePickerMediator.DoJSONToGUI(Data: TJSONObject;
  Element: TJSONFormElement);
var
  Timer: TZVDateTimePicker;
  PropData: TJSONData;
  PropName: String;
begin
  Timer := Element.Control as TZVDateTimePicker;
  PropName := Element.PropertyName;
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

class procedure TJSONZVDateTimePickerMediator.DoGUIToJSON(
  Element: TJSONFormElement; Data: TJSONObject);
var
  Timer: TZVDateTimePicker;
  PropName: String;
begin
  Timer := Element.Control as TZVDateTimePicker;
  PropName := Element.PropertyName;
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

initialization
  RegisterJSONMediator(TZVDateTimePicker, TJSONZVDateTimePickerMediator);

end.

