unit EvaluationModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiJSONModel;

type

  { TEvaluation }

  TEvaluation = class(TJSONModel)
  protected
    class function GetResourceName: String; override;
  end;

implementation

{ TEvaluation }

class function TEvaluation.GetResourceName: String;
begin
  Result := 'evaluation';
end;

end.

