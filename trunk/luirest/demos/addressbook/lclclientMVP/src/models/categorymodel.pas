unit CategoryModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleJSONModel;

type

  { TCategory }

  TCategory = class(TSimpleJSONModel)
  protected
    class function GetResourceName: String; override;
  end;

implementation

{ TCategory }

class function TCategory.GetResourceName: String;
begin
  Result := 'category';
end;

end.

