unit HandlebarsTestCase;

{$mode objfpc}{$H+}

interface

uses
  fpjson, Handlebars, LuiJSONUtils, TestFramework;

type

  { THandlebarsTestCase }

  THandlebarsTestCase = class(TTestCase)
  protected
    procedure CheckRender(const Template: String; const Elements : Array of Const;
      const Expected: String);
    procedure CheckRender(const Template, DataString, Expected: String);
  end;

implementation

function RenderAndFree(const Template: String; Data: TJSONObject): String;
begin
  Result := RenderTemplate(Template, Data);
  Data.Free;
end;

function RenderString(const Template, DataString: String): String;
var
  Data: TJSONObject;
begin
  Data := StrToJSON(DataString) as TJSONObject;
  Result := RenderTemplate(Template, Data);
  Data.Free;
end;

{ THandlebarsTestCase }

procedure THandlebarsTestCase.CheckRender(const Template: String;
  const Elements: array of const; const Expected: String);
begin
  CheckEquals(Expected, RenderAndFree(Template, TJSONObject.Create(Elements)));
end;

procedure THandlebarsTestCase.CheckRender(const Template, DataString,
  Expected: String);
begin
  CheckEquals(Expected, RenderString(Template, DataString));
end;


end.

