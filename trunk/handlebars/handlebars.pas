unit Handlebars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function RenderTemplate(const Template: String; Data: TJSONObject): String;

implementation

function RenderTemplate(const Template: String; Data: TJSONObject): String;
begin
  Result := '';
end;

end.

