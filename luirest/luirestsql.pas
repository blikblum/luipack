unit LuiRESTSQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LuiREST, HTTPDefs;

type

  {$INTERFACES CORBA}

  ISQLResponseBuilder = interface
  ['lui_sqlresponsebuilder']
    procedure BuildJSON(const Sql: String; Response: TResponse);
  end;

  { TCustomSqlRESTResource }

  TCustomSqlRESTResource = class(TCustomRESTResource)
  private
    FResponseBuilder: ISQLResponseBuilder;
  published
    property ResponseBuilder: ISQLResponseBuilder read FResponseBuilder write FResponseBuilder;
  end;

implementation

end.

