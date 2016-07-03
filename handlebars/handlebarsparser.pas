unit HandlebarsParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  THandlebarsHash = class;
  THandlebarsProgram = class;

  TStripFlag = (sfOpen, sfClose);

  TStripFlags = set of TStripFlag;

  { THandlebarsNode }

  THandlebarsNode = class
  protected
    function GetNodeType: String;
  public
    //function GetValue(Context: TJSONData): String; virtual; abstract;
  end;

  THandlebarsExpression = class(THandlebarsNode)
  private

  end;

  { THandlebarsPathExpression }

  THandlebarsPathExpression = class(THandlebarsExpression)
  private
    FOriginal: String;
    FParts: array of String;
    FDepth: Integer;
    FData: Boolean;
  public
    constructor Create(const Path: String);
  end;

  { THandlebarsSubExpression }

  THandlebarsSubExpression = class(THandlebarsExpression)
  private
    FPath: THandlebarsPathExpression;
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;
  public
    constructor Create(const Expression: String);
    destructor Destroy; override;
  end;

  { THandlebarsLiteral }

  THandlebarsLiteral = class(THandlebarsExpression)
  private
  public
  end;

  THandlebarsStringLiteral = class(THandlebarsLiteral)
  private
    FValue: String;
    FOriginal: String;
  end;

  THandlebarsBooleanLiteral = class(THandlebarsLiteral)
  private
    FValue: Boolean;
    FOriginal: Boolean;
  end;

  THandlebarsNumberLiteral = class(THandlebarsLiteral)
  private
    FValue: Double;
    FOriginal: Double;
  end;

  THandlebarsNullLiteral = class(THandlebarsLiteral)
  private
  end;

  THandlebarsUndefinedLiteral = class(THandlebarsLiteral)
  private
  end;

  THandlebarsStatement = class(THandlebarsNode)
  end;

  THandlebarsMustacheStatement = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList;
    FHash: THandlebarsHash;
    FStrip: TStripFlags;
    FScaped: Boolean;
  public

  end;

  THandlebarsBlockStatement = class(THandlebarsStatement)
  private
    FPath: THandlebarsPathExpression;
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;
    FProgram: THandlebarsProgram;
    FInverse: THandlebarsProgram;
    FOpenStrip: TStripFlags;
    FInverseStrip: TStripFlags;
    FCloseStrip: TStripFlags;
  end;

  THandlebarsPartialStatement = class(THandlebarsStatement)
  private
    FName: THandlebarsExpression; //PathExpression | SubExpression
    FParams: TFPObjectList; // [ Expression ]
    FHash: THandlebarsHash;
    FIndent: String;
    FStrip: TStripFlags;
  end;

  THandlebarsPartialBlockStatement = class(THandlebarsStatement)
  private
    FName: THandlebarsExpression; //PathExpression | SubExpression
    FParams: TFPObjectList; // [ Expression ]
    FHash: THandlebarsHash;
    FProgram: THandlebarsProgram;
    FIndent: String;
    FOpenStrip: TStripFlags;
    FCloseStrip: TStripFlags;
  end;

  THandlebarsContentStatement = class(THandlebarsStatement)
  private
    FValue: String;
    FOriginal: String;
  end;

  THandlebarsCommentStatement = class(THandlebarsStatement)
  private
    FValue: String;
    FStrip: TStripFlags;
  end;

  THandlebarsDecorator = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;

    FStrip: TStripFlags;
  end;

  THandlebarsDecoratorBlock = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;

    FProgram: THandlebarsProgram;

    FOpenStrip: TStripFlags;
    FCloseStrip: TStripFlags;
  end;

  THandlebarsHash = class(THandlebarsNode)
  private
    FPairs: TFPObjectList; //[ HashPair ]
  end;

  THandlebarsHashPair = class(THandlebarsNode)
  private
    FKey: String;
    FValue: THandlebarsExpression;
  end;

  { THandlebarsProgram }

  THandlebarsProgram = class(THandlebarsNode)
  private
    FBody: TFPObjectList;
    FBlockParams: array of String;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ THandlebarsNode }

function THandlebarsNode.GetNodeType: String;
const
  PrefixOffset = 11; //THandlebars
var
  TheClassName: String;
begin
  TheClassName := ClassName;
  Result := Copy(TheClassName, PrefixOffset, Length(TheClassName));
end;

{ THandlebarsSubExpression }

constructor THandlebarsSubExpression.Create(const Expression: String);
begin

end;

destructor THandlebarsSubExpression.Destroy;
begin
  inherited Destroy;
end;

{ THandlebarsProgram }

constructor THandlebarsProgram.Create;
begin

end;

destructor THandlebarsProgram.Destroy;
begin
  inherited Destroy;
end;

{ THandlebarsPathExpression }

constructor THandlebarsPathExpression.Create(const Path: String);
begin

end;

end.

