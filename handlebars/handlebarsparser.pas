unit HandlebarsParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  THandlebarsHash = class;
  THandlebarsProgram = class;
  THandlebarsStatement = class;

  TStripFlag = (sfOpen, sfClose);

  TStripFlags = set of TStripFlag;

  TStringArray = array of String;

  { THandlebarsNode }

  THandlebarsNode = class
  protected
    function GetNodeType: String;
  public
    //function GetValue(Context: TJSONData): String; virtual; abstract;
    property NodeType: String read GetNodeType;
  end;

  THandlebarsExpression = class(THandlebarsNode)
  private
  end;

  { THandlebarsPathExpression }

  THandlebarsPathExpression = class(THandlebarsExpression)
  private
    FOriginal: String;
    FParts: TStringArray;
    FDepth: Integer;
    FData: Boolean;
  public
    constructor Create(const Path: String);
    property Data: Boolean read FData;
    property Depth: Integer read FDepth;
    property Parts: TStringArray read FParts;
  end;

  { THandlebarsSubExpression }

  THandlebarsSubExpression = class(THandlebarsExpression)
  private
    FPath: THandlebarsPathExpression;
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    constructor Create(const Expression: String);
    destructor Destroy; override;
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsPathExpression read FPath;
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
  public
    property Value: String read FValue;
  end;

  THandlebarsBooleanLiteral = class(THandlebarsLiteral)
  private
    FValue: Boolean;
    FOriginal: Boolean;
  public
    property Value: Boolean read FValue;
  end;

  THandlebarsNumberLiteral = class(THandlebarsLiteral)
  private
    FValue: Double;
    FOriginal: Double;
  public
    property Value: Double read FValue;
  end;

  THandlebarsNullLiteral = class(THandlebarsLiteral)
  private
  end;

  THandlebarsUndefinedLiteral = class(THandlebarsLiteral)
  private
  end;

  THandlebarsStatement = class(THandlebarsNode)
  end;

  { THandlebarsMustacheStatement }

  THandlebarsMustacheStatement = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList;   //[Expression]
    FHash: THandlebarsHash;
    FStrip: TStripFlags;
    FScaped: Boolean;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read FPath;
  end;

  { THandlebarsBlockStatement }

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
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Hash: THandlebarsHash read FHash;
    property Inverse: THandlebarsProgram read FInverse;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsPathExpression read FPath;
    property TheProgram: THandlebarsProgram read FProgram;
  end;

  { THandlebarsPartialStatement }

  THandlebarsPartialStatement = class(THandlebarsStatement)
  private
    FName: THandlebarsExpression; //PathExpression | SubExpression
    FParams: TFPObjectList; // [ Expression ]
    FHash: THandlebarsHash;
    FIndent: String;
    FStrip: TStripFlags;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Indent: String read FIndent;
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
  end;

  { THandlebarsPartialBlockStatement }

  THandlebarsPartialBlockStatement = class(THandlebarsStatement)
  private
    FName: THandlebarsExpression; //PathExpression | SubExpression
    FParams: TFPObjectList; // [ Expression ]
    FHash: THandlebarsHash;
    FProgram: THandlebarsProgram;
    FIndent: String;
    FOpenStrip: TStripFlags;
    FCloseStrip: TStripFlags;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Indent: String read FIndent;
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property TheProgram: THandlebarsProgram read FProgram;
  end;

  THandlebarsContentStatement = class(THandlebarsStatement)
  private
    FValue: String;
    FOriginal: String;
  public
    property Value: String read FValue;
  end;

  THandlebarsCommentStatement = class(THandlebarsStatement)
  private
    FValue: String;
    FStrip: TStripFlags;
  public
    property Value: String read FValue;
  end;

  { THandlebarsDecorator }

  THandlebarsDecorator = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;
    FStrip: TStripFlags;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read FPath;
  end;

  { THandlebarsDecoratorBlock }

  THandlebarsDecoratorBlock = class(THandlebarsStatement)
  private
    FPath: THandlebarsExpression; //PathExpression | Literal
    FParams: TFPObjectList; //[ Expression ];
    FHash: THandlebarsHash;
    FProgram: THandlebarsProgram;
    FOpenStrip: TStripFlags;
    FCloseStrip: TStripFlags;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): THandlebarsExpression;
  public
    property Hash: THandlebarsHash read FHash;
    property Params[Index: Integer]: THandlebarsExpression read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Path: THandlebarsExpression read FPath;
    property TheProgram: THandlebarsProgram read FProgram;
  end;


  THandlebarsHashPair = class(THandlebarsNode)
  private
    FKey: String;
    FValue: THandlebarsExpression;
  public
    property Key: String read FKey;
    property Value: THandlebarsExpression read FValue;
  end;

  { THandlebarsHash }

  THandlebarsHash = class(THandlebarsNode)
  private
    FPairs: TFPObjectList; //[ HashPair ]
    function GetPairCount: Integer;
    function GetPairs(Index: Integer): THandlebarsHashPair;
  public
    property PairCount: Integer read GetPairCount;
    property Pairs[Index: Integer]: THandlebarsHashPair read GetPairs;
  end;

  { THandlebarsProgram }

  THandlebarsProgram = class(THandlebarsNode)
  private
    FBody: TFPObjectList;
    FBlockParams: TStringArray;
    function GetBody(Index: Integer): THandlebarsStatement;
    function GetBodyCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property BlockParams: TStringArray read FBlockParams;
    property Body[Index: Integer]: THandlebarsStatement read GetBody;
    property BodyCount: Integer read GetBodyCount;
  end;


implementation

{ THandlebarsHash }

function THandlebarsHash.GetPairCount: Integer;
begin
  Result := FPairs.Count;
end;

function THandlebarsHash.GetPairs(Index: Integer): THandlebarsHashPair;
begin
  Result := THandlebarsHashPair(FPairs[Index]);
end;

{ THandlebarsDecoratorBlock }

function THandlebarsDecoratorBlock.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsDecoratorBlock.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ THandlebarsDecorator }

function THandlebarsDecorator.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsDecorator.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ THandlebarsPartialBlockStatement }

function THandlebarsPartialBlockStatement.GetParamCount: Integer;
begin
  Result  := FParams.Count;
end;

function THandlebarsPartialBlockStatement.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ THandlebarsPartialStatement }

function THandlebarsPartialStatement.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsPartialStatement.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ THandlebarsBlockStatement }

function THandlebarsBlockStatement.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsBlockStatement.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ THandlebarsMustacheStatement }

function THandlebarsMustacheStatement.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsMustacheStatement.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

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

function THandlebarsSubExpression.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsSubExpression.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

constructor THandlebarsSubExpression.Create(const Expression: String);
begin

end;

destructor THandlebarsSubExpression.Destroy;
begin
  inherited Destroy;
end;

{ THandlebarsProgram }

function THandlebarsProgram.GetBody(Index: Integer): THandlebarsStatement;
begin
  Result := THandlebarsStatement(FBody[Index]);
end;

function THandlebarsProgram.GetBodyCount: Integer;
begin
  Result := FBody.Count;
end;

constructor THandlebarsProgram.Create;
begin
  FBody := TFPObjectList.Create;
end;

destructor THandlebarsProgram.Destroy;
begin
  FBody.Destroy;
  inherited Destroy;
end;

{ THandlebarsPathExpression }

constructor THandlebarsPathExpression.Create(const Path: String);
begin

end;

end.

