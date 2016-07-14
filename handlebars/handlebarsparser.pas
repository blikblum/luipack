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

  { TStatementList }

  TStatementList = class
  private
    FList: TFPObjectList;
    function GetCount: Integer; inline;
    function GetItems(Index: Integer): THandlebarsStatement; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Statement: THandlebarsStatement): Integer; inline;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: THandlebarsStatement read GetItems; default;
  end;

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
    FBody: TStatementList;
    FBlockParams: TStringArray;
  public
    constructor Create;
    destructor Destroy; override;
    property BlockParams: TStringArray read FBlockParams;
    property Body: TStatementList read FBody;
  end;


implementation

{ THandlebarsMustacheStatement }

function THandlebarsMustacheStatement.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function THandlebarsMustacheStatement.GetParams(Index: Integer): THandlebarsExpression;
begin
  Result := THandlebarsExpression(FParams[Index]);
end;

{ TStatementList }

function TStatementList.GetItems(Index: Integer): THandlebarsStatement;
begin
  Result := THandlebarsStatement(FList[Index]);
end;

function TStatementList.GetCount: Integer;
begin
  Result := FList.Count;
end;

constructor TStatementList.Create;
begin
  FList := TFPObjectList.Create;
end;

destructor TStatementList.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

function TStatementList.Add(Statement: THandlebarsStatement): Integer;
begin
  Result := FList.Add(Statement);
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

constructor THandlebarsProgram.Create;
begin
  FBody := TStatementList.Create;
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

