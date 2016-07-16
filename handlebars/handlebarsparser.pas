unit HandlebarsParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, HandlebarsScanner;

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

  { THandlebarsStringLiteral }

  THandlebarsStringLiteral = class(THandlebarsLiteral)
  private
    FValue: String;
    FOriginal: String;
  public
    constructor Create(const AValue: String);
    property Value: String read FValue;
  end;

  { THandlebarsBooleanLiteral }

  THandlebarsBooleanLiteral = class(THandlebarsLiteral)
  private
    FValue: Boolean;
    FOriginal: Boolean;
  public
    constructor Create(const AValue: String);
    property Value: Boolean read FValue;
  end;

  { THandlebarsNumberLiteral }

  THandlebarsNumberLiteral = class(THandlebarsLiteral)
  private
    FValue: Double;
    FOriginal: Double;
  public
    constructor Create(const ValueStr: String);
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
    constructor Create;
    destructor Destroy; override;
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

  { THandlebarsContentStatement }

  THandlebarsContentStatement = class(THandlebarsStatement)
  private
    FValue: String;
    FOriginal: String;
  public
    constructor Create(const Value: String);
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


  { THandlebarsHashPair }

  THandlebarsHashPair = class(THandlebarsNode)
  private
    FKey: String;
    FValue: THandlebarsExpression;
  public
    constructor Create(const AKey: String; AValue: THandlebarsExpression);
    destructor Destroy; override;
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
    constructor Create;
    destructor Destroy; override;
    function AddPair(Pair: THandlebarsHashPair): THandlebarsHashPair;
    property PairCount: Integer read GetPairCount;
    property Pairs[Index: Integer]: THandlebarsHashPair read GetPairs;
  end;

  { THandlebarsProgram }

  THandlebarsProgram = class(THandlebarsNode)
  private
    FBody: TFPObjectList; //[ Statement ]
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

  EHandlebarsParse = class(Exception);

  { THandlebarsParser }

  THandlebarsParser = class
  private
    FScanner: THandlebarsScanner;
    function ParseExpression(AllowSubExpression: Boolean): THandlebarsExpression;
    function ParseMustache: THandlebarsMustacheStatement;
    function ParsePath(IsData: Boolean): THandlebarsPathExpression;
    function ParseProgram(BreakToken: THandlebarsToken): THandlebarsProgram;
    function ParseStatement: THandlebarsStatement;
    procedure UnexpectedToken(Expected: THandlebarsTokens);
  public
    constructor Create(Source : TStream); overload;
    constructor Create(const Source : String); overload;
    destructor Destroy; override;
    function Parse: THandlebarsProgram;
  end;

implementation

{ THandlebarsHashPair }

constructor THandlebarsHashPair.Create(const AKey: String; AValue: THandlebarsExpression);
begin
  FKey := AKey;
  FValue := AValue;
end;

destructor THandlebarsHashPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

{ THandlebarsBooleanLiteral }

constructor THandlebarsBooleanLiteral.Create(const AValue: String);
begin
  FValue := AValue = 'true';
  FOriginal := FValue;
end;

{ THandlebarsStringLiteral }

constructor THandlebarsStringLiteral.Create(const AValue: String);
begin
  FValue := AValue;
  FOriginal := AValue;
end;

{ THandlebarsNumberLiteral }

constructor THandlebarsNumberLiteral.Create(const ValueStr: String);
begin
  FValue := StrToFloat(ValueStr);
  FOriginal := FValue;
end;

{ THandlebarsContentStatement }

constructor THandlebarsContentStatement.Create(const Value: String);
begin
  FValue := Value;
  FOriginal := Value;
end;

{ THandlebarsParser }

destructor THandlebarsParser.Destroy;
begin
  FScanner.Destroy;
end;

function THandlebarsParser.ParseExpression(AllowSubExpression: Boolean): THandlebarsExpression;
var
  T: THandlebarsToken;
begin
  T := FScanner.CurToken;
  case T of
    tkNumber: Result := THandlebarsNumberLiteral.Create(FScanner.CurTokenString);
    tkString: Result := THandlebarsStringLiteral.Create(FScanner.CurTokenString);
    tkBoolean: Result := THandlebarsBooleanLiteral.Create(FScanner.CurTokenString);
    tkNull: Result := THandlebarsNullLiteral.Create;
    tkUndefined: Result := THandlebarsUndefinedLiteral.Create;
    tkId: Result := ParsePath(False);
    tkData:
      begin
        if FScanner.FetchToken = tkId then
          Result := ParsePath(True)
        else
          UnexpectedToken([tkId]);
      end;
    tkOpenSExpr:
      begin
        if not AllowSubExpression then
          UnexpectedToken([tkUndefined..tkString, tkId, tkData]);
      end
    else
      UnexpectedToken([tkUndefined..tkString, tkId, tkData, tkOpenSExpr]);
  end;
  if T in LiteralTokens then
    FScanner.FetchToken;
end;

function THandlebarsParser.ParseMustache: THandlebarsMustacheStatement;
var
  PrevTokenString: String;
  Hash: THandlebarsHash;
  Expression: THandlebarsExpression;
begin
  Result := THandlebarsMustacheStatement.Create;
  FScanner.FetchToken;
  Result.FPath := ParseExpression(False);
  //params
  while FScanner.CurToken <> tkClose do
  begin
    PrevTokenString := FScanner.CurTokenString;
    Expression := ParseExpression(True);
    if FScanner.CurToken <> tkEquals then
      Result.FParams.Add(Expression)
    else
    begin
      //discard previous expression
      Expression.Destroy;
      Result.FHash := THandlebarsHash.Create;
      FScanner.FetchToken;
      Expression := ParseExpression(True);
      Result.FHash.AddPair(THandlebarsHashPair.Create(PrevTokenString, Expression));
      //hash
      while FScanner.CurToken = tkId do
      begin
        PrevTokenString := FScanner.CurTokenString;
        if FScanner.FetchToken = tkEquals then
        begin
          FScanner.FetchToken;
          Expression := ParseExpression(True);
          Result.FHash.AddPair(THandlebarsHashPair.Create(PrevTokenString, Expression));
        end
        else
          UnexpectedToken([tkEquals]);
      end;
    end;
  end;
end;

function THandlebarsParser.ParsePath(IsData: Boolean): THandlebarsPathExpression;
var
  PartCount: Integer;
  Part: String;
begin
  Result := THandlebarsPathExpression.Create;
  Result.FData := IsData;
  repeat
    Part := FScanner.CurTokenString;
    if Part <> 'this' then
    begin
      if Part = '..' then
        Inc(Result.FDepth)
      else
      begin
        PartCount := Length(Result.Parts);
        SetLength(Result.FParts, PartCount + 1);
        Result.FParts[PartCount] := Part;
      end;
    end;
    if FScanner.FetchToken = tkSep then
    begin
      if FScanner.FetchToken <> tkId then
        UnexpectedToken([tkId]);
    end
    else
      Break;
  until False;
end;

function THandlebarsParser.ParseProgram(BreakToken: THandlebarsToken): THandlebarsProgram;
var
  T: THandlebarsToken;
begin
  Result := THandlebarsProgram.Create;
  T := FScanner.FetchToken;
  while T <> BreakToken do
  begin
    Result.FBody.Add(ParseStatement);
    T := FScanner.FetchToken;
  end;
end;

function THandlebarsParser.ParseStatement: THandlebarsStatement;
var
  T: THandlebarsToken;
begin
  T := FScanner.CurToken;
  case T of
    tkContent:
      Result := THandlebarsContentStatement.Create(FScanner.CurTokenString);
    tkOpen, tkOpenUnescaped:
      begin
        Result := ParseMustache;
      end;
  end;
end;

function TokenSetToStr(Tokens: THandlebarsTokens): String;
var
  Token: THandlebarsToken;
  TokenStr: String;
begin
  Result := '[';
  for Token in Tokens do
  begin
    WriteStr(TokenStr, Token);
    Result := Result + TokenStr;
    Result := Result + ',';
  end;
  Result := Result + ']';
end;


procedure THandlebarsParser.UnexpectedToken(Expected: THandlebarsTokens);
var
  ActualStr, ExpectedStr: String;
begin
  WriteStr(ActualStr, FScanner.CurToken);
  ExpectedStr := TokenSetToStr(Expected);
  raise EHandlebarsParse.CreateFmt('Got %s expected %s', [ActualStr, ExpectedStr]);
end;

constructor THandlebarsParser.Create(Source: TStream);
begin
  FScanner := THandlebarsScanner.Create(Source);
end;

constructor THandlebarsParser.Create(const Source: String);
begin
  FScanner := THandlebarsScanner.Create(Source);
end;


function THandlebarsParser.Parse: THandlebarsProgram;
begin
  Result := ParseProgram(tkEOF);
end;

{ THandlebarsHash }

function THandlebarsHash.GetPairCount: Integer;
begin
  Result := FPairs.Count;
end;

function THandlebarsHash.GetPairs(Index: Integer): THandlebarsHashPair;
begin
  Result := THandlebarsHashPair(FPairs[Index]);
end;

constructor THandlebarsHash.Create;
begin
  FPairs := TFPObjectList.Create;
end;

destructor THandlebarsHash.Destroy;
begin
  FPairs.Destroy;
  inherited Destroy;
end;

function THandlebarsHash.AddPair(Pair: THandlebarsHashPair): THandlebarsHashPair;
begin
  Result := Pair;
  FPairs.Add(Pair);
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

constructor THandlebarsMustacheStatement.Create;
begin
  FParams := TFPObjectList.Create;
end;

destructor THandlebarsMustacheStatement.Destroy;
begin
  FPath.Free;
  FHash.Free;
  FParams.Destroy;
  inherited Destroy;
end;

{ THandlebarsNode }

function THandlebarsNode.GetNodeType: String;
const
  PrefixOffset = 12; //THandlebars
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

end.

