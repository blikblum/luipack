unit HandlebarsScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  THandlebarsToken = (
    tkEOF,
    tkContent,
    tkOpenPartial,
    tkOpenPartialBlock,
    tkOpenBlock,
    tkOpenEndBlock,
    tkEndBlock,
    tkOpenRawBlock,
    tkCloseRawBlock,
    tkEndRawBlock,
    tkOpenBlockParams,
    tkCloseBlockParams,
    tkOpenSExpr,
    tkCloseSExpr,
    tkInverse,
    tkOpenInverse,
    tkOpenInverseChain,
    tkOpenUnescaped,
    tkCloseUnescaped,
    tkOpen,
    tkClose,
    tkComment,
    tkEquals,
    tkId,
    tkSep,
    tkData,
    tkBoolean,
    tkNumber,
    tkString,
    tkUndefined,
    tkNull,
    tkInvalid
  );

  //inspired by fpc jsonscanner

  { THandlebarsScanner }

  THandlebarsScanner = class
  private
     FSource : TStringList;
     FCurToken: THandlebarsToken;
     FCurTokenString: string;
     FCurLine: string;
     TokenStr: PChar;
     FCurRow: Integer;
     FMustacheLevel: Integer;
     function FetchLine: Boolean;
     function GetCurColumn: Integer;
     procedure ScanContent;
   protected
     procedure Error(const Msg: string);overload;
     procedure Error(const Msg: string; const Args: array of Const);overload;
   public
     constructor Create(Source : TStream); overload;
     constructor Create(const Source : String); overload;
     destructor Destroy; override;
     function FetchToken: THandlebarsToken;

     property CurLine: string read FCurLine;
     property CurRow: Integer read FCurRow;
     property CurColumn: Integer read GetCurColumn;

     property CurToken: THandlebarsToken read FCurToken;
     property CurTokenString: string read FCurTokenString;
   end;

implementation

const
  Separators = ['.', '/'];

{ THandlebarsScanner }

function THandlebarsScanner.FetchLine: Boolean;
begin
  Result := FCurRow < FSource.Count;
  if Result then
  begin
    FCurLine := FSource[FCurRow];
    TokenStr := PChar(FCurLine);
    Inc(FCurRow);
  end
  else
  begin
    FCurLine := '';
    TokenStr := nil;
  end;
end;

function THandlebarsScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PChar(CurLine);
end;

procedure THandlebarsScanner.ScanContent;
var
  TokenStart: PChar;
  SectionLength: Integer;
begin
  TokenStart := TokenStr;
  while True do
  begin
    Inc(TokenStr);
    SectionLength := TokenStr - TokenStart;
    if TokenStr[0] = #0 then
    begin
      if not FetchLine then
      begin
        SetLength(FCurTokenString, SectionLength);
        Move(TokenStart^, FCurTokenString[1], SectionLength);
        Break;
      end;
    end;
    if (TokenStr[0] = '{') and (TokenStr[1] = '{') then
    begin
      SetLength(FCurTokenString, SectionLength);
      Move(TokenStart^, FCurTokenString[1], SectionLength);
      Break;
    end;
  end;
end;

procedure THandlebarsScanner.Error(const Msg: string);
begin

end;

procedure THandlebarsScanner.Error(const Msg: string; const Args: array of const);
begin

end;

constructor THandlebarsScanner.Create(Source: TStream);
begin
  FSource := TStringList.Create;
  FSource.LoadFromStream(Source);
end;

constructor THandlebarsScanner.Create(const Source: String);
begin
  FSource := TStringList.Create;
  FSource.Text := Source;
end;

destructor THandlebarsScanner.Destroy;
begin
  FSource.Destroy;
  inherited Destroy;
end;

function THandlebarsScanner.FetchToken: THandlebarsToken;
var
  TokenStart: PChar;
  SectionLength: Integer;
begin
  if (TokenStr = nil) and not FetchLine then
  begin
    Result := tkEOF;
    FCurToken := Result;
    Exit;
  end;

  Result := tkInvalid;

  FCurTokenString := '';

  case TokenStr[0] of
    #0:         // Empty line
      begin
        if not FetchLine then
        begin
          Result := tkEOF;
        end;
      end;
    '{':
      begin
        //{{
        TokenStart := TokenStr;
        if TokenStr[1] = '{' then
        begin
          Result := tkOpen;
          Inc(TokenStr, 2);
          case TokenStr[0] of
            '>': Result := tkOpenPartial;
            '#': Result := tkOpenBlock;
            '&': Inc(TokenStr);
            '{': Result := tkOpenUnescaped;
          end;
          if Result <> tkOpen then
            Inc(TokenStr);
          Inc(FMustacheLevel);
          SectionLength := TokenStr - TokenStart;
          SetLength(FCurTokenString, SectionLength);
          Move(TokenStart^, FCurTokenString[1], SectionLength);
        end
        else
        begin
          Result := tkContent;
          ScanContent;
        end;
      end;
    '}':
      begin
        TokenStart := TokenStr;
        if (TokenStr[1] = '}') and (FMustacheLevel > 0) then
        begin
          if TokenStr[2] = '}' then
          begin
            Result := tkCloseUnescaped;
            Inc(TokenStr, 3);
          end
          else
          begin
            Result := tkClose;
            Inc(TokenStr, 2);
          end;
          SectionLength := TokenStr - TokenStart;
          SetLength(FCurTokenString, SectionLength);
          Move(TokenStart^, FCurTokenString[1], SectionLength);
          Dec(FMustacheLevel);
        end
        else
        begin
          Result := tkContent;
          ScanContent;
        end;
      end;
  else
    if FMustacheLevel = 0 then
    begin
      Result := tkContent;
      ScanContent;
    end
    else
    begin
      while TokenStr[0] = ' ' do
        Inc(TokenStr);
      TokenStart := TokenStr;
      case TokenStr[0] of
        '/':
          begin
            Result := tkSep;
            Inc(TokenStr);
          end;
        '.':
          begin
            Result := tkSep;
            if TokenStr[1] = '.' then
            begin
              Result := tkId;
              Inc(TokenStr);
            end else if FCurToken <> tkId then
              Result := tkId;
            Inc(TokenStr);
          end;
      else
        Result := tkId;
        while True do
        begin
          if TokenStr[0] = #0 then
          begin
            if not FetchLine then
            begin
              SectionLength := TokenStr - TokenStart;
              SetLength(FCurTokenString, SectionLength);
              Move(TokenStart^, FCurTokenString[1], SectionLength);
              Break;
            end;
          end;
          if ((TokenStr[0] = '}') and (TokenStr[1] = '}')) or (TokenStr[0] in [' ', '.', '/']) then
            break;
          Inc(TokenStr);
        end;
      end;
      SectionLength := TokenStr - TokenStart;
      SetLength(FCurTokenString, SectionLength);
      Move(TokenStart^, FCurTokenString[1], SectionLength);
      //rigth trim space
      while TokenStr[0] = ' ' do
        Inc(TokenStr);
    end;
  end;

  FCurToken := Result;
end;

end.

