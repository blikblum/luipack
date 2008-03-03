{ @author(Michalis Kamburelis) }

{
 This is based in PasDoc_ProcessLineTalk from pasdoc project
 Clean up and improvements by Luiz Américo Pereira Câmara
}

unit ProcessLineTalk;

{$mode objfpc}
{$H+}

interface

uses 
  SysUtils, Classes, Process;

type
  { TTextReader reads given Stream line by line.
    Lines may be terminated in Stream with #13, #10, #13+#10 or #10+#13.
    This way I can treat any TStream quite like standard Pascal text files:
    I have simple Readln method.

    After calling Readln or Eof you should STOP directly using underlying
    Stream (but you CAN use Stream right after creating
    TTextReader.Create(Stream) and before any Readln or Eof
    operations on this TTextReader).

    Original version of this class comes from Michalis Kamburelis
    code library, see [http://www.camelot.homedns.org/~michalis/],
    unit base/KambiClassUtils.pas. }
    
  TTextReader = class
  private
    Stream: TStream;
    ReadBuf: string;
    FOwnsStream: boolean;
    { This is either #0 or #10 (tells to ignore next #13 char) or #13
      (tells to ignore next #10 char) }
    LastNewLineChar: char;
  public
    { This is a comfortable constructor, equivalent to
        TTextReader.Create(TFileStream.Create(FileName, fmOpenRead), true) }
    constructor CreateFromFileStream(const FileName: string);

    { If AOwnsStream then in Destroy we will free Stream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean);
    destructor Destroy; override;

    { Reads next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    function Eof: boolean;
  end;

  { This is a subclass of TProcess that allows to easy "talk"
    with executed process by pipes (read process stdout/stderr,
    write to process stdin) on a line-by-line basis.
   }

  { TProcessLineTalk }

  TProcessLineTalk = class(TProcess)
  private
    OutputLineReader: TTextReader;
  public
    { Adds poUsePipes to Options, since it's not reasonable to use
      this class when you don't want to communicate with process using
      pipes. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    procedure WriteLine(const S: string);
    function ReadLine: string;
    function ReadLine(SkipLines: Integer): String;
    function HasOutput: Boolean;
  end;

implementation

{ TTextReader ---------------------------------------------------------------- }

constructor TTextReader.CreateFromFileStream(const FileName: string);
begin
 Create(TFileStream.Create(FileName, fmOpenRead), true);
end;

constructor TTextReader.Create(AStream: TStream; AOwnsStream: boolean);
begin
 inherited Create;
 Stream := AStream;
 FOwnsStream := AOwnsStream;
 LastNewLineChar := #0;
end;

destructor TTextReader.Destroy;
begin
 if FOwnsStream then Stream.Free;
 inherited;
end;

function TTextReader.Readln: string;
const 
  BUF_INC = 100;
var 
  ReadCnt, i: integer;
begin
 i := 1;

 { Note that ReadBuf may contain data that we
   already read from stream at some time but did not returned it to
   user of this class
   (because we realized we have read too much). }

 repeat
  if i > Length(ReadBuf) then
  begin
   SetLength(ReadBuf, Length(ReadBuf) + BUF_INC);
   ReadCnt := Stream.Read(ReadBuf[Length(ReadBuf) - BUF_INC + 1], BUF_INC);
   SetLength(ReadBuf, Length(ReadBuf) - BUF_INC + ReadCnt);
   if ReadCnt = 0 then
   begin
    Result := ReadBuf;
    ReadBuf := '';
    Exit;
   end;
  end;

  if ((ReadBuf[i] = #10) and (LastNewLineChar = #13)) or 
     ((ReadBuf[i] = #13) and (LastNewLineChar = #10)) then
  begin
   { We got 2nd newline character ? Ignore it. }
   Assert(i = 1);
   Delete(ReadBuf, 1, 1);
   LastNewLineChar := #0;
  end else
  if ReadBuf[i] in [#10, #13] then
  begin
   Result := Copy(ReadBuf, 1, i-1);
   LastNewLineChar := ReadBuf[i];
   Delete(ReadBuf, 1, i);
   Exit;
  end else
  begin
   LastNewLineChar := #0;
   Inc(i);
  end;
 until false;
end;

function TTextReader.Eof: boolean;
var ReadCnt: Integer;
begin
 if ReadBuf = '' then
 begin
  SetLength(ReadBuf, 1);
  ReadCnt := Stream.Read(ReadBuf[1], 1);
  SetLength(ReadBuf, ReadCnt);
 end;
 Result := ReadBuf = '';
end;

{ TProcessLineTalk ----------------------------------------------------------- }

constructor TProcessLineTalk.Create(AOwner: TComponent);
begin
  inherited;
  Options := Options + [poUsePipes, poStdErrToOutput];
end;

destructor TProcessLineTalk.Destroy;
begin
  FreeAndNil(OutputLineReader);
  Active := False;
  inherited;
end;

procedure TProcessLineTalk.Execute;
begin
  inherited;
  FreeAndNil(OutputLineReader);
  OutputLineReader := TTextReader.Create(Output, false);
end;

procedure TProcessLineTalk.WriteLine(const S: string);
var
  LineTerminator: string;
begin
  if Length(S) > 0 then 
    Input.WriteBuffer(S[1], Length(S));
  LineTerminator := LineEnding;
  Input.Write(LineTerminator[1], Length(LineTerminator));
end;

function TProcessLineTalk.ReadLine: string;
begin
  Result := OutputLineReader.Readln;
end;

function TProcessLineTalk.ReadLine(SkipLines: Integer): String;
begin
  while SkipLines > 0 do
  begin
    //ReadLine; //this does not compiles
    OutputLineReader.Readln;
    Dec(SkipLines);
  end;
  Result := OutputLineReader.Readln;
end;

function TProcessLineTalk.HasOutput: Boolean;
begin
  Result := not OutputLineReader.Eof;
end;

end.
