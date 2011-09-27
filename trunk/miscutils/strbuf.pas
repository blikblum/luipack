 
unit StrBuf;
 
{$mode objfpc}{$H+}
 
{ Simple string buffer
 
  No setup/teardown required, memory mngmt, initialization, and finalization
  is done by FPC itself.
 
  Copyright (C) 2010 bflm, contact: befelemepeseveze at Google's free mail
 
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:
 
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.
 
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
 
  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
  Based on http://freepascal-bits.blogspot.com/2010/02/simple-string-buffer.html
  Modications by Luiz Américo:
  - Renamed methods / properties to more sensible names
  - Code cleanup
}
 
interface
 
uses
  SysUtils;
 
type
 
  { TStrBuf }
 
  TStrBuf = object
  private
    FLength: Integer;
    FBuffer: String;
    function GetAsString: String;
  public
    procedure Append(const S: String);
    procedure Append(const Fmt: String; Args: array of const);
    property AsString: String read GetAsString;
  end;
 
operator +(var Buf: TStrBuf; const S: String): TStrBuf;
operator :=(const S: String): TStrBuf;
operator :=(var Buf: TStrBuf): String;
 
implementation
 
{ TStrBuf }
 
function TStrBuf.GetAsString: String;
begin
  Result := LeftStr(FBuffer, FLength);
end;
 
procedure TStrBuf.Append(const S: String);
var
  SLength, NewLength: Integer;
begin
  SLength := Length(S);
  NewLength := FLength + SLength;
  if NewLength > Length(FBuffer) then
    SetLength(FBuffer, 2 * NewLength);
  Move(S[1], FBuffer[FLength + 1], SLength);
  FLength := NewLength;
end;
 
procedure TStrBuf.Append(const Fmt: String; Args: array of const);
begin
  Append(Format(Fmt, Args));
end;
 
operator +(var Buf: TStrBuf; const S: String): TStrBuf;
begin
  Result.FBuffer := Buf.FBuffer;
  Result.FLength := Buf.FLength;
  Result.Append(S);
end;
 
operator :=(const S: String): TStrBuf;
begin
  Result.FLength := Length(S);
  Result.FBuffer := S;
end;
 
operator :=(var Buf: TStrBuf): String;
begin
  Result := Buf.AsString;
end;
 
end.
