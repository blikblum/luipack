unit oleutils;

{ OLE helper functions

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}


//todo: add error handling

{$mode objfpc}{$H+}

interface
{$ifdef Windows}
uses
  Windows, Classes, ActiveX;

type

  { TOLEStream }

  TOLEStream = class (TStream)
  private
    FSrcStream: IStream;
  public
    constructor Create(const Stream: IStream);
    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; overload; override;
    function Write(const Buffer; Count: Integer): Integer; override;
  end;
{$endif}
implementation
{$ifdef Windows}
{ TOLEStream }

constructor TOLEStream.Create(const Stream: IStream);
begin
  inherited Create;
  FSrcStream:=Stream;
end;

function TOLEStream.Read(var Buffer; Count: Integer): Integer;
begin
  FSrcStream.Read(@Buffer, Count, @Result);
end;

function TOLEStream.Seek(Offset: Integer; Origin: Word): Integer;
var
  liResult, liOffset : LARGE_INTEGER;
begin
  //soFrom* constants are equal to STREAM_SEEK_* constants. Assume it here
  liOffset.LowPart:=Offset;
  liOffset.HighPart:=0;
  FSrcStream.Seek(liOffset, Origin, liResult);
  Result:=liResult.LowPart;
end;

function TOLEStream.Write(const Buffer; Count: Integer): Integer;
begin
  FSrcStream.Write(@Buffer,Count,@Result);
end;
{$endif}
end.

