unit delphicompat;

{ Delphi Compatibility Unit

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

{$mode objfpc}{$H+}

interface

uses
  LMessages,LCLIntf, Types, LCLType;

const
  //Messages
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;

  //Misc Constants
  MAXSHORT = $7FFF;
  
type
  //TWM* types
  TMessage = TLMessage;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;

//Unicode functions

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentExPoint(DC: HDC; p2: PChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;

implementation

{$i delphicompat.inc}


end.

