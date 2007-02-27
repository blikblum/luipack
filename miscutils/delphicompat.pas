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
  LMessages, Types, LCLType;

const
  //Messages
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;
  WM_CHAR = LM_CHAR;
  WM_KEYDOWN = LM_KEYDOWN;
  WM_KILLFOCUS = LM_KILLFOCUS;
  
  //Misc Constants
  MAXSHORT = $7FFF;
  
type
  //TWM* types
  TMessage = TLMessage;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;
  TWMChar = TLMChar;
  TWMKeyDown = TLMKeyDown;
  TWMKillFocus = TLMKillFocus;
  
//Unicode functions

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: LongInt; Rect: PRect;
  Str: PWideChar; Count: LongInt; Dx: PInteger): Boolean;

function TextOutW(DC: HDC; X,Y : Integer; Str : PWideChar; Count: Integer) : Boolean;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): Boolean;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

//GDI Functions

function GetTextExtentExPoint(DC: HDC; p2: PChar; p3, p4: Integer; p5, p6: PInteger; var p7: TSize): BOOL;

function InvertRect(hDC: HDC; var lprc: TRECT): Boolean;

//timer

type
  TTimerNotify = procedure (TimerId: LongWord) of Object;

  TLMTimer = record
    Msg: Cardinal;
    TimerID: LongWord;
    TimerProc: LPARAM; //TTimerNotify;
    Result: LRESULT;
  end;


function SetTimer(hWnd:THandle; nIDEvent:LongWord; uElapse:LongWord; lpTimerFunc:TTimerNotify):LongWord;

implementation

uses
{$i uses.inc}
  maps, LCLIntf;

type
  TTimerRecord = record
    Control: Pointer;
    Notify: TTimerNotify;
  end;

  { TTimerList }

  TTimerList = class
  private
    FHandleList: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: Pointer);
    function GetTimerInfo(Handle: hWnd; idEvent:LongWord; out TimerInfo: TTimerRecord):Boolean;
  end;

var
  FTimerList: TTimerList;
  
{ TTimerList }

constructor TTimerList.Create;
begin
  //todo: see 64bit (itu8??)
  FHandleList:=TMap.Create(itu4,SizeOf(TMap));
end;

destructor TTimerList.Destroy;
begin
  FHandleList.Destroy;
  inherited Destroy;
end;

procedure TTimerList.Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: Pointer);
var
  AIDList: TMap;
  ATimerRec: TTimerRecord;
begin
  ATimerRec.Notify:= NotifyFunc;
  ATimerRec.Control:= WinControl;
  with FHandleList do
  begin
    if GetData(hWnd,AIDList) then
    begin
      if AIDList.HasId(ID) then
        AIDList.SetData(ID,ATimerRec)
      else
        AIDList.Add(ID,ATimerRec);
    end
    else
    begin
      AIDList:=TMap.Create(itu4,SizeOf(TTimerRecord));
      Add(hWnd,AIDList);
      AIDList.Add(ID,ATimerRec);
    end;
  end;
end;

function TTimerList.GetTimerInfo(Handle: hWnd; idEvent: LongWord; out
  TimerInfo: TTimerRecord): Boolean;
var
  AIDList: TMap;
begin
  Result:=False;
  if FHandleList.GetData(Handle,AIDList) then
    if AIDList.GetData(idEvent,TimerInfo) then
      Result:=True;
end;

{$i delphicompat.inc}
{$i timer.inc}

initialization
  FTimerList:=TTimerList.Create;

finalization
  FTimerList.Free;

end.

