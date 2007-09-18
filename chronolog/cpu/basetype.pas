{****************************************************************************
*                                                                           *
*                              BASETYPE                                     *
*                                                                           *
*                                                                           *
* Language:             FPC Pascal v0.99.12+ / Virtual Pascal               *
* Supported platforms:  GO32V1, GO32V2, PMODE/DJ, LINUX, OS/2, WIN32        *
*                                                                           *
* Required switches:    none                                                *
*                                                                           *
* Author:               Thomas Schatzl                                      *
* License:              see the file COPYING, included in this distribution *
*                       for details about the copyright etc.                *
* Date:                 01.03.2005                                          *
* Version:              Rel1rev2                                            *
*                                                                           *
*      Send bug reports and feedback to tom_at_work@gmx.at                  *
*                                                                           *
* Description:  This unit defines the standard pascal types into a more     *
*               straightforward naming model. Additionally all these have   *
*               the same memory represention on all platforms.             ,*
*                                                                           *
* See also at the history at the end or the documentation (cpu package)     *
*                                                                           *
*****************************************************************************}

unit basetype;

{$I platform.inc}

{$IFDEF PPC_DELPHI}
	{$DEFINE NOPTRTYPE}
{$ENDIF}
{$IFDEF VER1_0}
	{$DEFINE NOPTRTYPE}
{$ENDIF}

interface

{ some basic types }
type
        Int8 = System.ShortInt;
        Int16 = System.SmallInt;
        Int32 = System.Longint;
        Int64 = System.Int64;

        UInt8 = System.Byte;
        UInt16 = System.Word;
        UInt32 = Cardinal;
        {$IFDEF PPC_FPC}
        UInt64 = QWord;
        {$ELSE}
        UInt64 = Int64;
        {$ENDIF}

        Float32 = System.Single;
        Float64 = System.Double;
        Float80 = System.Extended;

        Bool8 = System.Boolean;
        Bool16 = System.WordBool;
        Bool32 = System.LongBool;

{ pointers to all these }
        PUInt8 = ^UInt8;
        PUInt16 = ^UInt16;
        PUInt32 = ^UInt32;
        PUInt64 = ^UInt64;

        PInt8 = ^Int8;
        PInt16 = ^Int16;
        PInt32 = ^Int32;
        PInt64 = ^Int64;

        PFloat32 = ^Float32;
        PFloat64 = ^Float64;
        PFloat80 = ^Float80;

        PBool8 = ^Bool8;
        PBool16 = ^Bool16;
        PBool32 = ^Bool32;

        {$IFDEF NOPTRTYPE}
        PtrInt = Int32;
        PtrUInt = UInt32;
        {$ENDIF}

implementation

initialization

begin
end;

finalization

begin
end;

end.

{
        17-05-2000 start of unit, initial release
        - using FPU for 64 bit integer is default
        - adapted all units to use it
        - documentation in the CPU package

        07-09-2000 updates, conversion to VirtualPascal
        - removed initialization code

        08-12-2004 use native int64 type
        - instead of comp for 64 bit signed integers use given type
        
        11-01-2005 updated mail address in header (rel1rev1)
        - updated mail address in header (reported by Lee John)
}
