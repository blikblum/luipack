{****************************************************************************
*                                                                           *
*                                    MMX                                    *
*                                                                           *
*                                                                           *
* Language:             FPC Pascal v1.00.10+ / Virtual Pascal 2.1b /        *
*                       Delphi 3 +                                          *
* Environment:          i386                                                *
* Supported extenders:  GO32V2/WIN32/Linux/OS/2                             *
*                                                                           *
* Required switches:                                                        *
*                                                                           *
* Author:               Thomas Schatzl                                      *
* License:              see the file COPYING, included in this              *
*                       distribution for details                            *
* Date:                 04.02.2005                                          *
* Version:              Rel1rev7                                            *
*                                                                           *
*      Send bug reports and feedback to tom_at_work@gmx.at                  *
*                                                                           *
* Description:  Compatibility unit to the RTL. You only need this unit if   *
*               you want to compile old code using the RTL MMX unit. The    *
*               CPU unit is usually sufficient for any detection purposes.  *
*                                                                           *
*               Original code from the FreePascal RTL library, done by the  *
*               Free Pascal Development team.                               *
*                                                                           *
*               Full compatibility to the RTL units (CPU and MMX)           *
*                                                                           *
*****************************************************************************}

unit MMX;

{$I platform.inc}
interface

{$IFDEF PPC_FPC}
	{$MODE DELPHI}
	{$ASMMODE INTEL}
{$ENDIF}

uses
        basetype;

type
        TMMXByte = array[0..7] of UInt8;
        TMMXWord = array[0..3] of UInt16;
        TMMXDWord = array[0..1] of UInt32;
        TMMXQWord = UInt64;


        TMMXShort = array[0..7] of Int8;
        TMMXInteger = array[0..3] of Int16;
        TMMXLongint = array[0..1] of Int32;
        TMMXInt64 = Int64;

        TMMXSingle = array[0..1] of Float32;

        TXMMSingle = array[0..3] of Float32;
        TXMMDouble = array[0..1] of Float64;

        PMMXByte = ^TMMXByte;
        PMMXWord = ^TMMXWord;
        PMMXDWord = ^TMMXDWord;
        PMMXQWord = ^TMMXQWord;

        PMMXShort = ^TMMXShort;
        PMMXInteger = ^TMMXInteger;
        PMMXLongint = ^TMMXLongint;
        PMMXInt64 = ^TMMXInt64;

        PMMXSingle = ^TMMXSingle;

        PXMMSingle = ^TXMMSingle;
        PXMMDouble = ^TXMMDouble;

{ compatibility }
        TMMXCardinal = TMMXDWord;
        TMMXShortInt = TMMXShort;

        PMMXCardinal = ^TMMXCardinal;
        PMMXShortInt = ^TMMXShortInt;

{ some regularly used constants }
const
        MMXAllClear : TMMXQWord = (0);
        MMXAllSet : TMMXWord = ($ffff, $ffff, $ffff, $ffff);

// logically 'and' this constant to pack the lower byte of the words into bytes, unsigned
// to be used with packssw instruction
        MMXPackUWB : TMMXByte = ($ff, $00, $ff, $00, $ff, $00, $ff, $00);

{$IFDEF PPC_FPC}

const
       is_mmx_cpu : bool8 = false;
       is_sse_cpu : bool8 = false;
       is_sse2_cpu : bool8 = false;
       is_amd_3d_cpu : bool8 = false;
       is_amd_3d_dsp_cpu : bool8 = false;
       is_amd_3d_mmx_cpu : bool8 = false;
       
{ sets all floating point registers to empty
(use this after mmx usage) }
function mmx_support : Bool8;
function amd_3d_support : Bool8;
function sse_support : Bool8;

{$ENDIF}

procedure Emms;
procedure FEmms;

implementation

uses
        cpu;
{$IFDEF PPC_FPC}
{ returns true, if the processor supports the mmx instructions }
function mmx_support : bool8;
begin
        mmx_support := cpusupports(capMMX);
end;

function amd_3d_support : bool8;
begin
        amd_3d_support := cpusupports(cap3DNOW);
end;

function sse_support : Bool8;
begin
        sse_support := cpusupports(capSSE);
end;
{$ENDIF}

procedure emms; assembler;
asm
        {$IFDEF ASM_FPC}
        emms
        {$ELSE}
        db 0fh, 77h // emms
        {$ENDIF}
end;

procedure FEmms; assembler;
asm
	db 0fh, 0eh // femms
end;

initialization

begin
        {$IFDEF PPC_FPC}
        if (cpusupports(capMMX)) then begin
                is_mmx_cpu := true;
                { the exit code sets the fpu stack to empty }
                is_amd_3d_cpu := cpusupports(cap3DNOW);
                is_sse_cpu := cpusupports(capSSE);
                is_sse2_cpu := cpusupports(capSSE2);
                is_amd_3d_mmx_cpu := cpusupports(capMMXplus);
                is_amd_3d_dsp_cpu := cpusupports(capE3DNOW);
        end;
        {$ENDIF}
end;

finalization

begin
        if (cpusupports(capMMX)) then begin
                emms;
        end;
end;

end.
{ History
        12-05-2000 start of history (rel1rev1)
        - added a few new often used typed variables
        - names of MMX types changed a bit (added missing to compatibility section)

        17-05-2000 adaption to CPU rel2rev2 (rel1rev2)
        - MMX uses basic integer types declared in CPU
        - added initialization/finalization section instead of exitproc etc
        - Added $MODE DELPHI switch

        09-09-2000 updates (rel1rev3)
        - multiplatform capability
        - FPC legacy functions only available on this platform
        - removed 'Fixed' MMX types

        28-03-2001 fixing finalization bug (rel1rev4)
        - emms was called unconditionally so that it crashed on non-MMX CPUs (reported
        by Pierre Mueller)

        14-04-2001 fixed non-FPC targets (rel1rev5)
        - didn't compile on non-FPC targets; bug introduced by rel1rev4 (reported by Ralph
        Roth)
        
        11-01-2005 fixed mail address in header (rel1rev6)
        - updated mail address in header (reported by Lee John)
        
        04-02-2005 compatibility fixes  (rel1rev7)
        - compatibility to current MMX unit from FPC restored
}
