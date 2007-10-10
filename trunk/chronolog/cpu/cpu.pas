{****************************************************************************
*                                                                           *
*                                    CPU                                    *
*                                                                           *
*                                                                           *
* Language:             FPC Pascal v1.0.10+ / Delphi 7+                     *
* Supported platforms:  i386-WIN32/i386-*NIX                                *
*                                                                           *
* Required switches:    none                                                *
*                                                                           *
* Author:               Thomas Schatzl                                      *
* License:              see the file COPYING, included in this distribution *
*                       for details about the copyright etc.                *
* Date:                 27.02.2005                                          *
* Version:              Rel2rev29                                           *
*                                                                           *
*        Send bug reports and feedback to tom_at_work@gmx.at                *
*   You can always get the latest version/revision of this package from     *
*                                                                           *
*           http://members.yline.com/~tom_at_work/index.html                *
*                                  or                                       *
*      http://members.yline.com/~tom_at_work/cpudist.zip (direct link)      *
*                                                                           *
*                                                                           *
* Description:  CPU detection and timing unit. Detects various capabilities *
*               of CPUs like speed, rating, family, manufacturer, model,    *
*               stepping and different features of them (like CPUid support,*
*               MMX support, 3Dnow! support, KNI support, eMMX support....) *
*                                                                           *
*               Additionally it provides some objects to time code parts    *
*               with an accuracy of up to 1 microsecond, depending on the   *
*               timer used.                                                 *
*                                                                           *
*               To get maximum accuracy it is recommened that you enable    *
*               some optimization switches of the cómpiler to minimize over-*
*               head provided by objects.                                   *
*                                                                           *
*               Full compatibility to the RTL units (CPU and MMX)           *
*                                                                           *
* See also at the history at the end or the documentation (cpu.txt)         *
*                                                                           *
*****************************************************************************}
unit cpu;

{$DEFINE RELEASE}

{$I platform.inc}

{$IFDEF PPC_FPC}
	{$MODE DELPHI}
	{$ASMMODE INTEL}
	{$H+}
	{$INLINE ON}
	{$DEFINE OVERLOAD}
	
	{$IFNDEF VER1_0}
		{$DEFINE NEWCAPS}
	{$ENDIF}
{$ENDIF}
{$IFDEF PPC_DELPHI5UP}
	{$DEFINE OVERLOAD}
{$ENDIF}
{$IFDEF PPC_DELPHI9}
	{$INLINE AUTO} 
{$ENDIF}

interface

uses
	basetype, sysutils;

{ constants used to identify cpu unit release }
const
	cpu_version = 2;
	cpu_revision = 28;
	{$IFDEF RELEASE}
	cpu_build_type = 'release';
	{$ELSE}
	cpu_build_type = 'debug';
	{$ENDIF}
	cpu_build_date = '20050210';

{ this record holds the various CPU characteristics detected by this unit }

type
	TCPUVendor = (venAMD, venCentaur, venCyrix, venIntel, venNexGen, venRise,
		venUMC, venTransMeta, venNSC, venUnknown);

const
	CPUVendors : array[low(TCPUVendor)..high(TCPUVendor)] of string = (
		'AMD',
		'Centaur/IDT',
		'Cyrix/VIA',
		'Intel',
		'NexGen',
		'Rise',
		'Transmeta',
		'UMC',
		'National semiconductor',
		'Unknown');

type
	TVendorIdString = String[12];

const
	VendorIdStrings : array[low(TCPUVendor)..high(TCPUVendor)] of TVendorIdString = (
		'AuthenticAMD', 'CentaurHauls', 'CyrixInstead',
		'GenuineIntel', 'NexGenDriven', 'RiseRiseRise',
		'GenuineTMx86', ' UMC UMC UMC', 'Geode by NSC',
		'Other Vendor');

type
	TCPUType = (typePrimary, typeOverDrive, typeSecondary, typeReserved);

const
	CPUTypes : array[low(TCPUType)..high(TCPUType)] of string = (
		'Primary processor',
		'OverDrive processor',
		'Dual processor (MP)',
		'Reserved');

type
	TCPUFamily = UInt8;
	TCPUModel = UInt8;
	TCPUStepping = UInt8;

type
	TCPUCapabilities = (
		capFPU, capVME, capDE, capPSE, capTSC, capMSR, capPAE, capMCE, {8}
		capCX8, capAPIC, capSEP, capMTRR, capPGE, capMCA, capCMOV, capPAT, {16}
		capPSE36, capPSN, capMMX, capFXSR, capSSE, capMMXplus, capEMMX, capE3DNOW, {24}
		cap3DNOW, capCPUid, capCLFSH, capDTES, capACPI, capSSE2, capSLFSNP, capLM, {32}
		capTM, capHTT, capIA64, capPBE, capSSE3, capMON, capDSCPL, capEIST, capTM2,
		capCID, capxTPR, capEM64T, capNX, capTTP, capSYSCALL, capMP, cap3DNOWPro,
		capBrandId, capDAZ
		{$IFDEF NEWCAPS}
		, capDEBUGEXT = capDE, cap4MPAGE = capPSE, capMCHKXCP = capMCE, capCMPXCHG8B = capCX8, 
		capMCHKARCH = capMCA, capDS = capDTES, capSS = capSLFSNP,
		cap3DNOWEXT = capE3DNOW, capSSEMMX = capMMXplus
		{$ENDIF});
	TCapabilities = set of TCPUCapabilities;

type
	TCPUPSN = array[0..2] of UInt32;

type
	PCPUInfo = ^TCPUInfo;
	TCPUInfo = packed record
		next : PCPUInfo;
		baseLevel : UInt32;
		maxBaseLevel : UInt32;
		extLevel : UInt32;
		maxExtLevel : UInt32;		
		vendor : TCPUVendor;
		vendorIdString : TVendorIdString;
		cpuType : TCPUType;
		family : TCPUFamily;
		model : TCPUModel;
		stepping : TCPUStepping;
		caps : TCapabilities;
		psn : TCPUPSN;
		rating : UInt32;
		clockrate : Float64;
		// new to rel2rev23
		cpuCount : UInt16; // logical cpu count, 0 means N/A
		// new to rel2rev24
		brandId : UInt16; // brand Id as recommened by vendor (AMD/Intel only?)
		brandString : String; // brand string as recommened by vendor (AMD/Intel only?)
	end;

type
  TReadTimeFunction = function: Int64;


{ default cpu record, which is filled up at startup }

var
	this_cpu : TCPUInfo;

{ this procedure fills out the CPU record which is given to it as a parameter }

procedure GetCPUInfo(var cp : TCPUInfo);

{$IFDEF OVERLOAD}
function CPUSupports(const which : TCapabilities) : Bool8; overload;
{$ENDIF}
function CPUSupports(const which : TCPUCapabilities) : Bool8; {$IFDEF OVERLOAD} overload; {$ENDIF}

function CPUSupports2(const which : TCapabilities) : Bool8; overload;

function GetCPUString(const cpurec : TCPUInfo) : String;

{ returns the PSN string in readable format
}

function getPSNstring(ser : TCPUPSN) : string;

{ Base timer class.

  You are allowed to have as much timers timing at the same time as you want.
}

type
	ETimer = class(Exception);

	PTimer = ^TTimer;
	TTimer = class
		private

		mScale : Int64;
		mAutoReset : Bool32;
		mTiming : Bool32;

		mStartTime,
		mElapsedTime : Int64;

		protected
                ReadTime: TReadTimeFunction;
		// Converts a timers' raw time point to its' real time point
		// e.g. the one returned in resolution units of a second
		function CalcRealTime(const rawtime : Int64) : Int64; virtual; abstract;
		// Converts the real time duration to a timers' raw time
		function CalcRawTime(const realtime : Int64) : Int64; virtual; abstract;


		procedure SetScale(newscale : Int64); virtual;
		procedure SetAutoReset(newreset : Bool32); virtual;

		public

		constructor Create;

		procedure Reset; virtual;
		function Start : Int64; virtual;
		function Stop : Int64; virtual;
		function Pause : Int64; virtual;

		function Count : Int64; virtual;
		function Lap : Int64; virtual;
		function Resolution : Float64; virtual; abstract;

		function IsTiming : Bool32; virtual;

		procedure Delay(delaytime : Int64); virtual;
		procedure WaitFor(waittime : Int64); virtual;

		property Scale : Int64 read mScale write SetScale;
		property AutoReset : Bool32 read mAutoReset write SetAutoReset;

		property Time : Int64 read Count;
	end;

	PZenTimer = ^TZenTimer;
	TZenTimer = class(TTimer)

		private

		mRate : Float64;

		protected


		function CalcRealTime(const rawtime : Int64) : Int64; override;
		function CalcRawTime(const realtime : Int64) : Int64; override;

		public

		constructor Create;

		function Resolution : Float64; override;
	end;

{ TSysTimer }

TSysTimer = class(TTimer)


		function CalcRealTime(const rawtime : Int64) : Int64; override;
		function CalcRawTime(const realtime : Int64) : Int64; override;

		public
                constructor Create;
		function Resolution : Float64; override;
	end;

procedure Delay(ms : UInt32);
function Clock : Int64;

{ FPC RTL CPU compatibility procedures }
{$IFDEF PPC_FPC}
function cpuid_support : Bool8;
function floating_point_emulation : Bool8;
function cr0 : Int32;
{$ENDIF}

function cpu_id_string : String;

function setDAZ() : Bool8;

resourcestring
	capFPUString = 'Floating point unit';
	capVMEString = 'Virtual mode extensions';
	capDEBUGEXTString = 'Debugging extensions';
	cap4MPAGEString = 'Page size extension';
	capTSCString = 'Time stamp counter';
	capMSRString = 'Machine specific registers';
	capPAEString = 'Physical address extensions';
	capMCHKXCPString = 'Machine check exception';
	capCMPCXCHG8BString = 'CMP8XCHGB instruction support';
	capAPICString = 'On-chip APIC hardware';
	capSEPString = 'Fast system call instructions';
	capMTRRString = 'Memory type range registers support';
	capPGEString = 'Page global enable';
	capMCHKARCHString = 'Machine check architecture';
	capCMOVString = 'CMOV instruction';
	capPATString = 'Page attribute table';
	capPSE36String = '36-bit page size extension';
	capPSNString = 'Processor serial number present and enabled';
	capMMXString = 'Intel architecture MMX technology';
	capFXSRString = 'Fast floating point save and restore';
	capSSEString = 'Streaming SIMD extensions support';
	capSSEMMXString = 'MMX+ architecure';
	capEMMXString = 'Extended MMX architecture';
	cap3DNOWEXTString = 'Extended 3dNow! architecture';
	cap3DNOWString = '3dNow! architecture';
	capCPUIDString = 'CPUID instruction support';
	capCLFLSHString = 'CLFLUSH instruction support';
	capDTESString = 'Debug trace and event monitor store support';
	capACPIString = 'ACPI processor performance modulation registers support';
	capSSE2String = 'Streaming SIMD extensions 2 support';
	capSLFSNPString = 'Self snoop support';
	capLMString = 'AA-64 long mode enabled';
	capTMString = 'Thermal monitor support';
	capHTTString = 'Hyper-threading technology support';
	capIA64String = 'IA-64 mode enabled';
	capPBEString = 'Pending break event support';
	capSSE3String = 'Streaming SIMD extensions 3 support';
	capMONString = 'MONITOR/MWAIT instructions support';
	capDSCPLString = 'CPL qualified debug-store feature';
	capEISTString = 'Enhanced Intel SpeedStep technology';
	capTM2String = 'Thermal monitor 2 support';
	capCIDString = 'Context ID support';
	capXTPRString = 'Send task priority messages support';
	capEM64TString = 'Intel extended memory 64 technology support';
	capNXString = 'No execute page protection';
	capTTPString = 'Thermal trip support';
	capSYSCALLString = 'SYSCALL/SYSRET instructions support';
	capMPString = 'Multiprocessing capability';
	cap3DNOWPROString = '3DNow! Pro support';
	capBRANDIDString = 'Brand ID support';
	capDAZString = 'Denormals are Zero support';

const
	CPUCapabilities : array[low(TCPUCapabilities)..high(TCPUCapabilities)] of string = (
		capFPUString, capVMEString, capDEBUGEXTString, cap4MPAGEString, capTSCString, capMSRString, 
		capPAEString, capMCHKXCPString, capCMPCXCHG8BString, capAPICString, capSEPString, capMTRRString, 
		capPGEString, capMCHKARCHString, capCMOVString, capPATString, capPSE36String, capPSNString, 
		capMMXString, capFXSRString, capSSEString, capSSEMMXString, capEMMXString, cap3DNOWEXTString, 
		cap3DNOWString, capCPUIDString, capCLFLSHString, capDTESString, capACPIString, capSSE2String, 
		capSLFSNPString, capLMString, capTMString, capHTTString, capIA64String, capPBEString, capSSE3String, 
		capMONString, capDSCPLString, capEISTString, capTM2String, capCIDString, capXTPRString, capEM64TString, 
		capNXString, capTTPString, capSYSCALLString, capMPString, cap3DNOWPROString, capBRANDIDString, capDAZString
		);


implementation
{$IFDEF PPC_FPC}
{$IFNDEF OS_OS2}
{$IFNDEF OS_DOS}
uses
	{$IFDEF OS_WINDOWS}windows{$ENDIF}
	{$IFDEF OS_UNIX}unix{$ENDIF};
{$ENDIF OS_DOS}
{$ENDIF OS_OS2}
{$ENDIF PPC_FPC}


{ list of known official ratings of CPUs. This list MUST be sorted in
  descending CPU speed order that these ratings are correctly identified.
  To add other ratings simply insert it at a proper place and adjust the array
  size accordingly.

  You DON'T need to change the procedure which does the
  rating calculation, additional ratings are automatically detected.

  It's best to contact me to insert it in the official CPU-release...

  BTW: This list SHOULD NOT contain ratings of overclocked CPU's, this array
  is purely for informational purposes. Always use the clockrate entry of the
  CPU record instead of the rating for correct speed values

  Note that this table need not be updated anymore with higher ratings since
  this unit does CPU rating guessing based on known standard PCI clock
  frequencies.
}

const
	known_ratings : array [0..22] of Int32 = (
		0300, 0266, 0250, 0240, 0233, 0225, 0200, 0180, 0166, 0150, 0133,
		0120, 0100, 0090, 0075, 0066, 0050, 0040, 0033, 0030, 0025, 0016,
		0000);

const
	clock_multiplier : array[0..3] of Float32 = (33.333, 37.500, 41.500, 50.000);

const
	extended_family_shift = $10;

{ the timing frequency of the programming interval timer }

var
	timer_rate	: Int64 = $1234DD;

function GetCPUClockrate(family : UInt8) : Float64; forward;
function getCPUrating(const cpurec : TCPUInfo; clock : Float64) : UInt32; forward;

function FPUSupported : Bool8; assembler; pascal;
var
	_result : Bool8;
	fp_status : UInt16;
asm
	mov _result, 0
	mov fp_status, $5a5a
	fninit
	fnstsw fp_status
	mov ax, fp_status
	cmp al, 0
	jne @NoFPU

	fnstcw fp_status
	mov ax, fp_status
	and ax, $103f
	cmp ax, $3f
	jne @NoFPU
	mov _result, 1

@NoFPU:
	mov al, _result
end;

{ check if the cpuid instruction is available }
function CPUidSupported : Bool8; assembler; pascal;
asm
	pushfd
	pushfd               { Get original EFLAGS}
	pop    eax
	mov    ecx, eax
	xor    eax, 200000h { Flip id bit in eflags }
	push   eax           { Save new eflags value on stack }
	popfd                { Replace current eflags value }
	pushfd               { Get new eflags }
	pop    eax           { Store new eflags in eax }
	popfd                { Restore original eflags }
	xor    eax, ecx      { Can not toggle ID bit}
	setnz  al
end;


{ CPU detection routines
  These routines are executed when no cpuid instruction was found.
}

function Is_386 : Bool8; assembler; pascal;
asm
{ distuingish between 386/486 by examining the ability to preserve the AC bit
  of the flags register. (386 = no, 486 = yes)  }
	mov dx, sp         { save current stack pointer }
	and sp, $fffc      { align stack to avoid AC fault }
	pushfd
	pop eax
	mov ecx, eax
	xor eax, $40000    { xor AC bit in EFLAGS }
	push eax
	popfd
	pushfd
	pop eax
	xor eax, ecx       { Is AC toggled ? }
	setz al            { if not, we have a 386 }
	and sp, $fffc      { align stack to prevent AC fault }
	push ecx
	popfd              { restore original AC bit }
	mov sp, dx         { restore original stack pointer }
end;

function Is_486 : Bool8; assembler; pascal;
asm
{ Distinguish between the i486 and Pentium by the ability to set the ID flag
  in the EFLAGS register. If the ID flag is set, then we can use the CPUID
  instruction to determine the final version of the chip. Otherwise we
  simply have an 80486.}
	pushf                  { Get original EFLAGS}
	pop eax
	mov ecx, eax           { save EFLAGS }
	xor eax, $200000       { Flip ID bit in EFLAGS}
	push eax               { Save new EFLAGS value on stack}
	popf                   { Replace current EFLAGS value}
	pushf                  { Get new EFLAGS}
	pop eax                { Store new EFLAGS in EAX}
	xor eax, ecx           { Toggle ID bit}
	setz al
	push ecx              { restore old EFLAGS }
	popf
end;

procedure callCPUid(input : UInt32; var regeax, regebx, regecx, regedx : UInt32); assembler;
	pascal;
asm
	pushad
	mov eax, input
	//db 0fh, 0a2h // cpuid
	cpuid
	mov edi, regeax
	mov [edi], eax
	mov edi, regebx
	mov [edi], ebx
	mov edi, regecx
	mov [edi], ecx
	mov edi, regedx
	mov [edi], edx
	popad
end;

{ main cpu detection routine which uses all the procedures above  }

procedure GetCPUInfo(var cp : TCPUInfo);
type
	TCPUCaps = record
		cap : TCPUCapabilities;
		bit : UInt8;
	end;

	procedure getCapabilities(const capsmap : array of TCPUCaps; var caps : TCapabilities; const reg : UInt32);
	var
		l : Int32;
	begin
		for l := low(capsmap) to high(capsmap) do begin
			if ((reg and (1 shl capsmap[l].bit)) <> 0) then
				caps := caps + [capsmap[l].cap];
		end;
	end;
	
	function TrimArrayLeft(var c : array of char) : String;
	var
		i : Integer;
	begin
		i := low(c);
		while ((i <= high(c)) and (c[i] = #0)) do begin
			inc(i);
		end;
		result := '';
		while ((i <= high(c)) and (c[i] <> #0)) do begin
			result := result + c[i];
			inc(i);
		end;
	end;

	procedure getBrandIdAndString(var cp : TCPUInfo);
	var
		a, b, c, d : UInt32;
		temp : array[0..47] of Char;
	begin
		with cp do begin
			// get 8 bit brand ID
			callCPUId(baseLevel+1, a, b, c, d);
			brandId := b and $FF;
			// if brandID is zero and vendor is AMD, try the 12 bit brand ID
			if (vendor = venAMD) and (brandID = 0) then begin
				callCPUId(extLevel+1, a, b, c, d);
				brandId := b and $FFF;
			end;
			callCPUId(extLevel + 2,
				PUInt32(@temp[00])^, PUInt32(@temp[04])^, PUInt32(@temp[08])^, PUInt32(@temp[12])^);
			callCPUId(extLevel + 3,
				PUInt32(@temp[16])^, PUInt32(@temp[20])^, PUInt32(@temp[24])^, PUInt32(@temp[28])^);
			callCPUId(extLevel + 4,
				PUInt32(@temp[32])^, PUInt32(@temp[36])^, PUInt32(@temp[40])^, PUInt32(@temp[44])^);
			brandString := TrimArrayLeft(temp);
			if (brandId <> 0) then begin
				caps := caps + [capBrandID];
			end;
		end;
	end;
	
	procedure GetDAZ(var caps : TCapabilities);
	var
		scratchmem, base : PUInt32;
		mxcsr_mask : UInt32;
	begin
		// check if fxsave/fxstor and SSE of any kind available
		if (cpusupports([capFXSR, capSSE]) or cpusupports([capFXSR, capSSE2])) then begin
			// allocate and clear scratch buffer
			getmem(scratchmem, 512 + 15);
			fillchar(scratchmem^, 512 + 15, #0);
			// create 16 byte aligned pointer and execute fxsave
			base := PUInt32((PtrUInt(scratchmem) + 15) and not 15);
			asm
				mov eax, base
				fxsave [eax]
			end;
			// check mxcsr_mask
			mxcsr_mask := PUInt32(PtrUInt(base) + 28)^;
			if (mxcsr_mask = 0) then
				mxcsr_mask := $ffbf;
			if ((mxcsr_mask and $40) <> 0) then
				caps := caps + [capDAZ];
			freemem(scratchmem);
		end;
	end;


const
	// cpuid fn 1h / EDX
	All_Std1hEDXCapsMap : array[0..21] of TCPUCaps = (
		(cap : capFPU; bit : 0), (cap : capVME; bit : 1), (cap : {$IFDEF NEWCAPS}capDEBUGEXT{$ELSE}capDE{$ENDIF}; bit : 2), (cap : {$IFDEF NEWCAPS}cap4MPAGE{$ELSE}capPSE{$ENDIF}; bit : 3),
		(cap : capTSC; bit : 4), (cap : capMSR; bit : 5), (cap : capPAE; bit : 6), (cap : {$IFDEF NEWCAPS}capMCHKXCP{$ELSE}capMCE{$ENDIF}; bit : 7),
		(cap : {$IFDEF NEWCAPS}capCMPXCHG8B{$ELSE}capCX8{$ENDIF}; bit : 8), (cap : capAPIC; bit : 9), (cap : capSEP; bit : 11), (cap : capMTRR; bit : 12),
		(cap : capPGE; bit : 13), (cap : {$IFDEF NEWCAPS}capMCHKARCH{$ELSE}capMCA{$ENDIF}; bit : 14), (cap : capCMOV; bit : 15), (cap : capPAT; bit : 16),
		(cap : capPSE36; bit : 17), (cap : capCLFSH; bit : 19), (cap : capMMX; bit : 23), (cap : capFXSR; bit : 24),
		(cap : capSSE; bit : 25), (cap : capSSE2; bit : 26));

	// cpuid fn 8x1h / EDX + AMD
	AMD_Ext1hEDXCapsMap : array[0..3] of TCPUCaps = (
		(cap : capSYSCALL; bit : 11), (cap : {$IFDEF NEWCAPS}capSSEMMX{$ELSE}capMMXplus{$ENDIF}; bit : 22), (cap : {$IFDEF NEWCAPS}cap3DNOWEXT{$ELSE}capE3DNOW{$ENDIF}; bit : 30), (cap : cap3DNOW; bit : 31)
		);
	// cpuid fn 8x1h / EDX + AMD K7
	AMDK7_Ext1hEDXCapsMap : array[0..4] of TCPUCaps = (
		(cap : capSYSCALL; bit : 11), (cap : {$IFDEF NEWCAPS}capSSEMMX{$ELSE}capMMXplus{$ENDIF}; bit : 22), (cap : {$IFDEF NEWCAPS}cap3DNOWEXT{$ELSE}capE3DNOW{$ENDIF}; bit : 30), (cap : cap3DNOW; bit : 31),
		(cap : capMP; bit : 19)
		);
	// cpuid fn 8x1h / EDX + AMD K8
	AMDK8_Ext1hEDXCapsMap : array[0..5] of TCPUCaps = (
		(cap : capSYSCALL; bit : 11), (cap : capNX; bit : 20), (cap : {$IFDEF NEWCAPS}capSSEMMX{$ELSE}capMMXplus{$ENDIF}; bit : 22), (cap : capLM; bit : 29),
		(cap : {$IFDEF NEWCAPS}cap3DNOWEXT{$ELSE}capE3DNOW{$ENDIF}; bit : 30), (cap : cap3DNOW; bit : 31)
		);
	// cpuid fn 8x7h / EDX + AMD
	AMD_Ext7hEDXCapsMap : array[0..0] of TCPUCaps = (
		(cap : capTTP; bit : 3)
		);

	// cpuid fn 1h / EDX + Intel
	Intel_Std1hEDXCapsMap : array[0..7] of TCPUCaps = (
		(cap : capPSN; bit : 18), (cap : {$IFDEF NEWCAPS}capDS{$ELSE}capDTES{$ENDIF}; bit : 21), (cap : capACPI; bit : 22), (cap : {$IFDEF NEWCAPS}capSS{$ELSE}capSLFSNP{$ENDIF}; bit : 27),
		(cap : capHTT; bit : 28), (cap : capTM; bit : 29), (cap : capIA64; bit : 30), (cap : capPBE; bit : 31));
	// cpuid fn 1h / ECX + Intel	
	Intel_Std1hECXCapsMap : array[0..6] of TCPUCaps = (
		(cap : capSSE3; bit : 0), (cap : capMON; bit : 3), (cap : capDSCPL; bit : 4), (cap : capEIST; bit : 7),
		(cap : capTM2; bit : 8), (cap : capCID; bit : 10), (cap : capxTPR; bit : 14)
		);
	// cpuid fn 8x1h / EDX + Intel
	Intel_Ext1hEDXCapsMap : array[0..1] of TCPUCaps = (
		(cap : capNX; bit : 20), (cap : capEM64T; bit : 29)
		);
		
	Cyrix_Ext1hEDXCapsMap : array[0..20] of TCPUCaps = (
		(cap : capFPU; bit : 0), (cap : capVME; bit : 1), (cap : {$IFDEF NEWCAPS}capDEBUGEXT{$ELSE}capDE{$ENDIF}; bit : 2), (cap : {$IFDEF NEWCAPS}cap4MPAGE{$ELSE}capPSE{$ENDIF}; bit : 3),
		(cap : capTSC; bit : 4), (cap : capMSR; bit : 5), (cap : capPAE; bit : 6), (cap : {$IFDEF NEWCAPS}capMCHKXCP{$ELSE}capMCE{$ENDIF}; bit : 7),
		(cap : {$IFDEF NEWCAPS}capCMPXCHG8B{$ELSE}capCX8{$ENDIF}; bit : 8), (cap : capAPIC; bit : 9), (cap : capSEP; bit : 11), (cap : capMTRR; bit : 12),
		(cap : capPGE; bit : 13), (cap : capMCA; bit : 14), (cap : capCMOV; bit : 15), (cap : capPAT; bit : 16),
		(cap : capPSE36; bit : 17), (cap : capMMX; bit : 23), (cap : capMMXplus; bit : 24), (cap : capE3DNOW; bit : 30),
		(cap : cap3DNOW; bit : 31)
	);

var
	a, b, c, d : UInt32;
	i : Int32;
begin
	fillchar(cp, sizeof(cp), 0);
	with cp do begin
		if (FPUSupported) then
			caps := caps + [capFPU];
		if (CPUidSupported) then begin
			caps := caps + [capCPUid];
			// get maximum supported CPUid level
			baseLevel := $00000000;
			callCPUid(baseLevel, maxBaseLevel, b, c, d);

			// get vendor id string
			SetLength(vendorIdString, 12);
			PUInt32(@vendorIdString[1])^ := b;
			PUInt32(@vendorIdString[5])^ := d;
			PUInt32(@vendorIdString[9])^ := c;

			// Compare with strings in available list - and get cpu vendor
			i := ord(venAMD);
			while (i < ord(high(TCPUVendor))) and
				(vendorIdStrings[TCPUVendor(i)] <> vendorIdString) do inc(i);

			vendor := TCPUVendor(i);

			// get type, family, model and stepping values
			callCPUid(baseLevel + 1, a, b, c, d);

			// cpu type not available when using extended calls
			if (baseLevel = 0) then
				cpuType := TCPUType((a shr 12) and $3);
			family := TCPUFamily((a shr 8) and $F);
			// check for extended family, encode extended family
			// in family by adding an offset
			if (vendor = venIntel) and (family = $F) then
				family := TCPUFamily((a shr 20) and $FF) + extended_family_shift;
			model := TCPUModel((a shr 4) and $F);
			// check for extended model
			if (vendor = venIntel) and (model = $F) then
				model := TCPUModel((a shr 16) and $F);

			stepping := TCPUStepping(a and $F);

			// now check if we can use the extended CPUid levels
			// only Cyrix or Centaur processors with family >= 5
			// (_not_ AMD or Cyrix' 5x86 !!). AMD family 5 model 1
			// does not support it either
			if ((vendor = venAMD) and (family = 5) and (model >= 1)) or
				((vendor in [venAMD, venCyrix, venCentaur]) and (family >= 5)) or
				(vendor = venIntel) then begin
				extLevel := UInt32($80000000);
				callCPUid(extLevel, maxExtLevel, b, c, d);
				maxExtLevel := maxExtLevel;

				if (vendor <> venIntel) and (maxExtLevel >= UInt32($80000001)) then begin
					// get type, family, model and stepping values again, with extended
					// calls this time
					callCPUid(extLevel + 1, a, b, c, d);
					family := TCPUFamily((a shr 8) and $F);
					model := TCPUModel((a shr 4) and $F);
					stepping := TCPUStepping(a and $F);
				end;
			end;

			// some very old Intel Pentium return bad values on the maximum CPUID level
			// so set their maximum level to 1 (we do not check for cache size anyway)
			// pre-step B0 P5's return a max level of $000005xx too
			if (vendor = venIntel) and (model=5) and
				((maxBaseLevel < 1) or (maxBaseLevel and $F00 = $500)) then begin
				maxBaseLevel := 1;
				
				extLevel := 0;
				maxExtLevel := 0;
			end;

			// get CPU capabilities
			if (maxBaseLevel >= 1) then begin

				callCPUId(baseLevel + 1, a, b, c, d);
				getCapabilities(All_Std1hEDXCapsMap, caps, d);
				
				case vendor of
					venAMD : begin
						callCPUId(extLevel + 1, a, b, c, d);
						getCapabilities(AMD_Ext1hEDXCapsMap, caps, d);
						if (family = 7) then begin
							getCapabilities(AMDK7_Ext1hEDXCapsMap, caps, d);
						end else if (family = 15) then begin
							getCapabilities(AMDK8_Ext1hEDXCapsMap, caps, d);
						end;
						if (maxExtLevel >= (extLevel + 7)) then begin
							callCPUid(extLevel + 7, a, b, c, d);
							getCapabilities(AMD_Ext7hEDXCapsMap, caps, d);
						end;
					end;
					venCentaur : begin
						callCPUId(extLevel + 1, a, b, c, d);
						getCapabilities(AMD_Ext1hEDXCapsMap, caps, d);
					end;
					venCyrix : begin
						callCPUId(extLevel + 1, a, b, c, d);
						getCapabilities(Cyrix_Ext1hEDXCapsMap, caps, d);
					end;
					venIntel : begin
						getCapabilities(Intel_Std1hEDXCapsMap, caps, d);
						getCapabilities(Intel_Std1hECXCapsMap, caps, c);
						if (maxExtLevel >= (extLevel + 1)) then begin
							callCPUId(extLevel + 1, a, b, c, d);
							getCapabilities(Intel_Ext1hEDXCapsMap, caps, d);
						end;
					end;
				end;

				if (caps * [capSSE, cap3DNow] = [capSSE, cap3DNow]) then begin
					caps := caps + [cap3DNOWPro];
				end;
				// get brand ID and string
				if (maxExtLevel >= (extLevel + 4)) then begin
					getBrandIdAndString(cp);
				end;
				// get misc other info (Intel only)
				if (vendor = venIntel) then begin
					cpuCount := (b and $00FF0000) shr 16;
					// get DAZ capability
					getDAZ(caps);
					// don't care about other stuff... (in this release)
				end;
			end;

			// On Intel processors check the SEP capability for validity
			// since PPro with model = 1 does report this one incorrectly
			if (vendor = venIntel) and (family = 6) and (model < 3) and
				(stepping < 3) then caps := caps - [capSEP];

			// get PSN if available
			if (capPSN in caps) then begin
				callCPUid(baseLevel + 1, psn[0], b, c, d);
				callCPUid(baseLevel + 3, a, b, psn[1], psn[2]);
			end;

			// check if the OS supports ISSE-3
			if (capSSE2 in caps) then begin
				try
					asm
						// haddpd %xmm2, %xmm1
						db $66, $0f, $7c, $ca
					end;
				except
					on Exception do caps := caps - [capSSE3];
				end;
			end;
			// check if the OS supports ISSE-2
			if (capSSE2 in caps) then begin
				try
					asm
						// paddq %xmm7, %xmm6
						db $66, $0f, $d4, $fe
					end;
				except
					on Exception do caps := caps - [capSSE2];
				end;
			end;
			// check if the OS supports ISSE
			if (capSSE in caps) then begin
				try
					asm
						// movaps %xmm7, %xmm6
						db $0f, $28, $fe
					end;
				except
					// only check if _any_ exception occurs - else we'd need lots of IFDEF's for
					// the different targets since Invalid Opcode exception triggers different
					// errors on different platforms (if any)
					on Exception do caps := caps - [capSSE];
				end;
			end;
			
		end else begin

			if (Is_386) then family := 3 else
			if (Is_486) then family := 4 else
			family := 5;
			vendor := venUnknown;
			model := $ff;
			stepping := $ff;
		end;

		clockrate := getCPUClockrate(family);
		rating := getCPURating(cp, clockrate);
	end;
end;

{$IFDEF OVERLOAD}
function CPUSupports(const which : TCapabilities) : Bool8; overload;
begin
	result := (which * this_cpu.caps) = which;
end;
{$ENDIF}

function CPUSupports(const which : TCPUCapabilities) : Bool8; {$IFDEF OVERLOAD}overload;{$ENDIF}
begin
	result := which in this_cpu.caps;
end;

function CPUSupports2(const which : TCapabilities) : Bool8;
begin
	result := (which * this_cpu.caps) = which;
end;

function GetCPUString(const cpurec : TCPUInfo) : String;
const
	AMDBrandTable : array[4..6, 0..$f] of string = (
		('', '', '', '80486DX2', '', '', '', '80486DX2 Write back enhanced', '80486DX4', '80486DX4 Write back enhanced', '', '', '', '', '5x86', '5x86 Write back enhanced'),
		('SSA5 (PR75, PR90, PR100)', '5k86 (PR120, PR133)', '5k86 (PR166)', '5k86 (PR200)', '', '', 'K6 (0.30µm)', 'K6 (0.25µm)', 'K6-2', 'K6-III', '', '', '', 'K6-II+ or K6-III+', '', ''),
		('Athlon (0.25µm)', 'Athlon (0.18µm)', 'Duron (SF Core)', 'Athlon (TB Core)', '', 'Athlon (PM Core)', 'Athlon (MG Core)', 'Athlon (TH Core)', '', '', '', '', '', '', '', '')
		);
	CentaurBrandTable : array[5..5, 4..9] of string = (
		('C6', '', '', '', 'C2', 'C3')
		);
	CyrixBrandTable : array[4..6, 2..9] of string = (
		('', '', '', '', '', '', '', '5x86'),
		('6x86', '', 'MediaGX, GXm', '', '', '', '', ''),
		('6x86MX', '', '', 'Cyrix M2 core', 'WinChip C5A core', 'WinChip C5B/C core', 'Winchip C5C-T core', '')
		);
	IntelBrandTable : array[4..7, 0..$f] of string = (
		('i80486DX', 'i80486DX', 'i80486SX', 'i80486DX2', 'i80486SL', 'i80486SX2', 'i80486DX2 Write back enhanced', 'i80486DX4', 'i804864 Write back enhanced', '', '', '' ,'' ,'', '', ''),
		('Pentium A-Step', 'Pentium', 'Pentium 54C', 'Pentium 54T OverDrive', 'Pentium 55C', '', '', 'Pentium 54C', 'Pentium 55C (0.25µm)', '', '', '' ,'' ,'', '', ''),
		('Pentium Pro A-Step', 'Pentium Pro', '', 'Pentium II (0.28µm)', '', 'Pentium II (0.25µm)', 'Celeron A (Mendocino)', 'Pentium III (0.25µm) ', 'Pentium III (0.18µm) 256kB Cache', 'Pentium M', 'Pentium III Xeon (0.18µm)', 'Pentium III (0.13µm)', '' ,'Pentium M' ,'', ''),
		('Itanium (iA64)', '', '', '', '', '', '', '', '', '', '', '' ,'' ,'', '', '')
		);
	NexGenBrandTable : array[5..5, 0..0] of string = (
		('Nx586 or Nx58FPU')
		);
	RiseBrandTable : array[5..5, 0..2] of string = (
		('mP6 (0.25µm)', '', 'mP6 (0.18µm)')
		);
	UMCBrandTable : array[4..4, 1..2] of string = (
		('U5D', 'U5S')
		);
	TransmetaBrandTable : array[5..5, 4..4] of string = (
		('Crusoe TM3x00 or TM5x00')
		);

	IntelExtBrandTable : array[0..1, 0..3] of string = (
		('Pentium 4 (0.18µm)', 'Pentium 4 (0.18 µm)', 'Pentium 4 (0.13 µm)', 'Pentium 4 (0.09 µm'),
		('Itanium 2 (iA64)', '', '', '')
		);
	AMDExtBrandTable : array[5..7, 1..$d] of string = (
		('5k86 (PR120 or PR133)', '5k86 (PR166)', '5k86 (PR200)', '', '', '', '', '', '', '', '', '', ''),
		('', '', '', '', '', 'K6 (0.30 µm)', 'K6 (0.25 µm)', ' K6-2', 'K6-III','','','','K6-II+ or K6-III+'),
		('Athlon (0.25µm)', 'Athlon (0.18µm)', 'Duron (SF core)', 'Athlon (TB core)', 'Athlon (PM core)', 'K7 Duron (MG core)', 'K7 Athlon (TH core)', '', '', '', '', '', ''));
	CentaurExtBrandTable : array[5..5, 8..9] of string = (
		('C2', 'C3'));
	CyrixExtBrandTable : array[6..6, 5..8] of string = (
		('Cyrix M2 Core', ' WinChip C5A core', 'WinChip C5B/C core', 'WinChip C5C-T core')
		);

var
	tableString : String;

begin
	with cpurec do begin
		if (brandString <> '') then begin
			result := brandString;
		end else begin
			{$R+}
			try
				case vendor of
					venAMD :
						if (extlevel > 0) then
							tableString := AMDExtBrandTable[family, model]
						else
							tableString := AMDBrandTable[family, model];
					venCentaur :
						if (extLevel > 0) then
							tableString := CentaurExtBrandTable[family, model]
						else
							tableString := CentaurBrandTable[family, model];
					venCyrix :
						if (extlevel > 0) then
							tableString := CyrixExtBrandTable[family, model]
						else
							tableString := CyrixBrandTable[family, model];

					venIntel :
						if (family > extended_family_shift) then
						tableString := IntelExtBrandTable[family - extended_family_shift, model]
						else
							tableString := IntelBrandTable[family, model];
					venNexGen :
						tableString := NexGenBrandTable[family, model];
					venRise :
						tableString := RiseBrandTable[family, model];
					venUMC :
						tableString := UMCBrandTable[family, model];
					venTransmeta :
						tableString := TransmetaBrandTable[family, model];
					else
						tableString := '';
				end;
			except
				on ERangeError do tableString := '';
			end;
			{$R-}
			if (tableString = '') then
				tableString := 'unknown';
			result := CPUVendors[cpurec.vendor] + ' ' + tableString + ' processor';
		end;
	end;
end;


{ return the contents of the time stamp counter }

procedure GetTSCValue(var val : Int64); assembler; pascal;
asm
	push eax
	push edx
	push edi
	db 0fh, 31h // rdtsc
	mov edi, VAL
	mov [edi+0], eax
	mov [edi+4], edx
	pop edi
	pop edx
	pop eax
end;

{ return the current time value }
{$IFDEF OS_WINDOWS}
function GetPITValue(var val : Int64) : Bool32; stdcall;
external 'kernel32.dll' name 'QueryPerformanceCounter';
{$ENDIF}
{$IFDEF OS_DOS}
function GetPITValue(var val : Int64) : Bool32; assembler; pascal;
asm
	xor eax, eax
	cli
	out $43, al
	mov edx, fs:[$46c]
	in al, $40
	db $eb, 0, $eb, 0, $eb, 0
	mov ah, al
	in al, $40
	db $eb, 0, $eb, 0, $eb, 0
	xchg al, ah
	neg ax
	movzx edi, ax
	sti

	mov ebx, $10000
	mov eax, edx
	xor edx, edx
	mul ebx
	add eax, edi
	adc edx, 0

	push edx
	push eax
	mov ecx, $82bf1000
	movzx eax, word ptr fs:[$470]
	mul ecx
	mov ecx, eax
	pop eax
	pop edx

	add eax, ecx
	adc edx, 0

	mov edi, val
	mov [edi], eax
	mov [edi+4], edx
end;
{$ENDIF}
{$IFDEF OS_UNIX}
function GetPITValue(var val : Int64) : Bool32;
var
	tv : timeval;
begin
	fpGetTimeOfDay(@tv, nil);
	val := tv.tv_sec * timer_rate + tv.tv_usec;
end;
{$ENDIF}
{$IFDEF OS_OS2}
function DosTmrQueryFreq(var Freq:longint):longint; cdecl;
external 'DOSCALLS' index 362;
function DosTmrQueryTime(var Time:comp):longint; cdecl;
external 'DOSCALLS' index 363;

function GetPITValue(var val : Int64) : Bool32;
begin
	DosTmrQueryTime(val);
end;
{$ENDIF}

{ measure CPU speed by means of the rdtsc instruction }

function GetTSCSpeed : Float64;
const
	MAX_TRIES = 20; { number of maximume tries issued }

	TOLERANCE = 0.1; { the tolerance in Mhz three subsequent measurement results may differ }
var
	freq : array[0..2] of Float64; { storage for the 3 subsequent measurements }
	time0, time1, { start and endtime (PIT values) }
	stamp0, stamp1, { start and endcycles (rdtsc values) }
	cycles : Int64;
	ticks : Float64; { used for frequency calculation }
	total : Float64; { total amount of frequency calculated from the 3 subsequent runs }
	dummy,
	tries : UInt32; { current number of tries }
	{$IFDEF PPC_FPC}
	{$IFDEF OS_WINDOWS}
	oldPriorityClass : UInt32;
	{$ENDIF}
	{$ENDIF}

begin
	{ initialize variables }
	for dummy := 0 to 2 do
		freq[dummy] := 0.0;
	tries := 0;
	time0 := 0; time1 := 0; stamp0 := 0; stamp1 := 0;
	cycles := 0; ticks := 0; total := 0;
	repeat
		{ increment number of tries }
		tries := tries + 1;

		{ rotate frequency values in storage }
		freq[2] := freq[1];
		freq[1] := freq[0];
		{$IFDEF PPC_FPC}
		{$IFDEF OS_WINDOWS}
		// maximize priority for most exact results
		oldPriorityClass := GetPriorityClass(GetCurrentProcess);
		SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
		{$ENDIF}
		{$ENDIF}

		{ this accounts for overhead later }
		getPITValue(time0);
		time1 := time0;
		while ((time1-time0) < 50) do begin
			getPITvalue(time1);
			getTSCvalue(stamp0);
		end;
		{ here's the actual measurement }
		time0 := time1;
		{ time for about 10000 cycles }
		while ((time1-time0) < 10000) do begin
			getPITvalue(time1);
			getTSCvalue(stamp1);
		end;

		{$IFDEF PPC_FPC}
		{$IFDEF OS_WINDOWS}
		// restore old priority
		SetPriorityClass(GetCurrentProcess, oldPriorityClass);
		{$ENDIF}
		{$ENDIF}

		cycles := stamp1 - stamp0;

		{ calculate frequency }
		ticks := time1-time0;
		ticks := ((ticks) * 1000000) / TIMER_RATE;
		freq[0] := cycles / ticks;
		total := freq[0] + freq[1] + freq[2];

		{ if we've made enough tries, exit }
		if (tries > MAX_TRIES) then
			break;
		{ loop until three subsequent measurements are within tolerance }
	until ((abs((3*freq[0])-total) <= TOLERANCE) and
		(abs((3*freq[1])-total) <= TOLERANCE) and
		(abs((3*freq[2])-total) <= TOLERANCE));
	{ return speed value }
	getTSCspeed := total / 3;
end;

{ measure CPU speed by means of timing the execution of known loop }

function GetBSFSpeed(cycles : UInt32) : Float64;

const
	SAMPLINGS = 10;   { number of times the loop is timed at one run }
	ITERATIONS = 4000;

var
	t0, t1 : Int64;     { start and endtime }
	lowest : Int64;            { lowest time measured }
	ticks : Float64;
	current : Int64;
	i : UInt32;
	{$IFDEF PPC_FPC}
	{$IFDEF OS_WINDOWS}
	oldPriorityClass : UInt32;
	{$ENDIF}
	{$ENDIF}

begin
	{$IFDEF PPC_FPC}
	{$IFDEF OS_WINDOWS}
	oldPriorityClass := GetPriorityClass(GetCurrentProcess);
	SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
	{$ENDIF}
	{$ENDIF}
	{ initialize lowest to a huge value }
	lowest := $7fffffff;
	{ time loop }
	for i := 0 to SAMPLINGS-1 do begin
		getPITvalue(t0);
		{ the timed loop }
		asm {&alters eax, ecx, edx}
			mov eax, 80000000h
			mov ecx, iterations
		@BSFLoop:
			bsf edx, eax
			dec cx
			jnz @BSFLoop
		end;
		getPITvalue(t1);
		current := t1 - t0;
		if (current < lowest) then lowest := current;
	end;
	{$IFDEF PPC_FPC}
	{$IFDEF OS_WINDOWS}
	SetPriorityClass(GetCurrentProcess, oldPriorityClass);
	{$ENDIF}
	{$ENDIF}
	{ scale the PIT frequency to microsecond values }
	ticks := ((lowest * 1000000) / TIMER_RATE);
	result := (cycles * ITERATIONS) / ticks;
end;


{ the clock-rate routine which chooses between the above two methods and
  averages about some measurements
}

function getCPUclockrate(family : UInt8) : Float64;

const
	TIMING_RUNS = 5;       { the number of times timing is done }
	{ the number of cycles needed for the different CPU types for the
	non-rdtsc speed detection }
	bsf_cycles : array[3..6] of UInt32 =
		(115, 47, 43, 38);
var
	times : Int32;
	avgspeed : Float64;
	speed  : Float64;
	use_rdtsc : Bool8;
begin
	avgspeed := 0;
	use_rdtsc := CPUSupports(capTSC);
	for times := 1 to TIMING_RUNS do begin
		if (use_rdtsc) then speed := getTSCspeed
		else speed := getBSFspeed(bsf_cycles[family]);
		avgspeed := avgspeed + speed;
	end;
	result := (avgspeed / TIMING_RUNS);
end;


{ cpu rating detection }

function getCPUrating(const cpurec : TCPUInfo; clock : Float64) : UInt32;
var
	i : Int32;
	prev_abs : Int32;
	nearest_multiplier : Int32;
	cur_frac,
	prev_frac : Float32;
begin
  nearest_multiplier := 0;
	if (cpurec.family >= 5) then begin
		prev_frac := 1.0;
		for i := low(clock_multiplier) to high(clock_multiplier) do begin
			cur_frac := frac(clock / clock_multiplier[i]);
			if ((1.0 - cur_frac) < cur_frac) then cur_frac := 1.0 - cur_frac;
			if (cur_frac < prev_frac) then begin
				prev_frac := cur_frac;
				nearest_multiplier := i;
			end;
		end;
		result := round(clock_multiplier[nearest_multiplier] * round(clock / clock_multiplier[nearest_multiplier]));
	end else begin
		prev_abs := abs(known_ratings[0]-trunc(clock));
		i := 1;
		while (prev_abs > abs(known_ratings[i] - trunc(clock))) do begin
			prev_abs := abs(known_ratings[i] - trunc(clock));
			i := i + 1;
		end;
		result := (known_ratings[(i-1)]);
	end;
end;

function getPSNstring(ser : TCPUPSN) : string;

var
	i : Int32;
	s : string;
begin
	s := '';
	for i := 0 to 2 do
		s := s + IntToHex(hi(ser[i]), 4) + '-' + IntToHex(lo(ser[i]), 4) + '-';
	delete(s, length(s), 1);
	result := s;
end;


procedure TTimer.SetScale(newscale : Int64);
begin
	if (newscale > 1) then
		mScale := newScale;
end;

procedure TTimer.SetAutoReset(newreset : Bool32);
begin
	mAutoreset := newreset;
end;

constructor TTimer.Create;
begin
  mScale := 1;
end;

procedure TTimer.Reset;
begin
	mTiming := false;
	mStartTime := 0;
	mElapsedTime := 0;
end;

function TTimer.Start : Int64;
begin
	if (mTiming) then
		raise ETimer.Create('TTimer::Start');
	mTiming := true;
	mStartTime := ReadTime;
	result := CalcRealTime(mElapsedTime);
end;

function TTimer.Stop : Int64;
var
	temp : Int64;
begin
	temp := ReadTime;
	if (not mTiming) then
		raise ETimer.Create('TTimer::Stop');
	mElapsedTime := mElapsedTime + temp - mStartTime;
	mStartTime := 0;
	result := CalcRealTime(mElapsedTime);
	if (mAutoreset) then
		Reset;
end;

function TTimer.Count : Int64;
begin
	result := CalcRealTime(mElapsedTime);
end;

function TTimer.Lap : Int64;
begin
	if (mTiming) then
		result := CalcRealTime(mElapsedTime + ReadTime - mStartTime)
	else
		result := CalcRealTime(mElapsedTime);
end;

function TTimer.IsTiming : Bool32;
begin
	result := mTiming;
end;

procedure TTimer.Delay(delaytime : Int64);
var
	rawtime : Int64;
	temp : Int64;
begin
	if (mTiming) then
		raise ETimer.Create('TTimer::Delay');
	if (mAutoReset) then
		mElapsedTime := 0;
	rawtime := CalcRawTime(delaytime);
	temp := ReadTime + rawtime;
	while (ReadTime <= temp) do ;
	mElapsedTime := mElapsedTime + rawtime;
end;

procedure TTimer.WaitFor(waittime : Int64);
var
	temp, rawtime : Int64;
begin
	if (not mTiming) then
		raise ETimer.Create('TTimer::WaitFor');
	rawtime := CalcRawTime(waitTime);
	temp := mElapsedTime + mStartTime + rawtime;
	while (readtime < temp) do ;
	Stop;
end;

function TTimer.Pause : Int64;
begin
	if (mTiming) then
		result := Stop
	else
		result := Start;
end;

{****************************************************************************}
{                                                                            }
{ TZenTimer Timer Class                                                      }
{                                                                            }
{ - timing resolution of 1e-6  second (platform independant)                 }
{                                                                            }
{****************************************************************************}

//ReadTime funcions

function ReadTimeTSC: Int64;
begin
  GetTSCValue(Result);
end;

function ReadTimePIT: Int64;
begin
  GetPITValue(Result);
end;

function TZenTimer.CalcRealTime(const rawtime : Int64) : Int64;
begin
	result := round((rawTime * 1000000) / (scale * mRate));
end;

function TZenTimer.CalcRawTime(const realtime : Int64) : Int64;
begin
	result := round(realTime * mRate * resolution);
end;

constructor TZenTimer.Create;
begin
  inherited Create;

  if cpusupports(capTSC) then
  begin
    mRate := this_cpu.clockrate * 1000000;
    ReadTime := @ReadTimeTSC;
  end
  else
  begin
    mRate := timer_rate;
    ReadTime := @ReadTimePIT;
  end;
end;

function TZenTimer.Resolution : Float64;
begin
	result := 1e-6 * scale;
end;

{****************************************************************************}
{                                                                            }
{ TSysTimer Timer Class                                                      }
{                                                                            }
{ - timing resolution of 1/100 second (platform dependant)                   }
{                                                                            }
{****************************************************************************}

function ReadTimeSys: Int64;
var
  hour, minute, second, sec100 : UInt16;
begin
  DecodeTime(Now, hour, minute, second, sec100);
  Result := (sec100 + (second + (minute + hour * 60) * 60) * 100);
end;

function TSysTimer.CalcRealTime(const rawtime : Int64) : Int64;
begin
	result := round(rawTime / scale);
end;

function TSysTimer.CalcRawTime(const realtime : Int64) : Int64;
begin
	result := realTime * scale;
end;

constructor TSysTimer.Create;
begin
  inherited;
  ReadTime := @ReadTimeSys;
end;

function TSysTimer.Resolution : Float64;
begin
	result := 1e-2 * scale;
end;


{ RTL CPU unit compatibility functions }
{$IFDEF PPC_FPC}
function cpuid_support : Bool8;
begin
	result := CPUSupports(capCPUid);
end;

function floating_point_emulation : Bool8;
begin
	result := (cr0 and $4) <> 0;
end;

function cr0 : Int32; assembler;
asm
	db $0f, $20, $c0 // movl cr0, %eax
end;
{$ENDIF}
var
	delaytimer : TTimer;

procedure Delay(ms : UInt32);
begin
	if (Assigned(delayTimer)) then
		delayTimer.Delay(ms * 1000);
end;

function Clock : Int64;
begin
	result := delayTimer.Lap;
end;

function cpu_id_string : String;
begin
	result := 'CPU release ' + IntToStr(cpu_version) + ' revision ' + 
		IntToStr(cpu_revision) + ' ' + cpu_build_type + 
		' build (' + cpu_build_date + ')';
end;

function setDAZ() : Bool8;
var
	mxcsr_reg : UInt32;
begin
	if (cpuSupports([capDAZ])) then begin
		asm
			stmxcsr mxcsr_reg
			or mxcsr_reg, $40
			ldmxcsr mxcsr_reg
		end;
		result := true;
	end else begin
		result := false;
	end;
end;

procedure cpu_exitproc;
begin
	delayTimer.Stop;
	if (Assigned(delayTimer)) then
		delayTimer.Free;
end;

{$IFDEF OS_WINDOWS}
function QueryPerformanceFrequency(var frequency : Int64) : Bool32; stdcall;
external 'kernel32.dll' name 'QueryPerformanceFrequency';
{$ENDIF}

initialization

begin
	{$IFDEF OS_WINDOWS}
	QueryPerformanceFrequency(timer_rate);
	{$ENDIF}
	{$IFDEF OS_DOS}
	{ brute force PIT reset }
	asm
		mov eax, $34
		out $43, al
		xor eax, eax
		out $40, al
		out $40, al
	end;
	{$ENDIF}
	{$IFDEF OS_UNIX}
	timer_rate := 1000000;
	{$ENDIF}
	{$IFDEF OS_OS2}
	DosTmrQueryFreq(timer_rate);
	{$ENDIF}

	{ get cpu info for standard cpu record }
	getCPUInfo(this_cpu);
	{$IFDEF DEBUG}
	if (CPUSupports(capTSC)) then
		Writeln(stderr, 'CPU : Using TSC for microsecond timing')
	else
		Writeln(stderr, 'CPU : Using PIT for microsecond timing');
	Writeln(stderr);
	{$ENDIF}
	delayTimer := TZenTimer.Create;
	delayTimer.Autoreset := false;
	delayTimer.Start;
end;

finalization

begin
	cpu_exitproc;
end;

end.

{
  13-12-1998 Initial version (start of history)
  - still dissatisfied
        . Intel CPU detection
        . AMD CPU detection
        . NexGen CPU detection
        . UMC CPU detection
        . KNI detection
  - still not done
        . default cpu record and some funcs using it

  14-12-1998 Some patches applied

  15-12-1998 More bugfixing
  - did default cpu record and function to easily check different bits
  - fixed bug with multiple zentimers running at the same time
  - applied a fix for very fast processor concerning CPU detection
  - made Intel, AMD, NexGen, UMC detection
  - renamed capability constants a bit
  - KNI detection is still a guess
  - PPro is a PII model 0 or 1, there's no difference in family anymore
  - added 500 and 433 Mhz to known ratings

  16-12-1998 Getting mad with the Comp type :(
  - CPU speed detection doesn't work with DOS 7.10 (e.g. crashes)

  17-12-1998 Final bug removal to make first official beta
  - KNI detection is right(?) now
  - added 366 Mhz to known ratings
  - now works under DOS too
  - added unit header

  18-12-1998 More bug fixing to make beta 2
  - added cpu string returned by cpuid function 0 to cpu_rec
  - fixed Cyrix detection (PII have the same behaviour which I tested for
    as all Cyrix-processors)
  - better cpu-family handling
  - KNI detection is right !!!
  - added a $ASMMODE ATT at implementation start
  - fixed bug which was introduced by a Win9x (or FPC, DPMI-extender) bug :)

  19-12-1998 Bugfixing for beta 3
  - added 333 Mhz to known ratings
  - cpu speed detection on non-rdtsc systems tested
  - some more comments in the code (lots)
  - fixed eMMX detection
  - some additional code cleanup

  20-12-1998 some more finetuning (beta4)
  - replaced the round() calls by trunc() calls to have better accuracy (?)
  - 3Dnow! detection verified
  - different manufacturer handling
  - cpu speed now measured in 1/100 Mhz

  21-12-1998 undid some work made in beta4 -> beta4a
  - cpu speed in whole Mhz again (didn't like it)

  01-01-1999 beta5 (because of lack of docs)
  - changed cpu rating detection code, it works now on absolute differences
  between computed clock and ratings, not comparing computed clock and rating
  directly anymore

  05-01-1999 some additions (->beta5a)
  - added some output to stderr when DEBUG is defined

  29-01-1999 added some notes (beta6)
  - DOSEmu doesn't allow RDTSC and PIT access (from AlexS@Freepage.de)
  - added some new CPU ratings (reported from AlexS@Freepage.de)
  - note: I think it's about time to write a real doc.... ;(

  06-02-1999 header modifications
  - included License notice in the header

  23-03-1999 added serial no. detection
  - added capability for processor serial number enabled
  - added sernum field in processor record
  - added string return function as Intel recommends

  26-03-1999 changed docs
  - doesn't work with 0.99.10, requires 0.99.11

  30-03-1999 cleanup
  - removed some $ASMMODE defines
  - cyrix detection code fixed again (from marcov@stack.nl)
  - new constant: cHAVE_ISSE equals cHAVE_KNI (I do however prefer KNI)

  09-05-1999 adaption to new assembler
  - start of -""-
  - fixed cyrix cpuid detection (e.g. a 'movl $0, %eax' was missing)
  - still doesn't work with latest snapshot due to compiler bug

  12-05-1999 rtl compatibility
  - added the 3 functions available with the RTL cpu unit for completeness
  - made a new mmx unit which replaces the RTL mmx unit too

  17-05-1999 extended timer object
  - autoreset flag to automatically reset the timer on finishing a delay() or
  waitfor() method
  - scale the resolution via the units variable in the object
  - waitfor() method waits until the specified amount of time elapsed
  - get_time_stamp() to get some sort of unique 64 bit number which may be used
  as id tag
  - delay() procedure replacement
  - extended CPUTEST program to show the new features of the timer objects

  20-08-1999 win32 version
  - finished win32 port of the cpu unit, not very much to do

  26-08-1999 linux, OS/2 version
  - finished linux and OS/2 port of cpu unit, also allowing the memory unit to
  work.
  - ULZTimer only for GO32V2
  - updated e-mail address

  27-08-1999 fixed linux version
  - linux.gettime overrided dos.gettime which was meant to be used.

  04-09-1999 implemented suggestions from Lee John (LeeJ@logica.com)
  - fixed wrong spelling of 'milliseconds' in example and unit
  - some problems with 25 lines display solved

  06-11-1999 some additions
  - added quite a few new processor clock rates (from AlexS@Freepage.de)
  - added detection for Rise Technologies' processors

  02-01-2000 fixed arithmetic overflow
  - arithmetic overflow in get_cpu_rating() fixed. It was due to wrong parameters (e.g. FPC
  dislikes abs() function with UInt32s...)

  10-03-2000 docs, rel1rev0 release
  - added documentation for the unit (cpu.txt)
  - updated version to rel1rev0 because of docs

  14-03-2000 fixed FPU detection code, maintenance
  - FPU detection code was missing the check for validity of FP control word (from
  paradice@xtra.co.nz)
  - removed obsolete loops to avoid RTEs (problem from 09/05/1999)

  15-03-2000 finished Timer classes (rel2beta0)
  - TTimer is now mostly abstract
  - added TZenTimer (= former ZTimer) and TSysTimer (= former Timer) classes

  17-03-2000 fixed delay (rel2beta1)
  - fixed delay() method of Timer class which didn't take already elapsed time into account
  - similar problem with WaitTime() method
  - added credits notice to docs
  - completely new cpu detection API. Much better and more extendable now
  - more accurate speed detection with much less tolerance (1 Mhz -> 0.1 Mhz)

  18-03-2000 added critical section code, fixes (rel2beta2)
  - some Pentium processors don't report maximum cpuid level correctly - fixed
  - critical section code for Win32 target

  21-03-2000 fixed linux code (rel2beta3)
  - RTL gettimeofday() syntax changed, now 0.99.14+ only
  - some more ISSE checks

  25-03-2000 again linux code problem (rel2beta4)
  - changed gettimeofday() again
  - fixed detection of AMD extended CPUid functions

  31-03-2000 code changes (rel2beta5)
  - changed getBSFSpeed() and getTSCSpeed() to be more like the Intel original code. Returns
  nearly the same CPU speed tick count as the Linux OS does (seems to be same function, never
  looked)
  - tested cpu unit under really very heavy load (Win32) .. expectedly o.k.

  28-04-2000 packaging for (rel2beta6)
  - added LGPL license text to zip
  - source code cleanup
  - added inlining compiler switch (reported by LeeJ@logica.com)

  14-05-2000 docs creation, fixes
  - VIA bought Cyrix -> printable string changed
  - VendorIdStrings are of type TVendorIdString now
  - removed maxLevel and baseLevel members out of TCPUInfo record
  - more accurate delay
  - resolution returned is now modified by the scale
  - fixed TTimer::reset() - should not reset the timers' starting time
  - if TTimer::stop() were called when not currently timing some timer internal values
  screwed up - fixed
  - TTimer::start() does not overwrite previous aquired start time when called now

  15-05-2000 SEP bugfix (rel2rev1)
  - PPro model 1 report SEP availability incorrectly, workaround
  - CPUid instruction fix for old Pentium only, probably causes crashes on earlier processor
  technology

  16-05-2000 GetCPUidString fix (rel2rev2)
  - venUnknown crashed the getCPUidString (reported by Dirk Verwebe (dirk@verwebe.de))
  - added new processor ratings (8.5 x 66/133 Mhz) now all available x86 frequencies should be
  listed there (with 66/100/133 Mhz bus speed btw)
  - uses basetype unit

  09-06-2000 makefile, directory structure (rel2rev3)
  - added makefiles
  - new directory structure

  13-06-2000 delay() fix (rel2rev4)
  - changed TZentimer to calculate everything in microseconds, not the internal tick count
  solves some problems with delay(), waitfor() and other stuff. Decreases code size too.
  - TZentimer::TicksToUSec() inlined for better accuracy

  28-06-2000 updates (rel2rev5)
  - PIV, AMD Duron and AMD Thunderbird recognition
  - new clock rating detection for P5 class processors to avoid huge lists of known
  processor ratings
  - ratings also include 'overclocked' CPU's (e.g. frequency calc speeds <> 66, 100, 133,
  200 Mhz) now.

  05-07-2000 updates (rel2rev6)
  - added detection of several new P4 capabilities like SSE2
  - added Transmeta processor recognition

  23-07-2000 documentation update (rel2rev7)
  - updated documentation to rel1rev2

  16-08-2000 update, some new capabilities (rel2rev8)
  - added docupd.txt
  - added function Clock() which returns fixed resolution timestamp
  - added detection of AMD-64 bit Long Mode
  - milisecond fixes reported by John Lee(LeeJ@logica.com)
  - added information output to file for cpuid.pas

  17-08-2000 update, fixed clocks() function (rel2rev9)
  - added a call to round()

  09-09-2000 updates, fixes
  - PIT check now works across one day
  - changes for cross-compiler compatibility (platform.inc added, basetype, cpu)
  - startup/exit code stuff moved to initialization/finalization blocks
  - should be multiprocessor aware on Win32 now
  - accuracy boost of clock speed detection on Win32 platform
  - added platform.inc

  10-09-2000 fixes (rel2rev11)
  - platform.inc: added DPMI32 target for Virtual Pascal
  - fixes to compile FPC Win32 target

  10-09-2000 added Delphi 2 support (rel2rev12)
  - fixes to Delphi/VPascal port
  - now runs with Delphi 2
  - added example program in the delphi directory
  - overload bug fixed (Delphi 5, thanks go to XRay49er@aol.com)
  - fixed platform.inc

  12-09-2000 FPC 1.0 support, platform.inc (rel2rev13)
  - fixes to get it compile with FPC 1.0 (overload)
  - added FREEBSD within platform.inc

  09-11-2000 accuracy not scaling dependant for TZentimer
  - reprogrammed whole TTimer class to allow the accuracy being independant of
  scaling
  - removed AT&T code :( to reduce code size
  - some very long comments moved to the docs

  14-02-2001 fixed AMD family detection
  - AMD processor families range from 4 to 6 not from 5 to 7 as implemented in the code

  28-03-2001 new release (rel2rev16)
  - makefile improvements
  - doc improvements

  29-03-2001 fixes
  - AMD type/family now correctly read from extended cpuid levels,
  should fix some problems
  - added Pentium IV extended model and family checks
  - added Thermal Control capability (PIII model 8, PIV)
  - added AMD K6-II+ / K6-III+ to known processor types
  - AMD 5k86 model 0 doesn't have extended CPUid levels

  01-04-2001 fixes (rel2rev17)
  - extended model / family only on Intel processors

  10-04-2001 fixes
  - fixed GO32V2 compiler problems with 'uses' statement without a unit name to include
  (reported by Lee John)

  14-04-2001 fixes (rel2rev18)
  - fixed MMX unit for non-FPC targets (reported by Ralph Roth)

  06-06-2001 fixes (rel2rev19)
  - fixed Delphi 5 compatibility (kernel32.dll not found, reported by Harry Keogh)
  - added Pause Method to suspend / resume counter operation (suggestion by S. Goehler)

  18-12-2001 updates (rel2rev20)
  - HTT and IA-64 capability bit for Intel processors
  - Transmeta CPU strings
  - Cyrix MIII CPU strings
  - Intel CPU strings
  - AMD CPU strings

  07-01-2002 updates (rel2rev21)
  - fixes for VP / older FPC compilers (RTL_CRITICAL_SECTION -> CRITICAL_SECTION)
  (reported by R. Roth)
  - Transmeta Brand Tables are now used in CPU name generation

  29-05-2002 updates (rel2rev22)
  - added delphi6 support to platform.inc
  - some code to allow overloading for delphi6 (both problems reported by
  LeeJ@logica.com)
  - Pending Break Event capability added (P4)
  - some new processor type strings added
  - changed handling of extended family value - now mapped to values beginning
  with extended_family_shift ($10)
  - hopefully fixed final 'millisecond' problems ^__^ in cputest.pas
  - cpu_id_string() and related constants were introduced to allow runtime version
  retrieval

  11-02-2003 updates (rel2rev23)
  - revised CPU capabilites
  - revised CPU brand name tables
  - logical cpu count (P4) in TCPUInfo 
  
  08-12-2004 updates (rel2rev24)
  - integrated changes for FPC 1.9.2+ changes (calling convention, RTL)
  - Delphi 9 (2005) updates
  - only list tested platforms in header
  
  09-12-2004 capability detection updates (rel2rev25)
  - a few new capabilities are detected now
  - redesigned the capability detection code
  - using brand ID / brand ID string returned by CPUID 8x2-8x4 in addition to
  the table lookup for brand string calculation
  - Lazarus example program
  
  11-01-2005 mail address update (rel2rev26)
  - updated mail addresses in headers (thanks go to Lee John for reporting this)
  
  04-02-2005 fixes for 1.0.10 (rel2rev27)
  - fix compilation for 1.0.10 compiler (thanks go to Ralph Roth for the fix)
  - fixed renaming
  - makefile updates
  - new example program (timeit)
  
  10-02-2005 fixes for 1.0.10, brand string (rel2rev28)
  - fixed brand string reading which didn't like the strings returned by the CPUId
  function (left aligned with #0s). Compatible with 1.0.10 too.
  - fixed, if old caps were used, a type which caused wrong detection of a single
  capability
  - some space to tab changes
  
  27-02-2005 DAZ capability, resourcestrings, basetype udpate (rel2rev29)
  - added "Denormals Are Zero" capability including a method to set the FPU to this mode
  - capability strings are now resource strings
  - added PtrInt and PtrUInt to basetype unit
  - fixed em64t and added NX bit detection for intel cpus
}
