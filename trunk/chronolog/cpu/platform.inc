{
   Include file to sort out compilers/platforms/targets

   Written to be compatible with Free Pascal API's platform.inc.

   Done by Thomas Schatzl (tom_at_work@gmx.at), 11. Jan. 2005

   Thanks go to the Free Pascal Development team for the idea.
   
}

{ ****************************************************************************

   This include file defines some conditional defines to allow us to select
   the compiler/platform/target in a consequent way.

      OS_XXXX      The operating system (Windows, Win32, Win64, OS/2, Unix,
                   BSD, Linux, FreeBSD, NetBSD, OpenBSD, Amiga, Mac, Atari,
                   DPMI32, DOS, GO32, Palm, SUNOS)

                   OS_WINDOWS   MS Windows
                   OS_WIN32     32 Bit MS Windows. Also defines OS_WINDOWS.
                   OS_WIN64     64 Bit MS Windows. Also defines OS_WINDOWS.
                   OS_OS2       OS/2
                   OS_UNIX      Unix derivative.
                   OS_BSD       BSD. Also defines OS_UNIX.
                   OS_LINUX     Linux. Also defines OS_UNIX.
                   OS_FREEBSD   FreeBSD. Also defines OS_BSD.
                   OS_NETBSD    NetBSD. Also defines OS_BSD.
                   OS_OPENBSD   OpenBSD. Also defines OS_BSD.
                   OS_AMIGA     Classic Amiga.
                   OS_MAC       Classic Macintosh.
                   OS_ATARI     Atari TOS.
                   OS_DPMI32    32 bit DPMI. Also defines OS_DOS.
                   OS_DOS       DOS.
                   OS_GO32      GO32V2. Also defines OS_DOS.
                   OS_PALMOS    PalmOS.
                   OS_SUNOS     SunOS.
                   OS_BEOS      BeOS.
                 
      
      PPC_XXXX     The compiler (TP/BP, BP for OS/2, Delphi, Delphi2 - 7,
                   Kylix, Freepascal, TMT Pascal, Virtual Pascal, Speed 
                   Pascal)
                   
                   PPC_BP        Borland Pascal.
                   PPC_BPOS2     Borland Pascal with OS/2 patch.
                   PPC_DELPHI    Borland Delphi.
                   PPC_DELPHI2   Borland Delphi 2. Also defines PPC_DELPHI.
                   PPC_DELPHI3   Borland Delphi 3. Also defines PPC_DELPHI.
                   PPC_DELPHI4   Borland Delphi 4. Also defines PPC_DELPHI.
                   PPC_DELPHI5   Borland Delphi 5. Also defines PPC_DELPHI.
                   PPC_DELPHI6   Borland Delphi 6. Also defines PPC_DELPHI.
                   PPC_DELPHI7   Borland Delphi 7. Also defines PPC_DELPHI.
                   PPC_KYLIX     Borland Kylix. Also defines PPC_DELPHI and 
                                 PPC_DELPHI6.
                   PPC_FPC       Free Pascal.
                   PPC_TMT       TMT Pascal.
                   PPC_VIRTUAL   Virtual Pascal.
                   PPC_SPEED     Speed Pascal.
      
      BIT_XXXX     Target platform addressing mode, only applies when CPU_X86 
                   set.
                   
                   BIT_16  16 bit addressing environment.
                   BIT_32  32 bit addressing environment.
      
      PROC_XXXX    Target platform processor mode, only applies when CPU_X86
                   set.
                    
                   PROC_REAL       Real mode environment.
                   PROC_PROTECTED  Protected mode environment.
      
      ASM_XXXX     Type of assembler style used.
      
                   ASM_FPC or ASM_ATT    x86 AT&T style.
                   ASM_BP or ASM_INTEL   x86 Intel style.
                   ASM_INTERNAL          Some other style.
       
      CPU_XXXX     CPU type on target platform.  
      
                   CPU_X86       x86 compatible processor.
                   CPU_68K       68000 compatible processor.
                   CPU_POWERPC   32 bit PowerPC compatible processor.
                   CPU_ALPHA     Alpha AXP compatible processor.
                   CPU_SPARC     SPARC v7 compatible processor.

 ****************************************************************************}

{****************************************************************************}
{ Compiler specific stuff                                                    }
{****************************************************************************}

{----------------------------------------------------------------------------}
{ Free Pascal Compiler                                                       }
{----------------------------------------------------------------------------}

{$IFDEF FPC}
  {$DEFINE PPC_FPC}

  {$IFDEF I386}
    {$DEFINE ASM_FPC} // by default AT&T assembly
    {$DEFINE BIT_32} // always 32 bits
    {$DEFINE PROC_PROTECTED} // always protected mode
    {$DEFINE CPU_X86}
  {$ENDIF}

  {$IFDEF CPU68}
    {$DEFINE CPU_68K}
  {$ENDIF}
  
  {$IFDEF CPUSPARC}
    {$DEFINE CPU_SPARC}
  {$ENDIF}

  {$IFDEF CPUALPHA}
    {$DEFINE CPU_ALPHA}
  {$ENDIF}

  {$IFDEF CPUPPC}
    {$DEFINE CPU_POWERPC}
  {$ENDIF}

  {$IFDEF GO32V2}
    {$DEFINE OS_DOS}
    {$DEFINE OS_GO32}
  {$ENDIF}

  {$IFDEF WIN32}
    {$DEFINE OS_WINDOWS}
    {$DEFINE OS_WIN32}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE OS_LINUX}
  {$ENDIF}

  {$IFDEF FREEBSD}
    {$DEFINE OS_FREEBSD}
  {$ENDIF}

  {$IFDEF NETBSD}
    {$DEFINE OS_NETBSD}
  {$ENDIF}

  {$IFDEF OPENBSD}
    {$DEFINE OS_OPENBSD}
  {$ENDIF}

  {$IFDEF OS2}
    {$DEFINE OS_OS2}
  {$ENDIF}
  
  {$IFDEF BEOS}
    {$DEFINE OS_BEOS}
  {$ENDIF}
  
  {$IFDEF PALMOS}
    {$DEFINE OS_PALMOS}
  {$ENDIF}
  
  {$IFDEF SUNOS}
    {$DEFINE OS_SUNOS}
  {$ENDIF}

  {$IFDEF AMIGA}
    {$DEFINE OS_AMIGA}
    {$DEFINE CPU_68K}
  {$ENDIF}

  {$IFDEF ATARI}
    {$DEFINE OS_ATARI}
  {$ENDIF}

  {$IFDEF MACOS}
    {$DEFINE OS_MAC}
  {$ENDIF}
{$ENDIF}

{----------------------------------------------------------------------------}
{ Virtual Pascal Compiler                                                    }
{----------------------------------------------------------------------------}

{$IFDEF VIRTUALPASCAL}

  {$DEFINE PROC_PROTECTED} // always protected mode
  {$DEFINE BIT_32} // always 32 bits
  {$DEFINE PPC_VIRTUAL}
  {$DEFINE ASM_BP} // by default Intel assembly
  {$DEFINE CPU_X86} // always x86 code

  {$IFDEF DPMI32}
    {$DEFINE OS_DOS}
    {$DEFINE OS_DPMI32}
  {$ENDIF}

  {$IFDEF WIN32}
    {$DEFINE OS_WINDOWS}
    {$DEFINE OS_WIN32}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE OS_LINUX}
    {$DEFINE OS_UNIX}
  {$ENDIF}

  {$IFDEF OS2}
    {$DEFINE OS_OS2}
  {$ENDIF}

{$ENDIF}

{---------------------------------------------------------------------------}
{  TMT 32 BIT compiler                                                      }
{---------------------------------------------------------------------------}

{$IFDEF __TMT__}
  {$DEFINE PROC_PROTECTED} // always protected mode
  {$DEFINE BIT_32} // always 32 bits
  {$DEFINE PPC_TMT}
  {$DEFINE ASM_BP} // by default Intel assembly
  {$DEFINE CPU_X86} // always x86 code

  {$IFDEF __DOS__}
    {$DEFINE OS_DPMI32}
    {$DEFINE OS_DOS}
  {$ENDIF}

  {$IFDEF __WIN32__}
    {$DEFINE OS_WINDOWS}
    {$DEFINE OS_WIN32}
  {$ENDIF}

  {$IFDEF __OS2__}
    {$DEFINE OS_OS2}
  {$ENDIF}

{$ENDIF}

{---------------------------------------------------------------------------}
{  Delphi compilers                                                         }
{---------------------------------------------------------------------------}

{$IFDEF VER80}
  {$DEFINE PPC_DELPHI}
  {$DEFINE BIT_16}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI2}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI5}
  {$DEFINE PPC_DELPHI5UP}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI6}
  {$DEFINE PPC_DELPHI5UP}
  {$DEFINE PPC_DELPHI6UP}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI7}
  {$DEFINE PPC_DELPHI5UP}
  {$DEFINE PPC_DELPHI6UP}
  {$DEFINE PPC_DELPHI7UP}
{$ENDIF}

{$IFDEF VER160}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI8}
  {$DEFINE PPC_DELPHI5UP}
  {$DEFINE PPC_DELPHI6UP}
  {$DEFINE PPC_DELPHI7UP}
  {$DEFINE PPC_DELPHI8UP}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE PPC_DELPHI}
  {$DEFINE PPC_DELPHI9}
  {$DEFINE PPC_DELPHI5UP}
  {$DEFINE PPC_DELPHI6UP}
  {$DEFINE PPC_DELPHI7UP}
  {$DEFINE PPC_DELPHI8UP}
  {$DEFINE PPC_DELPHI9UP}
{$ENDIF}

{$IFDEF PPC_DELPHI}
  {$IFNDEF BIT_16}
    {$DEFINE BIT32}
  {$ENDIF}

  {$DEFINE PROC_PROTECTED} // always protected mode
  {$DEFINE ASM_BP} // by default Intel assembly style
  {$DEFINE CPU_X86} // always x86 code
  
  {$IFDEF WIN32}
    {$DEFINE OS_WINDOWS}
    {$DEFINE BIT_32}
  {$ENDIF}

  {$IFDEF WIN64} // a sophisticated guess =)
    {$DEFINE OS_WINDOWS}
    {$DEFINE OS_WIN64}
    {$DEFINE BIT_64}
    {$UNDEF BIT_32}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE OS_LINUX}
    {$DEFINE PPC_KYLIX}
  {$ENDIF}
{$ENDIF}


{---------------------------------------------------------------------------}
{  SPEED Pascal Compiler                                                    }
{---------------------------------------------------------------------------}
{$IFDEF Speed}
  {$DEFINE PROC_PROTECTED} // always protected mode
  {$DEFINE BIT_32} // always 32 bits
  {$DEFINE PPC_SPEED}
  {$DEFINE ASM_BP} // by default Intel assembly
  {$DEFINE CPU_X86} // always x86 code

  {$DEFINE OS_OS2} // always OS/2 ????
{$ENDIF}

{---------------------------------------------------------------------------}
{  Borland Pascal Compiler                                                  }
{---------------------------------------------------------------------------}

{$IFNDEF PPC_FPC}
{$IFNDEF PPC_VIRTUAL}
{$IFNDEF PPC_TMT}
{$IFNDEF PPC_DELPHI}
{$IFNDEF PPC_SPEED}

  {$DEFINE PPC_BP}
  {$DEFINE ASM_BP}
  {$DEFINE BIT_16}

  {$IFDEF DPMI}
    {$DEFINE PROC_PROTECTED}
  {$ELSE}
    {$DEFINE PROC_REAL}
  {$ENDIF}

  {$IFDEF OS2}
    {$DEFINE OS_OS2}
    {$DEFINE PPC_BPOS2}
    {$UNDEF PROC_REAL}
    {$DEFINE PROC_PROTECTED}
  {$ELSE}
    {$IFDEF WINDOWS}
      {$DEFINE OS_WINDOWS}
      {$DEFINE PROC_PROTECTED}
      {$UNDEF PROC_REAL}
    {$ELSE}
      {$DEFINE OS_DOS}
    {$ENDIF}
  {$ENDIF}

{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{****************************************************************************}
{ OS specific changes                                                        }
{****************************************************************************}

{$IFDEF OS_FREEBSD}
  {$DEFINE OS_BSD}
{$ENDIF}

{$IFDEF OS_OPENBSD}
  {$DEFINE OS_BSD}
{$ENDIF}

{$IFDEF OS_NETBSD}
  {$DEFINE OS_BSD}
{$ENDIF}

{$IFDEF OS_BSD}
  {$DEFINE OS_UNIX}
{$ENDIF}

{$IFDEF OS_LINUX}
  {$DEFINE OS_UNIX}
{$ENDIF}

{$IFDEF OS_BEOS}
  {$DEFINE OS_UNIX}
{$ENDIF}

{$IFDEF OS_DOS}
{$ENDIF}

{$IFDEF OS_OS2}
{$ENDIF}

{$IFDEF OS_WINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}

{****************************************************************************}
{ Compatibility                                                              }
{****************************************************************************}


{$IFDEF CPU_X86}
  {$DEFINE CPU86}
{$ENDIF}
{$IFDEF CPU_68K}
  {$DEFINE CPU68}
{$ENDIF}

{ History
  12-02-2003 start of history
  - lots of changes. Hope I didn't make too much mistakes.
  
  11-01-2005 updated mail address
  - updated mail address in header (reported by Lee John)
}
