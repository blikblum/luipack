# Introduction #

This package has some units to help LCL development:

  * DelphiCompat: Some winapi functions/constants that are missing in LCL
  * LCLExt: Contains functions that does not exist both in Delphi and LCL
  * OleUtils (win32 only): Implements TOleStream

# Status #

  * DelphiCompat:
    * Win32: all functions work.
    * Linux/GTK: only the functions required to build Virtual Treeview and ATBinHex work
  * LCLExt: Fully working in win32
  * OleUtils: Fully working

# History #

  * 01/07/07 - Version 0.2
    * Removed MultiLog dependency
    * Implemented SetTimer, KillTimer, GetBkColor under gtk
    * Added GetCurrentObject
  * 19/05/07 - Version 0.1
    * First release