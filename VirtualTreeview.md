# Introduction #

[Virtual Treeview](http://www.soft-gems.net/VirtualTreeview/) is a powerful Delphi component that combines fast painting with flexibility and functionality.

There's already a working Lazarus Virtual Treeview port so why redo this? The basic idea is to maintain all features of the original version that was missed in the first port. Detailed reasons [here](http://lazarusroad.blogspot.com/2007/02/ports-of-delphi-components.html)

# Status #

Currently the port is fully working in win32, gtk, gtk2 and Qt.

# Port Guidelines #

The general approach is to **try** to keep all features without changing much the core code.

  * Only remove code if absolutely unnecessary or absolutely not portable
  * Add log messages every where
  * If changes are necessary then change only one subsystem at time (Painting, DragDrop etc) and than test.
  * If a bug / Delphi incompatibility is found than fix or report
  * Document the changes made and the Delphi/LCL differences

# Port Roadmap #

  * Make compilable - OK
  * Revise the code - OK
  * Add logging infrastructure - OK
  * Make work at windows - OK
  * Emulate each missing winapi function (test both in win and gtk) - WIP
  * Ensure all messages are working as expected
  * Replace the emulated code by LCL functions
  * Define how Unicode/bidi support will be done - OK
    * Replace WideString by UTF-8 (The standard for LCL)
  * Define Drag and Drop support
    * Migrate to LCL Drag and Drop support - OK
    * See how to do Drag and Drop between applications
  * Convert asm code
    * Maybe not necessary to convert asm if newer versions of fpc become more Delphi compatible
    * Add fallback functions for architectures other than i386
  * Make logging functions optional by using conditional defines
  * Back port changes in the original code - OK at 29/03/2008
  * Add improvements

# Using the SVN Version #

## Requirements ##

  * Virtual Treeview from [svn](https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/virtualtreeview-unstable/)
  * Lazarus 0.9.26 or a recent svn
  * [LCLExtensions](LCLExtensions.md) package from svn
  * MultiLog 0.4 or above

## Known Issues ##

  * It was only tested in 32bit OS. Probably it won't work in 64bit OS mostly due to timer implementation that expects a 32bit THandle
  * Alpha blend is working only under win32 and Qt
  * It wont work with carbon interface (i don't have a Mac to develop)
  * The panning window in gtk is not implemented, although the scrolling when middle mouse button is pressed works.
  * Win98 requires Unicode compatibility layer to be installed. Not tested.
  * Drag and drop to/from other LCL controls was not tested extensively.
    * Few tests were done without known issues.
  * OLE drag and drop works only in win32.

## Planned changes before a full release ##

  * Remove multilog dependency
  * Fix most of the known issues
  * Merge the changes done in soft-gems svn repository. OK at 29/03/2008
  * The Unicode encoding will change from UTF-16 to UTF-8