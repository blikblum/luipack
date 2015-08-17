# Introduction #

ATBinhex is a component that can display files in Unicode (UTF-16LE), Hexadecimal, Binary and Ansi modes. It's originally developed by Alexey Torgashin. The Delphi package can be found [here](http://atorg.net.ru/delphi/atbinhex.htm)

# Features #

The LCL port has almost all features of the [Delphi](http://atorg.net.ru/delphi/atbinhex.htm) version except by:

  * All:
    * Does not has support for Regular Expressions search
    * Does not has file notification support
  * GTK only
    * Selection is disabled by default due to being slow in some situations (HEX and Unicode modes under gtk2) and crash in others (Unicode mode under gtk1).
      * To enable it define EnableSelection in ATBinHexOptions.inc file

# Documentation #

You can use the documentation that comes with the Delphi package considering the above remarks

# Requirements #

  * Lazarus 0.9.26 or a recent svn
  * [LCLExtensions](LCLExtensions.md) package

# Release History #

  * 19/05/2007
    * Initial release
