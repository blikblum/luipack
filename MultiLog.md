# Introduction #

MultiLog is a logging system designed from the ground and specifically to fpc/Lazarus. It has as primary objective be at same time flexible and with low overhead. MultiLog is composed by the base library (MultiLog and MultilogLcl units) used in the applications to send the messages and by the Log Viewers (SimpleLogViewer and MultilogViewer).

Starting from 0.5 version the base library can be used in Delphi applications, i.e., can send messages. The Log Viewers are still only compiled with Lazarus.

# Status #

MultiLog is fully functional.

# Requirements #

  * Base Library (MultiLog unit)
    * fpc 2.2.0
  * LCL (MultiLogLcl unit)
    * Lazarus 0.9.24 or above
  * SimpleLogViewer
    * Lazarus 0.9.24 or above
  * Multilog Viewer
    * Lazarus 0.9.24 or above
    * MultiLog
    * [Virtual Treeview](VirtualTreeview.md)
    * ATBinHex
    * [Unique Instance](http://wiki.lazarus.freepascal.org/UniqueInstance)
    * [Lui Controls](LuiControls.md)

# Release History #

See [Lazarus wiki](http://wiki.lazarus.freepascal.org/MultiLog) for changelog

# Usage examples #

## Add a log channel ##

Before sending messages is necessary to add a log channel to the Logger class. A logger class can have more than one channel at same time (the name MultiLog has a reason!). Is recommended to do this in the main program file. Than add SharedLogger in the uses classes of other units of the application.

```
uses
  SharedLogger, ipcchannel, filechannel;
begin
  Logger.Channels.Add(TIPCChannel.Create);
  Logger.Channels.Add(TFileChannel.Create('logfile.txt'));
end;
```

## Send messages / Evaluate variables ##

Multilog can evaluate the following variable types: Integer, Cardinal, Double, Boolean, String, TRect, TPoint, TObject, TStrings

```
uses 
  SharedLogger;
var
  i: Integer;
  s: String;
begin  
  Logger.Send('Simple Message');
  Logger.Send('Integer Value',i);
  Logger.Send('String Value',s);
  Logger.Send('Formated text - Integer: %d  String: %s',[i,s])  
end.
```

## Determine what messages will be sent ##

WARNING: Since 0.4 the Log Class schema changed. These examples are valid only for 0.4 or above

Multilog can determine at runtime what messages should be sent or not. All "SendXXX" methods are associated with one or more log classes. These classes can be determined explicitly (Using the Classes parameter) or implicitly (Messages sent using the overloaded methods that does not have Classes parameter will be associated with the classes determined by DefaultClasses).

The Classes parameter can be a set of any integer from 0 to 31. Is recommended that the programmer add to the SharedLogger unit constants to be used as classes aliases. For example: lcEvents, lcPaint, lcFileOperation etc...

```
uses 
  SharedLogger;
begin
  Logger.ActiveClasses:=[0,1];
  Logger.DefaultClasses:=[0];
  
  //Implicitly set Class (DefaultClasses)
  Logger.Send('This message should be sent because DefaultClasses ([0]) intersects with ActiveClasses ([0,1])');
  DefaultClasses:=[2];
  Logger.Send('This message should NOT be sent because DefaultClasses ([2]) does NOT intersect with ActiveClasses ([0,1])');  
  
  //Explicitly set Classes
  Logger.Send([1],'This message should be sent because Classes ([1]) intersects with  ActiveClasses [0,1]');
  Logger.Send([2],'This message should NOT be sent because Classes ([2]) does NOT intersect with  ActiveClasses [0,1]');
  Logger.ActiveClasses:=[2];
  Logger.Send([1],'This message should NOT be sent because Classes ([1]) does NOT intersect with  ActiveClasses [2]');
  Logger.Send([2],'This message should be sent because Classes ([2]) intersects with ActiveClasses [2]'); 
  //Associating a message with more than one class
  Logger.Send([1,3],'This message should NOT be sent because Classes ([1,3]) does NOT intersect with  ActiveClasses [2]');
  Logger.Send([2,3],'This message should be sent because Classes ([2,3]) intersects with ActiveClasses [2]');  
end.
```

## Send the current Call Stack ##

MultiLog has the ability to send the current call stack. If the program was compiled with debug information the function names and line numbers will be show otherwise only the function addresses. The stack frames number is limited by the MaxStackCount property

```
uses 
  SharedLogger;
begin
  Logger.MaxStackCount:=10; 
  Logger.SendCallStack('CurrentCallStack');
end.
```

## Send Exception Info ##

MultiLog can send the info of an Exception including the exception stack frame.

```
uses 
  SharedLogger;
begin
  try
    1/0
  except
  on E: Exception do
    Logger.SendException('Something wrong occurred',E); 
  end;
end.
```