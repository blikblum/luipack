# Introduction #

Lui Controls is a collection of simple visual controls created from scratch. They are mostly based in similar controls found in other GUI toolkits like Java or Gtk.

# Status #

Currently there are three components:

  * **TToggleLabel**: a TCustomLabel descendant that can be toggled between two states. It provides properties to define the caption showed in each state and an event to notify the state change. It also has a visual indication (a triangle) of the state.
  * **TMenuButton**: a TCustomSpeedButton descendant that combines with a TPopupMenu to give an integrated look and feel.
  * **TSearchEdit**: a TCustomEdit descendant that shows a text indicative of its usage and provides a callback method to execute when the user set a text.

Tested in win32, gtk, gtk2, Qt.

Remarks:

  * TPopupMenu:
    * Gtk/Gtk2: the menu is always show when mouse is released due to LCL bug [9435](http://www.freepascal.org/mantis/view.php?id=9435)
  * TSearchEdit:
    * Gtk/Gtk2 the color does not change to gray when is empty due to LCL bug [9945](http://www.freepascal.org/mantis/view.php?id=9945)

# Requirements #

  * Lazarus 0.9.26 or a recent svn

# History #

  * 22/10/2007 - version 0.2
    * Added TSearchEdit component
    * TMenuButton: Implemented inline arrow indicator
    * TMenuButton: Added Style property
    * TMenuButton: Added option to popup on MouseUp
    * TTogleLabel: Removed hack to determine text position. Use DoMeasureTextPosition instead
  * 12/08/2007
    * Initial release. Two components: TMenuButton and TToggleLabel

# Usage #

Complete API documentation needs to be written. Here's the minimal info to use the controls:

  * TToggleLabel
    * Expanded (Boolean): Returns or set the label Expanded state
    * Caption (String): The text show when Expanded is False
    * ExpandedCaption (String): The text show when Expanded is True
    * OnChange (TNotifyEvent): Notifies when the Label changes it states
  * TMenuButton
    * Menu (TPopupMenu): the associated menu
    * Style (mbsSingle|mbsCombo):
      * mbsSingle: only one button is show. Click on it and the menu will popup. OnClick is not fired
      * mbsCombo: a button with an arrow indicator is show at the side of the main button. Click in the arrow button to popup the menu and in the main button to fire OnClick event
    * Options (set of [mboShowIndicator, mboPopupOnMouseUp]):
      * mboShowIndicator: an arrow is draw in the main button just after the text. Ignored in mbsCombo style
      * mboPopupOnMouseUp: the menu will popup in the MouseUp event
  * TSearchEdit:
    * EmptyText(String): the text to be show when the edit is empty and the control has no focus
    * OnExecute(TNotifyEvent): called when the user finish edit. Behavior modified by Options property.
    * Options(set of [seoExecuteEmpty, seoExecuteOnKillFocus])
      * seoExecuteEmpty: OnExecute is called even if the edited text is empty
      * seoExecuteOnKillFocus: OnExecute is called when the control looses focus