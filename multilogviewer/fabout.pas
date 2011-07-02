unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonClose: TBitBtn;
    LabelCopyright: TLabel;
    LabelTitle: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

initialization

end.

