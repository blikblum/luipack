unit CategoriesView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TCategoriesForm }

  TCategoriesForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CategoriesForm: TCategoriesForm;

implementation

{$R *.lfm}

end.

