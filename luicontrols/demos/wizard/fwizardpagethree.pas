unit fWizardPageThree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TPageThreeFrame }

  TPageThreeFrame = class(TFrame)
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

initialization
  RegisterClass(TPageThreeFrame);

end.

