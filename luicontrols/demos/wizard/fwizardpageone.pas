unit fWizardPageOne;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TPageOneFrame }

  TPageOneFrame = class(TFrame)
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

initialization
  RegisterClass(TPageOneFrame);

end.

