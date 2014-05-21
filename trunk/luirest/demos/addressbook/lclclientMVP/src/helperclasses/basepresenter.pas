unit BasePresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PresentationManager;

type

  { TBasePresenter }

  TBasePresenter = class(TPresenter)
  private
    class var FPresentations: IPresentationManager;
  protected
  public
    class property Presentations: IPresentationManager read FPresentations;
    class function newinstance: tobject; override;
  end;

implementation

uses
  LuiServices;

{ TBasePresenter }

class function TBasePresenter.newinstance: tobject;
begin
  Result := inherited newinstance;
  if FPresentations = nil then
    Services.Resolve(IPresentationManager, FPresentations);
end;

end.

