unit ContactPresenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasePresenter, ContactModel, fpjson;

type

  { TContactPresenter }

  TContactPresenter = class(TBasePresenter)
  private
    FContact: TContact;
  protected
    function GetViewCaption(const DesignCaption: String): String; override;
  public
    function EditCategories: Boolean;
    procedure SaveContact;
  published
    property Contact: TContact read FContact write FContact;
  end;

implementation

uses
  strutils, Controls;

{ TContactPresenter }

function TContactPresenter.GetViewCaption(const DesignCaption: String): String;
begin
  Result := IfThen(FContact.Data.Find('id') = nil, 'Add', 'Edit') + ' Contact';
end;

function TContactPresenter.EditCategories: Boolean;
begin
  Result := Presentations['categories'].ShowModal.ModalResult = mrOK;
end;

procedure TContactPresenter.SaveContact;
begin
  FContact.Save;
end;

end.

