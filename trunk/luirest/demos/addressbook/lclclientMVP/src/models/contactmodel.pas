unit ContactModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, SimpleJSONModel;

type

  { TContact }

  TContact = class(TSimpleJSONModel)
  protected
    function CreateData: TJSONObject; override;
    class function GetResourceName: String; override;
  end;

  { TContacts }

  TContacts = class(TSimpleJSONCollection)
  public
    constructor Create;
    function Add: TContact;
    function GetItem(ItemData: TJSONObject): TContact;
  end;

implementation

{ TContacts }

constructor TContacts.Create;
begin
  inherited Create(TContact);
end;

function TContacts.Add: TContact;
begin
  Result := TContact(inherited Add);
end;

function TContacts.GetItem(ItemData: TJSONObject): TContact;
begin
  Result := TContact(inherited GetItem(ItemData));
end;

{ TContact }

function TContact.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create(['name', 'New Contact']);
end;

class function TContact.GetResourceName: String;
begin
  Result := 'contact';
end;

end.

