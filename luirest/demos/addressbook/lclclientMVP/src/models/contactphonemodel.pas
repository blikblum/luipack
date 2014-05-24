unit ContactPhoneModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, SimpleJSONModel;

type

 { TContactPhone }

 TContactPhone = class(TSimpleJSONModel)
 protected
   function CreateData: TJSONObject; override;
   class function GetResourceName: String; override;
 end;


 { TContactPhones }

 TContactPhones = class(TSimpleJSONCollection)
 public
   constructor Create;
   function Add: TContactPhone;
   function GetItem(ItemData: TJSONObject): TContactPhone;
 end;

implementation

{ TContactPhone }

function TContactPhone.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create(['number', 'New Number']);
end;

class function TContactPhone.GetResourceName: String;
begin
  Result := 'contactphone';
end;

{ TContactPhones }

constructor TContactPhones.Create;
begin
  inherited Create(TContactPhone);
end;

function TContactPhones.Add: TContactPhone;
begin
  Result := TContactPhone(inherited Add);
end;

function TContactPhones.GetItem(ItemData: TJSONObject): TContactPhone;
begin
  Result := TContactPhone(inherited GetItem(ItemData));
end;

end.

