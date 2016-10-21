unit PasswordUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function DecodePassword(const Password: String): String;

function EncodePassword(const Password: String): String;

implementation

uses
  strutils;

const
  PassKey = {$I passkey.inc};

function DecodePassword(const Password: String): String;
begin
  Result := XorDecode(PassKey, Password);
end;

function EncodePassword(const Password: String): String;
begin
  Result := XorEncode(PassKey, Password);
end;

end.

