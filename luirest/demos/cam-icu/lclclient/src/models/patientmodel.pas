unit PatientModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LuiJSONModel;

type

  { TPatient }

  TPatient = class(TJSONModel)
  protected
    function CreateData: TJSONObject; override;
    class function GetResourceName: String; override;
  end;

  { TPatients }

  TPatients = class(TJSONCollection)
  public
    constructor Create; override;
    function CreateItem: TPatient;
    function Get(ItemData: TJSONObject): TPatient;
  end;

implementation

{ TPatients }

constructor TPatients.Create;
begin
  inherited Create(TPatient);
end;

function TPatients.CreateItem: TPatient;
begin
  Result := TPatient(inherited CreateItem);
end;

function TPatients.Get(ItemData: TJSONObject): TPatient;
begin
  Result := TPatient(inherited Get(ItemData));
end;

{ TPatient }

function TPatient.CreateData: TJSONObject;
begin
  Result := TJSONObject.Create(['name', 'Novo Paciente']);
end;

class function TPatient.GetResourceName: String;
begin
  Result := 'patient';
end;

end.

