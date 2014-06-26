unit JSONModelDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf;

type

  { TJSONModelUnitDescriptor }

  TJSONModelUnitDescriptor = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetLocalizedDescription : String; override;
    function GetInterfaceSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
  end;

implementation

{ TJSONModelUnitDescriptor }

constructor TJSONModelUnitDescriptor.Create;
begin
  inherited Create;
  Name := 'LuiJSONModel Template';
  DefaultFilename:='jsonmodel.pas';
  DefaultSourceName:='JSONModel';
end;

function TJSONModelUnitDescriptor.GetLocalizedDescription: String;
begin
  Result := 'Boilerplate code to use LuiJSONModel';
end;

function TJSONModelUnitDescriptor.GetInterfaceSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  //todo: add inline to Get, GetItem, Find, Add methods after fpc >= 2.8 is released
  //fpc 2.6.4 has a bug that corrupts memory when using inline in Add
  Result :='type' +LineEnding +
''+ LineEnding +
'  { TMyModel }'+ LineEnding +
''+ LineEnding +
'  TMyModel = class(TJSONModel)' + LineEnding +
'  protected'+ LineEnding +
'    class function GetResourceName: String; override;'+ LineEnding +
'  end;'+ LineEnding +
''+ LineEnding +
'  { TMyCollection }'+ LineEnding +
''+ LineEnding +
'  TMyCollection = class(TJSONCollection)'+ LineEnding +
'  private'+ LineEnding +
'    function GetItem(Index: Integer): TMyModel;'+ LineEnding +
'  protected'+ LineEnding +
'    class function GetItemClass: TJSONModelClass; override;'+ LineEnding +
'  public'+ LineEnding +
'    function CreateItem: TMyModel;'+ LineEnding +
'    function Find(const Id: Variant): TMyModel;'+ LineEnding +
'    function Get(ItemData: TJSONObject): TMyModel;'+ LineEnding +
'    property Items[Index: Integer]: TMyModel read GetItem; default;'+ LineEnding +
'  end;'+ LineEnding + LineEnding;
end;

function TJSONModelUnitDescriptor.GetInterfaceUsesSection: String;
begin
  Result := 'Classes, SysUtils, LuiJSONModel, fpjson';
end;

function TJSONModelUnitDescriptor.GetImplementationSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  Result :='{ TMyModel }'+ LineEnding +
''+ LineEnding +
'class function TMyModel.GetResourceName: String;'+ LineEnding +
'begin'+ LineEnding +
'  Result := ''resourcename'';'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'{ TMyCollection }'+ LineEnding +
''+ LineEnding +
'function TMyCollection.GetItem(Index: Integer): TMyModel;'+ LineEnding +
'begin'+ LineEnding +
'  Result := TMyModel(inherited GetItem(Index));'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'class function TMyCollection.GetItemClass: TJSONModelClass;'+ LineEnding +
'begin'+ LineEnding +
'  Result := TMyModel;'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function TMyCollection.CreateItem: TMyModel;'+ LineEnding +
'begin'+ LineEnding +
'  Result := TMyModel(inherited CreateItem);'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function TMyCollection.Find(const Id: Variant): TMyModel;'+ LineEnding +
'begin'+ LineEnding +
'  Result := TMyModel(inherited Find(Id));'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function TMyCollection.Get(ItemData: TJSONObject): TMyModel;'+ LineEnding +
'begin'+ LineEnding +
'  Result := TMyModel(inherited Get(ItemData));'+ LineEnding +
'end;' +LineEnding +LineEnding;
end;

end.

