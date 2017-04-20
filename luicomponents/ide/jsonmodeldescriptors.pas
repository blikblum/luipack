unit JSONModelDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Forms;

type

  { TJSONModelUnitDescriptor }

  TJSONModelUnitDescriptor = class(TFileDescPascalUnit)
  private
    FCollectionName: String;
    FModelName: String;
    FResourceName: String;
    procedure LoadDefaults;
  public
    constructor Create; override;
    function CreateSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function GetLocalizedDescription: String; override;
    function GetLocalizedName: string; override;
    function GetInterfaceSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function GetInterfaceUsesSection: String; override;
    function GetImplementationSource(const aFilename, aSourceName,
      aResourceName: string): string; override;
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
  published
    property ModelName: String read FModelName write FModelName;
    property CollectionName: String read FCollectionName write FCollectionName;
    property ResourceName: String read FResourceName write FResourceName;
  end;

implementation

uses
  JSONModelUnitOptionsView;

{ TJSONModelUnitDescriptor }

procedure TJSONModelUnitDescriptor.LoadDefaults;
begin
  DefaultFilename := 'jsonmodel.pas';
  DefaultSourceName := 'MyModel';
  FModelName := 'TMyModel';
  FCollectionName := 'TMyCollection';
  FResourceName := 'my';
end;

constructor TJSONModelUnitDescriptor.Create;
begin
  inherited Create;
  Name := 'LuiJSONModel Template';
end;

function TJSONModelUnitDescriptor.CreateSource(const aFilename, aSourceName,
  aResourceName: string): string;
begin
  Result := inherited CreateSource(aFilename, aSourceName, aResourceName);
  Result := StringReplace(Result, '{{modelname}}', ModelName, [rfReplaceAll]);
  Result := StringReplace(Result, '{{collectionname}}', CollectionName, [rfReplaceAll]);
end;

function TJSONModelUnitDescriptor.GetLocalizedDescription: String;
begin
  Result := 'Boilerplate code to use LuiJSONModel';
end;

function TJSONModelUnitDescriptor.GetLocalizedName: string;
begin
  Result := 'LuiJSONModel unit';
end;

function TJSONModelUnitDescriptor.GetInterfaceSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  //todo: add inline to Get, GetItem, Find, Add methods after fpc >= 2.8 is released
  //fpc 2.6.4 has a bug that corrupts memory when using inline in Add
  Result :='type' +LineEnding +
''+ LineEnding +
'  { {{modelname}} }'+ LineEnding +
''+ LineEnding +
'  {{modelname}} = class(TJSONModel)' + LineEnding +
'  protected'+ LineEnding +
'    class function GetResourceName: String; override;'+ LineEnding +
'  end;'+ LineEnding +
''+ LineEnding +
'  { {{collectionname}} }'+ LineEnding +
''+ LineEnding +
'  {{collectionname}} = class(TJSONCollection)'+ LineEnding +
'  private'+ LineEnding +
'    function GetItem(Index: Integer): {{modelname}};'+ LineEnding +
'  protected'+ LineEnding +
'    class function GetItemClass: TJSONModelClass; override;'+ LineEnding +
'  public'+ LineEnding +
'    function CreateItem(AddItem: Boolean = True): {{modelname}};'+ LineEnding +
'    function Find(const Id: Variant): {{modelname}};'+ LineEnding +
'    function Get(ItemData: TJSONObject): {{modelname}};'+ LineEnding +
'    property Items[Index: Integer]: {{modelname}} read GetItem; default;'+ LineEnding +
'  end;'+ LineEnding + LineEnding;
end;

function TJSONModelUnitDescriptor.GetInterfaceUsesSection: String;
begin
  Result := 'Classes, SysUtils, LuiJSONModel, fpjson';
end;

function TJSONModelUnitDescriptor.GetImplementationSource(const aFilename,
  aSourceName, aResourceName: string): string;
begin
  Result :='{ {{modelname}} }'+ LineEnding +
''+ LineEnding +
'class function {{modelname}}.GetResourceName: String;'+ LineEnding +
'begin'+ LineEnding +
'  Result := ''' + ResourceName + ''';'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'{ {{collectionname}} }'+ LineEnding +
''+ LineEnding +
'function {{collectionname}}.GetItem(Index: Integer): {{modelname}};'+ LineEnding +
'begin'+ LineEnding +
'  Result := {{modelname}}(inherited GetItem(Index));'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'class function {{collectionname}}.GetItemClass: TJSONModelClass;'+ LineEnding +
'begin'+ LineEnding +
'  Result := {{modelname}};'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function {{collectionname}}.CreateItem(AddItem: Boolean): {{modelname}};'+ LineEnding +
'begin'+ LineEnding +
'  Result := {{modelname}}(inherited CreateItem(AddItem));'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function {{collectionname}}.Find(const Id: Variant): {{modelname}};'+ LineEnding +
'begin'+ LineEnding +
'  Result := {{modelname}}(inherited Find(Id));'+ LineEnding +
'end;'+ LineEnding +
''+ LineEnding +
'function {{collectionname}}.Get(ItemData: TJSONObject): {{modelname}};'+ LineEnding +
'begin'+ LineEnding +
'  Result := {{modelname}}(inherited Get(ItemData));'+ LineEnding +
'end;' +LineEnding +LineEnding;
end;

function TJSONModelUnitDescriptor.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
begin
  LoadDefaults;
  with TJSONModelUnitOptionsForm.Create(nil) do
  begin
    Descriptor := Self;
    Result := ShowModal;
    Destroy;
  end;
end;

end.

